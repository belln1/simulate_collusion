rm(list = ls())
source(file = "analysis/scripts/functions_simulation.R")
# MODEL 1 #


# Set basic parameters  --------------------------------------------------------------

# Set seed for reproducibility  
sim_seed <- 123 
directory <- "model1_seed123" 

allperiods <- 1000
r_1 <- 0.03 #interest rate
#min_share <- 0.8 # minimum percentage of firms needed to form a cartel - former version with incomplete cartels
min_share <- 1 # minimum percentage of firms needed to form a cartel - revised version with complete cartels

n_industries <- 300

n_max <- 10 # max number of firms
n_firms_in <- 2:n_max
sigma_all <- seq(0.1, 0.35, 0.05)
sigma_t <- 1 - (1-sigma_all)^(1/200) # 200 is an approximation of mean duration, in simulation run with sigma=0


# ------------------------------------------------------------------------------------

# Create data folder

if (!file.exists("analysis/data")) {
  dir.create("analysis/data")
}  
if (!file.exists(paste("analysis/data/", directory, sep = ""))) {
  dir.create(paste("analysis/data/", directory, sep = ""))
}  
if (!file.exists(paste("analysis/data/", directory, "/cartels", sep = ""))) {
  dir.create(paste("analysis/data/", directory, "/cartels", sep = ""))
}  

# Build dataframe with all possible parameter combinations from above
parms <- combine_parms(n_firms_in, sigma_t)
write.table(parms, file = paste("analysis/data/", directory, "/parms.csv", sep = ""), row.names = FALSE, sep = ";")


for (k in 1:nrow(parms)) {
  firms_detected <- data.frame(matrix(ncol = 1000, nrow = 0))
  firms_undetected <- data.frame(matrix(ncol = 1000, nrow = 0))
  firms_population <- data.frame(matrix(ncol = 1000, nrow = 0))

  # Array: dim = rows=periods, columns=industries, matrices=parameters
  allcartels_det <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_undet <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_pop <- matrix(0, nrow = allperiods, ncol = n_industries)
  
  for (i in 1:n_industries) {
    sim_list <- simulate_firms(i, parms[k,], k, sim_seed, model=1, min_share) 
    
    firms_det <- get_sample(sim_list$firms, sim_list$detection)
    firms_undet <- get_undetected(sim_list$firms, sim_list$detection)
    firms_pop <- sim_list$firms

    cartels_det <- ifelse(rowSums(firms_det)>0, 1, 0)
    cartels_undet <- ifelse(rowSums(firms_undet)>0, 1, 0)
    cartels_pop <- ifelse(rowSums(firms_pop)>0, 1, 0)
    
    allcartels_det[, i] <- cartels_det
    allcartels_undet[, i] <- cartels_undet
    allcartels_pop[, i] <- cartels_pop
  }
  saveRDS(allcartels_det, file = paste("analysis/data/", directory, "/cartels/cartels_detected_", k, ".rds", sep = ""))
  saveRDS(allcartels_undet, file = paste("analysis/data/", directory, "/cartels/cartels_undetected_", k, ".rds", sep = ""))
  saveRDS(allcartels_pop, file = paste("analysis/data/", directory, "/cartels/cartels_population_", k, ".rds", sep = ""))
}

# Read all cartel files into one large array
cartels_detected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_undetected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_population <- array(0,dim = c(allperiods, n_industries, nrow(parms)))

# For every parm combination, read in the matrix with 300 industries
for (k in 1:nrow(parms)) {
  cartels_det <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_detected_", k, ".rds", sep = ""))
  cartels_undet <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_undetected_", k, ".rds", sep = ""))
  cartels_pop <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_population_", k, ".rds", sep = ""))
  
  # Save each matrix
  cartels_detected[,, k] <- cartels_det
  cartels_undetected[,, k] <- cartels_undet
  cartels_population[,, k] <- cartels_pop
}
# Save large array
saveRDS(cartels_detected, file = paste("analysis/data/", directory, "/cartels/cartels_detected.rds", sep = ""))
saveRDS(cartels_undetected, file = paste("analysis/data/", directory, "/cartels/cartels_undetected.rds", sep = ""))
saveRDS(cartels_population, file = paste("analysis/data/", directory, "/cartels/cartels_population.rds", sep = ""))


######################################################################
# Calculate cartel durations
#directory = "model1"
parms <- read.table(file = paste("analysis/data/", directory, "/parms.csv", sep = ""), header = TRUE, sep = ";")
cartels_undetected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_undetected.rds", sep = ""))
cartels_detected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_detected.rds", sep = ""))

cartels_duration <- combine_durations(cartels_detected, cartels_undetected, parms, model=1)

# Add industries with 0 cartels for Heckman Selection Correction
data_all <- add_non_collusive_industries(parms, n_industries, cartels_duration)

# Add nonlinear variables for Lasso CV
data <- add_nonlinears_model1(cartels_duration)

write.table(data, file = paste("analysis/data/", directory, "/cartels_duration.csv", sep = ""), row.names = FALSE, sep = ";")
