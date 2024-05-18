rm(list = ls())
source(file = "analysis/scripts/functions_simulation.R")

# refactor:
# add model 1
# add model 2
# test with orig parms
# back to original parms
# delete commented code
# delete all prints
# delete unnecessary read and write
# delete unnecessary packages


# Set basic parameters  --------------------------------------------------------------

# Set seed for reproducibility 
sim_seed <- 1673465635
directory <- "model3"

#allperiods <- 1000
allperiods <- 10
periodsNoLen <- 0 # thetas remain constant for all time periods
periodsLen <- allperiods
r_1 <- 0.03 #interest rate

#n_industries <- 300
n_industries <- 2


# n_firms_in <- 2:10
# rho_in <- seq(0.1, 0.35, 0.05)
# gamma_in <- c(0.7, 0.8, 0.9)
# theta_in <- c(0, 0.5, 1)
# struc_in <- c(0, 1)

n_firms_in <- 2:5
rho_in <- seq(0.1, 0.3, 0.1)
gamma_in <- c(0.7, 0.8)
theta_in <- c(0, 1)
struc_in <- c(0, 1)


if (!file.exists("analysis/data")) {
  dir.create("analysis/data")
}  
if (!file.exists(paste("analysis/data/", directory, sep = ""))) {
  dir.create(paste("analysis/data/", directory, sep = ""))
}  
if (!file.exists(paste("analysis/data/", directory, "/cartels", sep = ""))) {
  dir.create(paste("analysis/data/", directory, "/cartels", sep = ""))
}  

parms <- combine_parms_model3(n_firms_in, rho_in, gamma_in, theta_in, struc_in)

# Saved parameters are needed for plots
write.table(parms, file = paste("analysis/data/", directory, "/parms.csv", sep = ""), row.names = FALSE, sep = ";")

for (k in 1:nrow(parms)) {
  allcartels_det <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_undet <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_pop <- matrix(0, nrow = allperiods, ncol = n_industries)
  
  for (i in 1:n_industries) {
    sim_list <- simulate_firms_model3(i, parms[k,], k, sim_seed) 
    
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

# Read all cartel files in and make one large array
# array: dim = rows=periods, columns=industries, matrices=parameters
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
# Save large array (needed for plots of all cartels)
saveRDS(cartels_detected, file = paste("analysis/data/", directory, "/cartels/cartels_detected.rds", sep = ""))
saveRDS(cartels_undetected, file = paste("analysis/data/", directory, "/cartels/cartels_undetected.rds", sep = ""))
saveRDS(cartels_population, file = paste("analysis/data/", directory, "/cartels/cartels_population.rds", sep = ""))


######################################################################
# Calculate cartel durations
cartels_duration <- combine_durations(cartels_detected, cartels_undetected, parms, model=3)

# Add industries with 0 cartels for Heckman Selection Correction
data_all <- add_non_collusive_industries(parms, n_industries, cartels_duration)

# Add nonlinear variables for Lasso CV
data <- add_nonlinears_model3(data_all)


###--------------------------------------------------------------------------------------------------------------------------
###--------------------------------------------------------------------------------------------------------------------------
# TESTCASE FOR REFACTORING
sumstats <- describe(data, fast = FALSE)
sumstats
file_m3 <- paste("C:/Users/bell/ZHAW/Research Collusion - General/Code/simulation/simulate_collusion_1_2_constant_theta/analysis/data/test/sumstats_m3.csv", sep = "")
m3 <- read.table(file_m3, header = TRUE, sep = ";")
table(round(sumstats, 2) == round(m3, 2)) #364

