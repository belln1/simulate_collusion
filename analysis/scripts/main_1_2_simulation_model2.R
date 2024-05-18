rm(list = ls())
source(file = "analysis/scripts/functions_simulation.R")
# MODEL 2 #

# Set basic parameters  --------------------------------------------------------------

allperiods <- 1000
timefactor <- 12
r_1 <- 0.03 #interest rate

n_industries <- 300

sim_seed <- 1673465635 # seed for enforcement 
seeds_dir <- "model2"


# Simulation  --------------------------------------------------------------

# Simulate deltas. set seed different for every industry and every row of parms
n_min <- 2
n_max <- 10 # max number of firms
n_firms_in <- n_min:n_max
rho_in <- seq(0.1, 0.35, 0.05)

parms <- combine_parms(n_firms_in, rho_in)
parms$ICC <- get_ICC_model1(parms$n_firms)
parms$d_nfirms <- sort(1/parms$n_firms, decreasing = FALSE)


if (!file.exists("analysis/data")) {
  dir.create("analysis/data")
}  
if (!file.exists(paste("analysis/data/", seeds_dir, sep = ""))) {
  dir.create(paste("analysis/data/", seeds_dir, sep = ""))
}  
if (!file.exists(paste("analysis/data/", seeds_dir, "/cartels", sep = ""))) {
  dir.create(paste("analysis/data/", seeds_dir, "/cartels", sep = ""))
}  

write.table(parms, file = paste("analysis/data/", seeds_dir, "/parms.csv", sep = ""), row.names = FALSE, sep = ";")

for (k in 1:nrow(parms)) {
  allcartels_det <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_undet <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_pop <- matrix(0, nrow = allperiods, ncol = n_industries)
  
  for (i in 1:n_industries) {
    sim_list <- simulate_firms(i, parms[k,], k, sim_seed, model=2) 
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
  saveRDS(allcartels_det, file = paste("analysis/data/", seeds_dir, "/cartels/cartels_detected_", k, ".rds", sep = ""))
  saveRDS(allcartels_undet, file = paste("analysis/data/", seeds_dir, "/cartels/cartels_undetected_", k, ".rds", sep = ""))
  saveRDS(allcartels_pop, file = paste("analysis/data/", seeds_dir, "/cartels/cartels_population_", k, ".rds", sep = ""))
}

# read all cartel files in and make one big array
# array: dim = rows=periods, columns=industries, matrices=parameters
cartels_detected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_undetected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_population <- array(0,dim = c(allperiods, n_industries, nrow(parms)))

# for every parm combination, read in the matrix with 300 industries
for (k in 1:nrow(parms)) {
  cartels_det <- readRDS(file = paste("analysis/data/", seeds_dir, "/cartels/cartels_detected_", k, ".rds", sep = ""))
  cartels_undet <- readRDS(file = paste("analysis/data/", seeds_dir, "/cartels/cartels_undetected_", k, ".rds", sep = ""))
  cartels_pop <- readRDS(file = paste("analysis/data/", seeds_dir, "/cartels/cartels_population_", k, ".rds", sep = ""))
  
  # save each matrix
  cartels_detected[,, k] <- cartels_det
  cartels_undetected[,, k] <- cartels_undet
  cartels_population[,, k] <- cartels_pop
}
# save big array
saveRDS(cartels_detected, file = paste("analysis/data/", seeds_dir, "/cartels/cartels_detected.rds", sep = ""))
saveRDS(cartels_undetected, file = paste("analysis/data/", seeds_dir, "/cartels/cartels_undetected.rds", sep = ""))
saveRDS(cartels_population, file = paste("analysis/data/", seeds_dir, "/cartels/cartels_population.rds", sep = ""))



######################################################################
# Calculate cartel durations
cartels_duration <- combine_durations(cartels_detected, cartels_undetected, parms, model=1)

# Add industries with 0 cartels for Heckman Selection Correction
data_all <- add_non_collusive_industries(parms, n_industries, cartels_duration)

# Add nonlinear variables for Lasso CV
data <- add_nonlinears_model1(data_all)


###--------------------------------------------------------------------------------------------------------------------------
###--------------------------------------------------------------------------------------------------------------------------
# TESTCASE FOR REFACTORING
# sumstats <- describe(data, fast = FALSE)
# file_m2 <- paste("C:/Users/bell/ZHAW/Research Collusion - General/Code/simulation/simulate_collusion_3_heck/analysis/data/test/sumstats_m2.csv", sep = "")
# m2 <- read.table(file_m2, header = TRUE, sep = ";")
# table(round(sumstats, 2) == round(m2, 2))
