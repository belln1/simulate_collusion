source(file = "analysis/scripts/functions_simulation.R")

# Set basic parameters  --------------------------------------------------------------

# EC cartels start at 1964
allperiods <- 1000
periodsNoLen <- allperiods/2
periodsLen <- allperiods/2
timefactor <- 12
r_1 <- 0.03 #interest rate

n_industries <- 2

n_firms_in <- 2:10
rho_in <- seq(0.1, 0.35, 0.05)
gamma_in <- c(0.7, 0.8, 0.9)
theta_in <- c(0, 0.5, 1)
struc_in <- c(0, 1)


combine_parms <- function(n_firms_in, rho_in, gamma_in, theta_in, struc_in){
  n_sim <- length(n_firms_in) * length(rho_in) * length(gamma_in) * length(theta_in) * length(struc_in)
  parms <- tibble(
    n_firms = rep(n_firms_in, each=n_sim/length(n_firms_in)),
    rho_start = rep(rho_in, times=length(n_firms_in), each=n_sim/(length(n_firms_in)*length(rho_in))),
    theta_len = rep(theta_in, times=length(n_firms_in)*length(rho_in), each=n_sim/(length(n_firms_in)*length(rho_in)*length(theta_in))),
    gamma = rep(gamma_in, times=length(n_firms_in)*length(rho_in)*length(theta_in), each=n_sim/(length(n_firms_in)*length(rho_in)*length(theta_in)*length(gamma_in))),
    structured = rep(struc_in, times=length(n_firms_in)*length(rho_in)*length(theta_in)*length(gamma_in), each=n_sim/(length(n_firms_in)*length(rho_in)*length(theta_in)*length(gamma_in)*length(struc_in)))
  )
}
parms <- combine_parms(n_firms_in, rho_in, gamma_in, theta_in, struc_in)
write.table(parms, file = paste("analysis/data/seeds_2/parms.csv", sep = ""), row.names = FALSE, sep = ";")

# generate random seed from time
op <- options(digits.secs = 6)
sim_seed <- as.numeric(Sys.time())
set.seed(sim_seed)
sim_seed

sim_seed <- 1673465635




for (k in 1:nrow(parms)) {
  print(Sys.time())
  
  # function: simulate_parms(parms, seed) {
  firms_detected <- data.frame(matrix(ncol = 1000, nrow = 0))
  firms_undetected <- data.frame(matrix(ncol = 1000, nrow = 0))
  firms_population <- data.frame(matrix(ncol = 1000, nrow = 0))
  rho_firms <- data.frame(matrix(ncol = 1000, nrow = 0))
  
  # array: dim = rows=periods, columns=industries, matrices=parameters
 # cartels_detected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
#  cartels_undetected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
#  cartels_population <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
  
  
  #for (k in 1:nrow(parms)) {  # for all parms
    allcartels_det <- matrix(0, nrow = allperiods, ncol = n_industries)
    allcartels_undet <- matrix(0, nrow = allperiods, ncol = n_industries)
    allcartels_pop <- matrix(0, nrow = allperiods, ncol = n_industries)
    
    for (i in 1:n_industries) {
      sim_list <- simulate_firms(i, parms[k,], k, sim_seed) 
      
      
      firms_det <- get_sample(sim_list$firms, sim_list$detection)
      # convert matrix to data frame with transpose (change rows to columns). rows shall be firms, columns shall be time periods and names
      firms_det_df <- data.frame(t(firms_det))
      firms_det_df$firm <- seq(1:nrow(firms_det_df))
      firms_det_df$industry <- i
      firms_det_df[,1003:1007] <- parms[k,]
      firms_detected <- bind_rows(firms_detected, firms_det_df)
      
      firms_undet <- get_undetected(sim_list$firms, sim_list$detection)
      firms_undet_df <- data.frame(t(firms_undet))
      firms_undet_df$firm <- seq(1:nrow(firms_undet_df))
      firms_undet_df$industry <- i
      firms_undet_df[,1003:1007] <- parms[k,]
      firms_undetected <- bind_rows(firms_undetected, firms_undet_df)
      
      firms_pop <- sim_list$firms
      firms_pop_df <- data.frame(t(firms_pop))
      firms_pop_df$firm <- seq(1:nrow(firms_pop_df))
      firms_pop_df$industry <- i
      firms_pop_df[,1003:1007] <- parms[k,]
      firms_population <- bind_rows(firms_population, firms_pop_df)
      
      rho <- sim_list$rho_firms
      rho_df <- data.frame(t(rho))
      rho_df$firm <- seq(1:nrow(rho_df))
      rho_df$industry <- i
      rho_df[,1003:1007] <- parms[k,]
      rho_firms <- bind_rows(rho_firms, rho_df)
      
      cartels_det <- ifelse(rowSums(firms_det)>0, 1, 0)
      cartels_undet <- ifelse(rowSums(firms_undet)>0, 1, 0)
      cartels_pop <- ifelse(rowSums(firms_pop)>0, 1, 0)
      
      allcartels_det[, i] <- cartels_det
      allcartels_undet[, i] <- cartels_undet
      allcartels_pop[, i] <- cartels_pop
      
    }
    
    

  #saveRDS(cartels_detected, file = paste("analysis/data/seeds/cartels/cartels_detected_", sim_seed, name, ".rds", sep = ""))
    
  saveRDS(allcartels_det, file = paste("analysis/data/seeds_2/cartels/cartels_detected_", k, ".rds", sep = ""))
  saveRDS(allcartels_undet, file = paste("analysis/data/seeds_2/cartels/cartels_undetected_", k, ".rds", sep = ""))
  saveRDS(allcartels_pop, file = paste("analysis/data/seeds_2/cartels/cartels_population_", k, ".rds", sep = ""))
  
  write.table(rho_firms, file = paste("analysis/data/seeds_2/firms/rho_firms/rho_firms_", k, ".csv", sep = ""), row.names = FALSE, sep = ";")
  write.table(firms_detected, file = paste("analysis/data/seeds_2/firms/firms_detected/firms_detected_enforcement_", k, ".csv", sep = ""), row.names = FALSE, sep = ";")
  write.table(firms_undetected, file = paste("analysis/data/seeds_2/firms/firms_undetected/firms_undetected_enforcement_", k, ".csv", sep = ""), row.names = FALSE, sep = ";")
  write.table(firms_population, file = paste("analysis/data/seeds_2/firms/firms_population/firms_population_enforcement_", k, ".csv", sep = ""), row.names = FALSE, sep = ";")
}


# read all cartel files in and make one big array
# array: dim = rows=periods, columns=industries, matrices=parameters
cartels_detected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_undetected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_population <- array(0,dim = c(allperiods, n_industries, nrow(parms)))

# for every parm combination, read in the matrix with 300 industries
for (k in 1:nrow(parms)) {
  print(Sys.time())
  cartels_det <- readRDS(file = paste("analysis/data/seeds_2/cartels/cartels_detected_", k, ".rds", sep = ""))
  cartels_undet <- readRDS(file = paste("analysis/data/seeds_2/cartels/cartels_undetected_", k, ".rds", sep = ""))
  cartels_pop <- readRDS(file = paste("analysis/data/seeds_2/cartels/cartels_population_", k, ".rds", sep = ""))
  
# save each matrix
  cartels_detected[,, k] <- cartels_det
  cartels_undetected[,, k] <- cartels_undet
  cartels_population[,, k] <- cartels_pop
}
# save big array
saveRDS(cartels_detected, file = paste("analysis/data/seeds_2/cartels/cartels_detected.rds", sep = ""))
saveRDS(cartels_undetected, file = paste("analysis/data/seeds_2/cartels/cartels_undetected.rds", sep = ""))
saveRDS(cartels_population, file = paste("analysis/data/seeds_2/cartels/cartels_population.rds", sep = ""))
