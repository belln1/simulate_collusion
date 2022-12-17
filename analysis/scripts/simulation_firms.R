source(file = "analysis/scripts/set_up_simulation.R")

# record time to measure runtime
sleep_func <- function() { Sys.sleep(5) } 
startTime <- Sys.time()
#sleep_func()


# Workflow --------------------------------------------------------------

theta <- 1
struc <- 1

#theta <- 0
#struc <- 0

name <- paste(struc, "struc_", theta, "theta", sep = "")
parms <- setParms(theta = theta, struc = struc)
n_all_firms <- sum(parms$n_firms) * n_industries

firms_detected <- data.frame(matrix(ncol = 1000, nrow = 0))
firms_undetected <- data.frame(matrix(ncol = 1000, nrow = 0))
firms_population <- data.frame(matrix(ncol = 1000, nrow = 0))
rho_firms <- data.frame(matrix(ncol = 1000, nrow = 0))


#n_k <- 4
#n_i <- 20

for (k in 1:nrow(parms)) {  # for all parms
#for (k in 1:n_k) {  # for all parms
  for (i in 1:n_industries) {
  #for (i in 1:n_i) {
    sim_list <- simulate_firms(i, parms[k,], k)
    
   #  firms_det <- get_sample(sim_list$firms, sim_list$detection)
   #  # convert matrix to data frame with transpose (change rows to columns). rows shall be firms, columns shall be time periods and names
   #  firms_det_df <- data.frame(t(firms_det))
   #  firms_det_df$firm <- seq(1:nrow(firms_det_df))
   #  firms_det_df$industry <- i
   #  firms_det_df[,1003:1007] <- parms[k,]
   #  firms_detected <- bind_rows(firms_detected, firms_det_df)
   # 
   #  firms_undet <- get_undetected(sim_list$firms, sim_list$detection)
   #  firms_undet_df <- data.frame(t(firms_undet))
   #  firms_undet_df$firm <- seq(1:nrow(firms_undet_df))
   #  firms_undet_df$industry <- i
   #  firms_undet_df[,1003:1007] <- parms[k,]
   # firms_undetected <- bind_rows(firms_undetected, firms_undet_df)
   # 
   #  firms_pop <- sim_list$firms
   #  firms_pop_df <- data.frame(t(firms_pop))
   #  firms_pop_df$firm <- seq(1:nrow(firms_pop_df))
   #  firms_pop_df$industry <- i
   #  firms_pop_df[,1003:1007] <- parms[k,]
   #  firms_population <- bind_rows(firms_population, firms_pop_df)
    
    rho <- sim_list$rho_firms
    rho_df <- data.frame(t(rho))
    rho_df$firm <- seq(1:nrow(rho_df))
    rho_df$industry <- i
    rho_df[,1003:1007] <- parms[k,]
    rho_firms <- bind_rows(rho_firms, rho_df)
    
     
  }
}  

# end recorded time
endTime <- Sys.time()

# prints recorded time
print(endTime - startTime) # 21.58 mins - Todo: change bind_rows to [i*k : i*k+100,]

write.table(rho_firms, file = paste("analysis/data/firms/rho_firms_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")
write.table(firms_detected, file = paste("analysis/data/firms/firms_detected_enforcement_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")
write.table(firms_undetected, file = paste("analysis/data/firms/firms_undetected_enforcement_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")
write.table(firms_population, file = paste("analysis/data/firms/firms_population_enforcement_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")


