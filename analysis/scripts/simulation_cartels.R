source(file = "analysis/scripts/set_up_simulation.R")

# record time to measure runtime
sleep_func <- function() { Sys.sleep(5) } 
startTime <- Sys.time()
sleep_func()


# Workflow --------------------------------------------------------------

theta <- 0
struc <- 1
name <- paste(struc, "struc_", theta, "theta", sep = "")
parms <- setParms(theta = theta, struc = struc)
n_all_firms <- sum(parms$n_firms) * n_industries

# array: dim = rows=periods, columns=industries, matrices=parameters
cartels_detected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_undetected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_population <- array(0,dim = c(allperiods, n_industries, nrow(parms)))

# ToDo: add industry name to parms
firms_detected <- data.frame(matrix(ncol = 1000, nrow = 0))
firms_undetected <- data.frame(matrix(ncol = 1000, nrow = 0))
firms_population <- data.frame(matrix(ncol = 1000, nrow = 0))


for (k in 1:nrow(parms)) {  # for all parms
  allcartels_det <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_undet <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_pop <- matrix(0, nrow = allperiods, ncol = n_industries)
  for (i in 1:n_industries) {
    # todo: this seed is not used
    #count <- seed_start + (i-1)*n_industries*10 + (k-1)*100   # different seed for each parm
    count <- seed_start + (i-1)*n_industries*10   # same seed for each parm
    
    sim_list <- simulation_firms(i, parms[k,], k)
    
    firms_det <- get_sample(sim_list$firms, sim_list$detection)
    firms_undet <- get_undetected(sim_list$firms, sim_list$detection)
    firms_pop <- sim_list$firms
# NEXT: append to firms dataframe    

#    firms_sample <- get_sample(firms_in_cartel, detection)
    #firms_undetected <- get_undetected(firms_in_cartel, detection)
    cartels_det <- ifelse(rowSums(firms_det)>0, 1, 0)
    cartels_undet <- ifelse(rowSums(firms_undet)>0, 1, 0)
    cartels_pop <- ifelse(rowSums(firms_pop)>0, 1, 0)
    
    allcartels_det[, i] <- cartels_det
    allcartels_undet[, i] <- cartels_undet
    allcartels_pop[, i] <- cartels_pop
  }
  cartels_detected[,, k] <- allcartels_det
  cartels_undetected[,, k] <- allcartels_undet
  cartels_population[,, k] <- allcartels_pop
  #  cartels_population[,,k] <- allcartels_det + allcartels_undet
}  
sum(cartels_detected) == 967753
sum(cartels_undetected) == 322768
sum(cartels_population) == 1290521

saveRDS(cartels_detected, file = paste("analysis/data/cartels_enforcement_", name, "_detected.rds", sep = ""))
saveRDS(cartels_undetected, file = paste("analysis/data/cartels_enforcement_", name, "_undetected.rds", sep = ""))
saveRDS(cartels_population, file = paste("analysis/data/cartels_enforcement_", name, "_population.rds", sep = ""))

x <- readRDS("analysis/data/cartels_enforcement_0struc_0.5theta_detected.rds")

parms$mean_sum_detected <- round(apply(cartels_detected, MARGIN=3, FUN=get_mean_sum_cartels), 3)
parms$mean_duration_detected <- round(apply(cartels_detected, MARGIN=3, FUN=get_mean_duration), 0)
parms$mean_sum_undetected <- round(apply(cartels_undetected, MARGIN=3, FUN=get_mean_sum_cartels), 3)
parms$mean_duration_undetected <- round(apply(cartels_undetected, MARGIN=3, FUN=get_mean_duration), 0)
#parms$mean_sum_population <- round(apply(cartels_population, MARGIN=3, FUN=get_mean_sum_cartels), 2)
parms$mean_sum_population <- apply(cartels_population, MARGIN=3, FUN=get_mean_sum_cartels)
parms$mean_duration_population <- round(apply(cartels_population, MARGIN=3, FUN=get_mean_duration), 3)

write.table(parms, file = paste("analysis/data/parms_enforcement_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")
k <- kable(parms, "latex")
save_kable(k, file = paste("analysis/data/parms_enforcement_", name, ".tex", sep = ""))


# todo: draw all graphs in a loop
rho <- 0.2
x <- which(parms$rho_start == rho)
c_det <- cartels_detected[,,x]
c_undet <- cartels_undetected[,,x]
c_pop <- cartels_population[,,x]

title <- paste("Simulated cartels: 900 industries with each between 2 and 10 firms")
filename <- paste("analysis/figures/cartels/enforcement_cartels_", name, "_", rho, "rho.png", sep = "")
sim_cartels <- ts(data = cbind(rowSums(c_pop), rowSums(c_det), rowSums(c_undet)))
colnames(sim_cartels) <- c("Population", "Detected", "Undetected")
plot_cartels(title, sim_cartels, filename)



# Calculate cartel durations
cartels_detected_duration <- get_enforcement_duration(cartels_detected, parms) 
cartels_detected_duration$detected <- 1
cartels_undetected_duration <- get_enforcement_duration(cartels_undetected, parms) 
cartels_undetected_duration$detected <- 0
cartels_duration <- rbind(cartels_detected_duration, cartels_undetected_duration)
write.table(cartels_duration, file = paste("analysis/data/duration_enforce_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")


# end recorded time
endTime <- Sys.time()

# prints recorded time
print(endTime - startTime)

