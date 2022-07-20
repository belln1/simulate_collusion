source(file = "analysis/scripts/set_up.R")

# record time to measure runtime
sleep_func <- function() { Sys.sleep(5) } 
startTime <- Sys.time()
sleep_func()



# Plotting Functions --------------------------------------------------------------

plot_sim <- function(title, sim, filename) {
  f <- autoplot(sim) +
#    guides(color="none") +
    guides(color=guide_legend("")) +
    ggtitle(title) +
    xlab("Time") +
    ylab("Discount Factor delta")
#    geom_hline(aes(yintercept=get_ICC_entry(n, rho, gamma), linetype="Entry"), colour="blue") +
#    geom_hline(aes(yintercept=get_ICC_exit(n, rho, gamma, theta), linetype="Exit"), colour="red") +
    scale_linetype_manual(name="ICC", values = c(1, 1), guide = guide_legend(override.aes = list(color = c("blue", "red"))))
  print(f)
  ggsave(filename)
}






# Simulation Functions --------------------------------------------------------------



# Model 2: ICC with fines and leniency (\citet{Bos:Davies:Harrington:Ormosi:2018})
get_ICC_entry <- function(n, rho, gamma){
  return(1-((1-rho)/(n+rho*gamma-rho)))
}
get_ICC_exit <- function(n, rho, gamma, theta){
  return(1-((1-rho)/(n+rho*gamma-theta*rho*gamma-rho)))
}


increase_rho <- function(rho, n_times_caught) {
  return(ifelse(n_times_caught > 0, rho * (1+1/2^n_times_caught), rho))
}

get_sample <- function(firms_in_cartel, detection){
  firms_in_cartel * detection
}

get_undetected <- function(firms_in_cartel, detection){
  firms_in_cartel * (1 - detection)
}


# todo: delete return command
get_detection <- function(periods, rho, seed){
  set.seed(seed)
  x <- matrix(as.numeric(runif(periods) <= 20*rho/allperiods), nrow = periods)
  return(x)
}

# does a firm want to be in a cartel?
get_in_cartel <- function(ind, ICC_entry, ICC_exit) {
  ind_entry <- ifelse(ind > ICC_entry, 1, 0) # ifelse keeps matrix, if_else makes big vector
  ind_exit <- ifelse(ind < ICC_exit, -1, 0)
  v <- ind_entry + ind_exit
  w <- Reduce(function(x,y) ifelse(y==0, x, y), v, accumulate=TRUE) # fill 0-values with last non-0-value (makes big vector)
  in_cartel <- if_else(w == -1, 0, 1) # change -1 to 0 (not in cartel)
  in_cartel <- matrix(as.numeric(in_cartel), ncol = ncol(v))
}

# we have a cartel if enough firms want to be in a cartel. firm_share = 1 means complete cartels. vector with 0 and 1 in all times
check_firm_share <- function(firms, firm_share){
  as.numeric(rowSums(firms) >= ncol(firms)*firm_share)
}

# k is parm row and needed for seed
simulation <- function(i, parms, k) {
  gamma = parms$gamma
  theta = c(rep(1, periodsNoLen), rep(parms$theta_len, periodsLen))
  rho <- matrix(parms$rho_start, nrow = allperiods, ncol = parms$n_firms)
  n_times_caught <- matrix(0, nrow = allperiods, ncol = parms$n_firms)
  ICC_entry <- get_ICC_entry(parms$n_firms, rho, gamma)
  ICC_exit <- get_ICC_exit(parms$n_firms, rho, gamma, theta)
  
  # simulate deltas. set seed different for every industry and every row of parms
  count <- seed_start + (i-1)*n_industries*10 + (k-1)*100
  all_ind_delta <- ind_delta(count, parms$n_firms)
  
  # who wants to be in cartel?
  in_cartel <- get_in_cartel(all_ind_delta, ICC_entry, ICC_exit)
  # who is in cartel? allow for incomplete cartels
  firms_in_cartel <- check_firm_share(in_cartel, 0.8) * in_cartel
  sum(in_cartel)
  sum(firms_in_cartel)
  
  detection <- get_detection(allperiods, rho, seed=count)
  
  #which(rowSums(detection)>0)
  #which(detection * firms_in_cartel > 0, arr.ind=TRUE)
  #which(rowSums((detection * firms_in_cartel))>0)
  
  for (j in 1:allperiods) {
    # if detected & in cartel
    if(sum(firms_in_cartel[j,] * detection[j,])>0){
      # increase number of times caught for actual period
      n_times_caught[j,] <- n_times_caught[j,] + firms_in_cartel[j,] * detection[j,]
      
      # periods in cartel before i get detected (if there are periods before)
      if(j>1){
        v <- detection[1:j,]
        v <- Rev(v, margin=1)
        c <- firms_in_cartel[1:j,]
        c <- Rev(c, margin=1)
        v[cumall(rowSums(c)>0)]=1
        v <- v*c
        detection[1:j,] = Rev(v, margin = 1)
      }
      
      if(j<allperiods){
        # increase number of times caught
        range <- (j+1):allperiods
        n_times_caught[range,] <- n_times_caught[range,] + rep.row(firms_in_cartel[j,] * detection[j,], allperiods-j)
      }
      
      # change rho if there are more than one periods left
      if (parms$structured & (allperiods-j)>1) {
        rho[range,] = increase_rho(rho[range,], n_times_caught[range,])
        
        ICC_entry[range,] <- get_ICC_entry(parms$n_firms, rho[range,], gamma)
        ICC_exit[range,] <- get_ICC_exit(parms$n_firms, rho[range,], gamma, theta[range])
        in_cartel[range,] <- get_in_cartel(all_ind_delta[range,], ICC_entry[range,], ICC_exit[range,])
        firms_in_cartel[range,] <- check_firm_share(in_cartel[range,], 0.8) * in_cartel[range,] # make a function out of this
        count = count + j
        detection[range,] <- get_detection(allperiods-j, rho[range,], seed=count)
      }
      
      # no cartel in next period (if there are next periods)
      if(j<allperiods){
        firms_in_cartel[j+1,] = 0
      }
    }
  }
  
  # plot only if 5 firms..
  x <- n_times_caught[allperiods,]
  y <- which(x==max(x))
  z <- y[1]
  if(z>2){
    sim <- ts(data = cbind(ICC_entry[,z], ICC_exit[,z], all_ind_delta))
    #colnames(sim) <- c("firm 1", "firm 2", "firm 3", "firm 4", "firm 5", "ICC entry", "ICC exit")
    colnames(sim) <- c(paste("firm", 1:ncol(all_ind_delta)),  "ICC entry", "ICC exit")
#    title <- paste("Industry with ", parms$n_firms, " firms and increasing detection probability starting with", parms$rho_start)
    title <- paste("Industry with ", parms$n_firms, " firms")
    filename <- paste("analysis/figures/ICC/enforce_deltas_", name, "_", parms$n_firms, "firms_", parms$rho_start, "rho.png", sep = "")
    plot_sim(title, sim, filename)
  }
  
  sum(firms_in_cartel)
  firms_sample <- get_sample(firms_in_cartel, detection)
  firms_undetected <- get_undetected(firms_in_cartel, detection)
  
  sum(firms_sample)
  cartels_pop <- ifelse(rowSums(firms_in_cartel)>0, 1, 0)
  cartels_undet <- ifelse(rowSums(firms_undetected)>0, 1, 0)
  cartels_det <- ifelse(rowSums(firms_sample)>0, 1, 0)
  return(list(cartels_pop = cartels_pop, cartels_undet = cartels_undet, cartels_det = cartels_det))
}

#c(paste("firm", 1:3), "ICC")

#c("firm 1", "firm 2", "firm 3")

# Workflow --------------------------------------------------------------


setParms <- function(theta, struc){
  n_sim <- 36
  parms <- tibble(
    n_firms = c(rep(2, n_sim/6), rep(3, n_sim/6), rep(4, n_sim/6), rep(5, n_sim/6), rep(6, n_sim/6), rep(7, n_sim/6)),
    rho_start = rep(c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35), n_sim/6),
    theta_len = rep(c(theta), n_sim),
    structured = rep(c(struc), n_sim),
    gamma = rep(0.9, n_sim)
  )
  parms <- parms %>%
    arrange(n_firms, rho_start, theta_len, structured)
}

theta <- 0
struc <- 1
name <- paste(struc, "struc_", theta, "theta", sep = "")
parms <- setParms(theta = theta, struc = struc)


# array: dim = rows, columns, matrices
cartels_detected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_undetected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_population <- array(0,dim = c(allperiods, n_industries, nrow(parms)))

for (k in 1:nrow(parms)) {  # for all parms
  allcartels_det <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_undet <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_pop <- matrix(0, nrow = allperiods, ncol = n_industries)
  for (i in 1:n_industries) {
    # todo: this seed is not used
    #count <- seed_start + (i-1)*n_industries*10 + (k-1)*100   # different seed for each parm
    count <- seed_start + (i-1)*n_industries*10   # same seed for each parm
    
    sim_list <- simulation(i, parms[k,], k)
    allcartels_undet[, i] <- sim_list$cartels_undet
    allcartels_det[, i] <- sim_list$cartels_det
    allcartels_pop[, i] <- sim_list$cartels_pop
  }
  cartels_detected[,, k] <- allcartels_det
  cartels_undetected[,, k] <- allcartels_undet
  cartels_population[,, k] <- allcartels_pop
  #  cartels_population[,,k] <- allcartels_det + allcartels_undet
}  

  
saveRDS(cartels_detected, file = paste("analysis/data/cartels_enforcement_", name, "_detected.rds", sep = ""))
saveRDS(cartels_undetected, file = paste("analysis/data/cartels_enforcement_", name, "_undetected.rds", sep = ""))
saveRDS(cartels_population, file = paste("analysis/data/cartels_enforcement_", name, "_population.rds", sep = ""))

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

get_enforcement_duration <- function(cartels, parms) {
  cd <- apply(cartels, MARGIN=3, FUN=get_cartel_duration)
  for (i in 1:nrow(parms)) {
    cd[[i]]$n_firms <- parms$n_firms[i]
    cd[[i]]$rho_start <- parms$rho_start[i]
    cd[[i]]$theta_len <- parms$theta_len[i]
    cd[[i]]$structured <- parms$structured[i]
  }
  cartels_duration <- do.call(rbind, cd)
}

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

