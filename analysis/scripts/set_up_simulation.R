source(file = "analysis/scripts/set_up.R")

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
# k is parm row and needed for seed
simulate_firms <- function(i, parms, k) {
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
  #  sum(in_cartel)
  #  sum(firms_in_cartel)
  
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
  # x <- n_times_caught[allperiods,]
  # y <- which(x==max(x))
  # z <- y[1]
  # if(z>2){
  #   sim <- ts(data = cbind(ICC_entry[,z], ICC_exit[,z], all_ind_delta))
  #   colnames(sim) <- c(paste("firm", 1:ncol(all_ind_delta)),  "ICC exit", "ICC entry")
  #   title <- paste("Industry with ", parms$n_firms, " firms")
  #   filename <- paste("analysis/figures/ICC/enforce_deltas_", name, "_", parms$n_firms, "firms_", parms$rho_start, "rho.png", sep = "")
  #   plot_sim(title, sim, filename)
  #}
  
  # return only firms_in_cartel, detection, ICC_entry, ICC_exit.
  
  # sum(firms_in_cartel)
  
  return(list(firms = firms_in_cartel, detection = detection, ICC_entry = ICC_entry, ICC_exit = ICC_exit, rho_firms = rho))

  # firms_sample <- get_sample(firms_in_cartel, detection)
  # firms_undetected <- get_undetected(firms_in_cartel, detection)
  # 
  # sum(firms_sample)
  # cartels_pop <- ifelse(rowSums(firms_in_cartel)>0, 1, 0)
  # cartels_undet <- ifelse(rowSums(firms_undetected)>0, 1, 0)
  # cartels_det <- ifelse(rowSums(firms_sample)>0, 1, 0)
  #return(list(cartels_pop = cartels_pop, cartels_undet = cartels_undet, cartels_det = cartels_det, ICC_entry = ICC_entry, ICC_exit = ICC_exit))
}



# Todo: function that receives all different single parameter values and then makes a parameter table out of it
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

