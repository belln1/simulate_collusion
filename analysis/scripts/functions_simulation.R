library(psych) # describe()
library(dplyr)
library(purrr)
library(tidyr)
library(DescTools) # Rev() reverse order of rows and columns in a matrix
library(gtools) # cut quantiles
library(ggplot2)
library(ggfortify) # time series plots
library(forecast)
library(patchwork) # combine multiple plots with +
library(kableExtra)
library(abind)
library(stargazer)


# General Functions --------------------------------------------------------------

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# Simulation Functions  --------------------------------------------------------------

combine_parms <- function(n_firms_in, sigma_in){
  n_sim <- length(n_firms_in) * length(sigma_in)
  parms <- tibble(
    n_firms = rep(n_firms_in, each=n_sim/length(n_firms_in)),
    sigma_start = rep(sigma_in, times=length(n_firms_in), each=n_sim/(length(n_firms_in)*length(sigma_in))),
  )
}

combine_parms_model3 <- function(n_firms_in, sigma_in, gamma_in, theta_in, struc_in){
  n_sim <- length(n_firms_in) * length(sigma_in) * length(gamma_in) * length(theta_in) * length(struc_in)
  parms <- tibble(
    n_firms = rep(n_firms_in, each=n_sim/length(n_firms_in)),
    sigma_start = rep(sigma_in, times=length(n_firms_in), each=n_sim/(length(n_firms_in)*length(sigma_in))),
    theta = rep(theta_in, times=length(n_firms_in)*length(sigma_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in))),
    gamma = rep(gamma_in, times=length(n_firms_in)*length(sigma_in)*length(theta_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in))),
    structured = rep(struc_in, times=length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in)*length(struc_in)))
  )
}

get_delta <- function(r){1/(1+r)}
anyfunction <- function(x){atan(x*2)/(pi) + 0.5}

# Randomly return a number of +/- 0.01
random_steps <- function(n, seed){
  set.seed(seed)
  x <- rbinom(n, 1, 0.5)
  (2*x - 1)/100
}

# Discount factor delta based on random walk for interest r
get_deltas_r <- function(start, periods, seed) {
  steps <- random_steps(periods-1, seed)
  walk_r <- cumsum(c(start, steps))
  deltas <- anyfunction(get_delta(walk_r))
} 

# 1 industry with n firms, each with random walk deltas (same seed for every industry)
ind_delta <- function(count, n_firms){
  replicate(n_firms, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)})
}

# Model 1 and 2: ICC depending on number of firms (cite Stigler 1964)
get_ICC_model1 <- function(n) {
  1-1/n
}

# Model 3: ICC with fines and leniency (Bos/Davies/Harrington/Ormosi(2018)
# get_ICC_entry_model3 <- function(n, sigma, gamma){
#   return(1-((1-sigma)/(n+sigma*gamma-sigma)))
# }
# revised version: same entry und exit ICC
get_sigma_all_t <- function(sigma_t){
  1 - (1-sigma_t)^200
}

get_ICC_entry_model3 <- function(n, sigma_t, gamma, theta){
  sigma <- get_sigma_all_t(sigma_t)
  1-((1-sigma)/(n+sigma*gamma-theta*sigma*gamma-sigma))
}
get_ICC_exit_model3 <- function(n, sigma_t, gamma, theta){
  sigma <- get_sigma_all_t(sigma_t)
  1-((1-sigma)/(n+sigma*gamma-theta*sigma*gamma-sigma))
}

increase_sigma <- function(sigma, n_times_caught) {
  return(ifelse(n_times_caught > 0, sigma * (1+1/2^n_times_caught), sigma))
}

get_sample <- function(firms_in_cartel, detection){
  firms_in_cartel * detection
}

get_undetected <- function(firms_in_cartel, detection){
  firms_in_cartel * (1 - detection)
}

get_detection <- function(periods, sigma, seed){
  set.seed(seed)
#  x <- matrix(as.numeric(runif(periods) <= 20*sigma/allperiods), nrow = periods)
  x <- matrix(as.numeric(runif(periods) <= sigma), nrow = periods)
}

# sigma = Prob(at least once found during cartel duration) = 1 - ((1-p_t)^duration)
# p_t = Prob(found in time t) = 1 - ((1-sigma)^(1/duration)) # formular for sigma, solved for p_t


# In Model 2, detection depends on n_firms and sigma
get_detection_model2 <- function(periods, sigma, d_nfirms, seed){
  set.seed(seed)
#  x <- matrix(as.numeric(runif(periods) <= 20*sigma*parms$d_nfirms/allperiods), nrow = periods) # former version
  x <- matrix(as.numeric(runif(periods) <= sigma*parms$d_nfirms), nrow = periods) # revised version, more intuitive formular
}


# Does a firm want to be in a cartel?
# Reduce(function, vector) applies function on the first two elements of vector, then on the result of that and the third element
# accumulate=TRUE returns all results, accumulate=FALSE returns only last result
get_in_cartel <- function(ind, ICC_entry, ICC_exit) {
  ind_entry <- ifelse(ind > ICC_entry, 1, 0) # ifelse keeps matrix, if_else makes large vector
  ind_exit <- ifelse(ind < ICC_exit, -1, 0)
  v <- ind_entry + ind_exit
  w <- Reduce(function(x,y) ifelse(y==0, x, y), v, accumulate=TRUE) # fill 0-values with last non-0-value (makes large vector)
  in_cartel <- if_else(w == -1, 0, 1) # change -1 to 0 (not in cartel)
  in_cartel <- matrix(as.numeric(in_cartel), ncol = ncol(v))
}

# We have a cartel if enough firms want to be in a cartel. firm_share = 1 means complete cartels. vector with 0 and 1 in all times
check_firm_share <- function(firms, firm_share){
  as.numeric(rowSums(firms) >= ncol(firms)*firm_share)
}



# MODEL 1 and 2: k is parm row and needed for seed
simulate_firms <- function(i, parms, k, seed_start, model, min_share) {
  sigma <- matrix(parms$sigma_start, nrow = allperiods, ncol = parms$n_firms)
  n_times_caught <- matrix(0, nrow = allperiods, ncol = parms$n_firms)
  ICC_entry <- get_ICC_model1(parms$n_firms)
  ICC_exit <- ICC_entry

  # Simulate deltas. set seed different for every industry and every row of parms
  count <- k*1000000+i*1000 + seed_start
  all_ind_delta <- ind_delta(count, parms$n_firms)
  
  # Who wants to be in cartel?
  in_cartel <- get_in_cartel(all_ind_delta, ICC_entry, ICC_exit)
  # Who is in cartel? Allow for incomplete cartels
  firms_in_cartel <- check_firm_share(in_cartel, min_share) * in_cartel
  
  if (model==2){
    detection <- get_detection_model2(allperiods, sigma, seed=count)
  }
  else {
    detection <- get_detection(allperiods, sigma, seed=count)
  }
  
  for (j in 1:allperiods) 
    
  {
    # If detected & in cartel
    if(sum(firms_in_cartel[j,] * detection[j,])>0){
      # Increase number of times caught for actual period
      n_times_caught[j,] <- n_times_caught[j,] + firms_in_cartel[j,] * detection[j,]
      
      # Periods in cartel before i get detected (if there are periods before)
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
        # Increase number of times caught
        range <- (j+1):allperiods
        n_times_caught[range,] <- n_times_caught[range,] + rep.row(firms_in_cartel[j,] * detection[j,], allperiods-j)
      }
      
      # No cartel in next period (if there are next periods)
      if(j<allperiods){
        firms_in_cartel[j+1,] = 0
      }
    }
  }
  return(list(firms = firms_in_cartel, detection = detection, ICC_entry = ICC_entry, ICC_exit = ICC_exit, sigma_firms = sigma))
}


# MODEL 3: k is parm row and needed for seed
simulate_firms_model3 <- function(i, parms, k, seed_start, min_share) {
  gamma = parms$gamma
  theta = c(rep(1, periodsNoLen), rep(parms$theta, periodsLen))
  sigma <- matrix(parms$sigma_start, nrow = allperiods, ncol = parms$n_firms)
  n_times_caught <- matrix(0, nrow = allperiods, ncol = parms$n_firms)
  ICC_entry <- get_ICC_entry_model3(parms$n_firms, sigma, gamma, theta)
  ICC_exit <- get_ICC_exit_model3(parms$n_firms, sigma, gamma, theta)
  
  # Simulate deltas. set seed different for every industry and every row of parms
  count <- k*1000000+i*1000 + seed_start
  all_ind_delta <- ind_delta(count, parms$n_firms)
  
  # Who wants to be in cartel?
  in_cartel <- get_in_cartel(all_ind_delta, ICC_entry, ICC_exit)
  # Who is in cartel? allow for incomplete cartels
  firms_in_cartel <- check_firm_share(in_cartel, min_share) * in_cartel
  
  detection <- get_detection(allperiods, sigma, seed=count)
  
  for (j in 1:allperiods) 
    
  {
    # If detected & in cartel
    if(sum(firms_in_cartel[j,] * detection[j,])>0){
      # Increase number of times caught for actual period
      n_times_caught[j,] <- n_times_caught[j,] + firms_in_cartel[j,] * detection[j,]
      
      # Periods in cartel before i get detected (if there are periods before)
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
        # Increase number of times caught
        range <- (j+1):allperiods
        n_times_caught[range,] <- n_times_caught[range,] + rep.row(firms_in_cartel[j,] * detection[j,], allperiods-j)
      }
      
      # Change sigma if there are more than one periods left, only for detected firms in cartel
      if (parms$structured & (allperiods-j)>1) {
        # Row-wise matrix multiplication with vector
        change <- sweep(n_times_caught[range,], MARGIN = 2, (firms_in_cartel[j,] * detection[j,]), `*`)
        sigma[range,] = increase_sigma(sigma[range,], change)
        ICC_entry[range,] <- get_ICC_entry_model3(parms$n_firms, sigma[range,], gamma, theta[range])
        ICC_exit[range,] <- get_ICC_exit_model3(parms$n_firms, sigma[range,], gamma, theta[range])
        in_cartel[range,] <- get_in_cartel(all_ind_delta[range,], ICC_entry[range,], ICC_exit[range,])
        firms_in_cartel[range,] <- check_firm_share(in_cartel[range,], min_share) * in_cartel[range,] # make a function out of this
        count = count + j
        detection[range,] <- get_detection(allperiods-j, sigma[range,], seed=count)
      }
      
      # No cartel in next period (if there are next periods)
      if(j<allperiods){
        firms_in_cartel[j+1,] = 0
      }
    }
  }
  return(list(firms = firms_in_cartel, detection = detection, ICC_entry = ICC_entry, ICC_exit = ICC_exit, sigma_firms = sigma))
}



# Functions to calculate cartel durations  --------------------------------------------------------------

get_starttimes <- function(cartels) {
  d <- lag(cartels)
  d[is.na(d)] = 0
  e <- as.numeric((cartels - d) == 1)
  starttimes <- matrix(e, ncol = ncol(cartels))
}

get_endtimes <- function(cartels) {
  d <- lead(cartels)
  d[is.na(d)] = 0
  e <- as.numeric((cartels - d) == 1)
  endtimes <- matrix(e, ncol = ncol(cartels))
}

get_cartel_duration <- function(cartels) {
  starttimes <- get_starttimes(cartels)
  endtimes <- get_endtimes(cartels)
  start <- as_tibble(which(starttimes==1, arr.ind = TRUE))
  start <- dplyr::rename(start, "start" = row)
  end <- as_tibble(which(endtimes==1, arr.ind = TRUE))
  end <- dplyr::rename(end, "end" = row)
  df <- cbind(start, end=end$end)
  df$duration <- df$end - df$start + 1
  # Logarithm of duration: log(n+1)
  df$lduration <- log(df$duration+1)
  df <- df %>%
    dplyr::rename(industry = col) %>%
    group_by(industry) %>%
    arrange(industry, start) %>%  # order rows
    relocate(industry, start, end, duration)  # order columns
  return(df)
}

get_enforcement_duration <- function(cartels, parms, model) {
  cd <- apply(cartels, MARGIN=3, FUN=get_cartel_duration)
  for (i in 1:nrow(parms)) {
    cd[[i]]$n_firms <- parms$n_firms[i]
    cd[[i]]$sigma_start <- parms$sigma_start[i]
    cd[[i]]$sigma_all_t <- get_sigma_all_t(parms$sigma_start[i])
    cd[[i]]$parm_id <- i
    if (model==3){
      cd[[i]]$theta <- parms$theta[i]
      cd[[i]]$gamma <- parms$gamma[i]
      cd[[i]]$structured <- parms$structured[i]
    }
  }
  cartels_duration <- do.call(rbind, cd)
}

combine_durations <- function(cartels_detected, cartels_undetected, parms, model){
  cartels_detected_duration <- get_enforcement_duration(cartels_detected, parms, model) 
  df <- cartels_detected_duration %>%
    dplyr::mutate(detected = 1,
           nTc = 1) %>%
    group_by(industry, parm_id) %>%
    arrange(start) %>%
    dplyr::mutate(nTc = cumsum(nTc)) %>%
    relocate(industry, parm_id, detected, nTc) %>%
    arrange(parm_id, industry, nTc)
  cartels_detected_duration <- df
  
  cartels_undetected_duration <- get_enforcement_duration(cartels_undetected, parms, model) 
  df <- cartels_undetected_duration %>%
    dplyr::mutate(detected = 0,
           nTc = 0) %>%
    relocate(industry, parm_id, detected, nTc) %>%
    arrange(parm_id, industry, nTc)
  cartels_undetected_duration <- df
  
  cartels_duration <- rbind(cartels_detected_duration, cartels_undetected_duration)
  # Cartel id is unique in industry
  df <- cartels_duration %>%
    mutate(cartel = 1,
           in_cartel = 1) %>%
    group_by(parm_id, industry) %>%
    arrange(start) %>%
    dplyr::mutate(cartel = cumsum(cartel)) %>%
    arrange(parm_id, industry, start) %>%
    dplyr::mutate(nTc = ifelse((detected == 0 & cartel > 1), lag(nTc), nTc),
           rep_off = ifelse(nTc > 1, 1, 0)) %>%
    arrange(industry, parm_id, start) %>%
    relocate(parm_id, industry, cartel, detected, nTc, rep_off, start)
  cartels_duration <- df
}



# Functions to extend dataset for estimations --------------------------------------------------------------

# Add industries without cartels, used for Heckman Sample Selection Correction
add_non_collusive_industries <- function(parms, n_industries, cartels_duration) {
  parms$parm_id <- 1:nrow(parms)
  data_grid <- expand.grid(industry = 1:n_industries, parm_id = parms$parm_id)
  data_grid <- data_grid[order(data_grid$industry, data_grid$parm_id),]
  
  # Join unique parms with basic grid 
  data_parms <- full_join(parms, data_grid, by="parm_id")
  
  # Join parms_grid with all cartels
  data_all <- full_join(cartels_duration, data_parms)  #by all parms
  
  # Add unique industry ID
  df <- data_all %>%
    replace(is.na(data_all), 0) %>%
    relocate(industry, parm_id, cartel) %>%
    group_by(parm_id, industry) %>%
    dplyr::mutate(industry_id = cur_group_id()) %>%
    arrange(parm_id, industry, start)
  data_all <- df
  data_all$sigma_all_t  <- get_sigma_all_t(data_all$sigma_start)
  data_all
}

# Add nonlinear variables, used for Lasso CV - Model 1 and 2
add_nonlinears_model1 <- function(data) {
  data <- data %>%
    mutate(nfirms2 = n_firms^2,
           nfirms3 = n_firms^3,
           sigma2 = sigma_all_t^2,
           sigma3 = sigma_all_t^3,
           nfirms_sigma = n_firms * sigma_all_t) %>%
    arrange(industry, parm_id, start)
}

# Add nonlinear variables, used for Lasso CV - Model 3
add_nonlinears_model3 <- function(data) {
  data <- data %>%
    mutate(nfirms2 = n_firms^2,
           nfirms3 = n_firms^3,
           sigma2 = sigma_all_t^2,
           sigma3 = sigma_all_t^3,
           gamma2 = gamma^2,
           gamma3 = gamma^3,
           nfirms_sigma = n_firms * sigma_all_t,
           nfirms_theta = n_firms * theta) %>%
    arrange(industry, parm_id, start)
}