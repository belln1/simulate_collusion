#rm(list = ls())
library(psych) # describe()
library(dplyr)
library(purrr)
library(tidyr)
library(DescTools) # Rev() reverse order of rows and columns in a matrix
library(gtools) # cut quantiles
library(ggplot2)
library(ggfortify) # time series plots
library(forecast)

library(ggmosaic) # mosaic plots
library(NCmisc) # which packages are used

library(kableExtra)
library(abind)





# General Functions --------------------------------------------------------------

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# Simulation Functions  --------------------------------------------------------------

get_delta <- function(r){1/(1+r)}
anyfunction <- function(x){atan(x*2)/(pi) + 0.5}

# randomly return a number of +/- 0.01
random_steps <- function(n, seed){
  set.seed(seed)
  x <- rbinom(n, 1, 0.5)
  (2*x - 1)/100
}

## discount factor delta based on random walk for interest r
get_deltas_r <- function(start, periods, seed) {
  steps <- random_steps(periods-1, seed)
  walk_r <- cumsum(c(start, steps))
  deltas <- anyfunction(get_delta(walk_r))
} 

# 1 industry with n firms, each with random walk deltas (same seed for every industry)
ind_delta <- function(count, n_firms){
  replicate(n_firms, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)})
}

# Model 1: ICC depending on number of firms (cite Stigler 1964)
ICC_basic <- function(n) {
  1-1/n
}

# Basic ICC for different number of firms  
ICC_nfirms <- function(n_max){
  n_firms <- 2:n_max
  ICC <- tibble(n_firms, ICC = ICC_basic(n_firms))
}


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

get_detection <- function(periods, rho, seed){
  set.seed(seed)
  x <- matrix(as.numeric(runif(periods) <= 20*rho/allperiods), nrow = periods)
}

# does a firm want to be in a cartel?
# reduce(function, vector) applies function on the first two elements of vector, then on the result of that and the third element
# accumulate=TRUE returns all results, accumulate=FALSE returns only last result
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
simulate_firms <- function(i, parms, k, seed_start) {
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
  
  detection <- get_detection(allperiods, rho, seed=count)
  
  for (j in 1:allperiods) 
    
  {
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
      
      # change rho if there are more than one periods left, only for detected firms in cartel
      if (parms$structured & (allperiods-j)>1) {
        #rho[range,] = increase_rho(rho[range,], n_times_caught[range,])
        # row-wise matrix multiplication with vector
        change <- sweep(n_times_caught[range,], MARGIN = 2, (firms_in_cartel[j,] * detection[j,]), `*`)
        rho[range,] = increase_rho(rho[range,], change)
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
  return(list(firms = firms_in_cartel, detection = detection, ICC_entry = ICC_entry, ICC_exit = ICC_exit, rho_firms = rho))
}



# Evaluation Functions  --------------------------------------------------------------

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
  start <- rename(start, "start" = row)
  end <- as_tibble(which(endtimes==1, arr.ind = TRUE))
  end <- rename(end, "end" = row)
  df <- cbind(start, end=end$end)
  df$duration <- df$end - df$start + 1
  df <- df %>%
    mutate(cartel = 1) %>%
    rename(industry = col) %>%
    group_by(industry) %>%
    mutate(cartel = cumsum(cartel),
           startyear = ceiling(start/timefactor),
           endyear = ceiling(end/timefactor),
           duration_year = endyear - startyear + 1) %>%
    arrange(industry, cartel, start) %>%
    relocate(industry, cartel, start, end, duration)
  return(df)
}


get_mean_duration <- function(cartels){
  ifelse(sum(cartels) > 0, sum(cartels)/sum(get_starttimes(cartels)), 0)
}

get_mean_sum_cartels <- function(cartels) {
  mean(rowSums(cartels))
}

# Calculate cartel durations
get_enforcement_duration <- function(cartels, parms) {
  cd <- apply(cartels, MARGIN=3, FUN=get_cartel_duration)
  for (i in 1:nrow(parms)) {
    cd[[i]]$n_firms <- parms$n_firms[i]
    cd[[i]]$rho_start <- parms$rho_start[i]
    cd[[i]]$theta_len <- parms$theta_len[i]
    cd[[i]]$gamma <- parms$gamma[i]
    cd[[i]]$structured <- parms$structured[i]
    cd[[i]]$parm_id <- i
  }
  cartels_duration <- do.call(rbind, cd)
}




# Plotting Functions  --------------------------------------------------------------

plot_cartels <- function(title, sum_cartels, filename) {
  f <- autoplot(sum_cartels) +
    guides(color=guide_legend("")) +
    ggtitle(title) +
    xlab("Time") +
    ylab("Number of cartels")
  print(f)
  ggsave(filename)
}

basic_autoplot <- function(sim_cartels, pallete){
  f <- autoplot(sim_cartels) +
    guides(color="none") +
    xlab("Time") +
    ylab("Number of cartels") +
    ylim(0,500) +
    scale_colour_manual(values=pallete) 
  print(f)
}
