rm(list = ls())
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(DescTools)
library(ggfortify)
library(NCmisc)

# Which packages do we use?
p <- list.functions.in.file("analysis/scripts/simulation.R")
summary(p)

# Basic Functions --------------------------------------------------------------

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}


# Plotting Functions --------------------------------------------------------------

plot_deltas <- function(n) {
  count <- i*10
  sim <- ts(replicate(10, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)}))
  f <- autoplot(sim) +
    guides(color="none") +
    ggtitle(paste("Discount Factors for 10 Firms and ICC = 1-1/n for ", n, " Firms")) +
    xlab("Time") +
    ylab("Discount Factor delta") +
    geom_hline(aes(yintercept=ICC_basic(n), linetype="Entry"), colour="blue") +
    scale_linetype_manual(name="ICC", values = c(1), guide = guide_legend(override.aes = list(color = c("blue"))))
  #print(f)
}
for (i in 2:10) {
  # plot_deltas(i)
}

plot_sim <- function(title, sim, ylabel) {
  f <- autoplot(sim) +
    #guides(color="none") +
    guides(color=guide_legend("")) +
    ggtitle(title) +
    xlab("Time") +
    ylab(ylabel) 
  # geom_hline(aes(yintercept=get_ICC_entry(n, rho, gamma), linetype="Entry"), colour="blue") +
  # geom_hline(aes(yintercept=get_ICC_exit(n, rho, gamma, theta), linetype="Exit"), colour="red") +
  # scale_linetype_manual(name="ICC", values = c(1, 1), guide = guide_legend(override.aes = list(color = c("blue", "red")))) 
  print(f)
}


plot_cartels <- function(cartels_population, cartels_sample, parms) {
  sim_cartels <- ts(data = cbind(rowSums(cartels_population), rowSums(cartels_sample)))
  colnames(sim_cartels) <- c("Population", "Sample")
  title <- paste("Simulated cartels, ", parms$theta_len, " leniency reduction, ", parms$structured, " structured ", parms$rho_start, " start detections prob. ", parms$n_firms, " firms")
  ylabel <- "Number of cartels"
  plot_sim(title, sim_cartels, ylabel)
}


# Evaluation Functions --------------------------------------------------------------

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

get_mean_duration <- function(cartels){
  ifelse(sum(cartels) > 0, sum(cartels)/sum(get_starttimes(cartels)), 0)
}

get_mean_sum_cartels <- function(cartels) {
  mean(rowSums(cartels))
}


# Simulation Functions --------------------------------------------------------------

get_delta_1 <- function(r){1/(1+r)}
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
  deltas <- anyfunction(get_delta_1(walk_r))
#  ts(deltas, start = c(1964,1), frequency = timefactor)
} 


# 1 industry with n firms, each with random walk deltas (same seed for every industry)
ind_delta <- function(count, n_firms){
  replicate(n_firms, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)})
}

# Model 1: ICC depending on number of firms (cite Stigler 1964)
ICC_basic <- function(n) {
  1-1/n
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

# k is parm row
simulation <- function(i, parms, k) {
  gamma = parms$gamma
  theta = c(rep(1, periodsNoLen), rep(parms$theta_len, periodsLen))
  rho <- matrix(parms$rho_start, nrow = allperiods, ncol = parms$n_firms)
  n_times_caught <- matrix(0, nrow = allperiods, ncol = parms$n_firms)
  #ICC_entry <- matrix(ICC_basic(parms$n_firms), nrow = allperiods, ncol = parms$n_firms)
  #ICC_exit <- matrix(ICC_basic(parms$n_firms), nrow = allperiods, ncol = parms$n_firms)
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
      
      # no cartel in next period (if there are next periods)
      if(j<allperiods){
        firms_in_cartel[j+1,] = 0
        # increase number of times caught
        range <- (j+1):allperiods
        n_times_caught[range,] <- n_times_caught[range,] + rep.row(firms_in_cartel[j,] * detection[j,], allperiods-j)
      }
      
      # change rho if there are more than one periods left
      if (parms$structured & (allperiods-j)>1) {
        rho[range,] = increase_rho(rho[range,], n_times_caught[range,])
        
        # ICC_entry[range,] <- ICC_basic(parms$n_firms)
        #ICC_exit[range,] <- ICC_basic(parms$n_firms)
        
        ICC_entry[range,] <- get_ICC_entry(parms$n_firms, rho[range,], gamma)
        ICC_exit[range,] <- get_ICC_exit(parms$n_firms, rho[range,], gamma, theta[range])
        in_cartel[range,] <- get_in_cartel(all_ind_delta[range,], ICC_entry[range,], ICC_exit[range,])
        firms_in_cartel[range,] <- check_firm_share(in_cartel[range,], 0.8) * in_cartel[range,] # make a function out of this
        count = count + j
        detection[range,] <- get_detection(allperiods-j, rho[range,], seed=count)
      }
    }
  }
  
  # plot only if 5 firms..
  x <- n_times_caught[allperiods,]
  y <- which(x==max(x))
  z <- y[1]
  if(z>2){
    sim <- ts(data = cbind(all_ind_delta[,1:5], ICC_entry[,z], ICC_exit[,z]))
    colnames(sim) <- c("firm 1", "firm 2", "firm 3", "firm 4", "firm 5", "ICC entry", "ICC exit")
    title <- paste("Industry with ", parms$n_firms, " firms and increasing detection probability starting witih", parms$rho_start)
    ylabel <- "Discount Factor"
    #    plot_sim(title, sim, ylabel)
  }
  sum(firms_in_cartel)
  firms_sample <- get_sample(firms_in_cartel, detection)
  sum(firms_sample)
  cartel <- ifelse(rowSums(firms_in_cartel)>0, 1, 0)
  cartel_sample <- ifelse(rowSums(firms_sample)>0, 1, 0)
  return(list(cartel = cartel, cartel_sample = cartel_sample))
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


sim_parms <- function(n_industries, parms, k) {
  cartels_population <- matrix(0, nrow = allperiods, ncol = n_industries)
  cartels_sample <- matrix(0, nrow = allperiods, ncol = n_industries)
  
  for (i in 1:n_industries) {
    sim_list <- simulation(i, parms, k)
    cartels_population[, i] <- sim_list$cartel
    cartels_sample[, i] <- sim_list$cartel_sample
  }
  parms$mean_sum_pop <- round(get_mean_sum_cartels(cartels_population),2)
  parms$mean_duration_pop <- round(get_mean_duration(cartels_population),2)
  parms$mean_sum_sample <- round(get_mean_sum_cartels(cartels_sample),2)
  parms$mean_duration_sample <- round(get_mean_duration(cartels_sample),2)
  # plot_cartels(cartels_population, cartels_sample, parms)

  df_pop <- get_cartel_duration(cartels_population)
  df_sample <- get_cartel_duration(cartels_sample)
  df_sample$detected <- 1
  df_pop <- left_join(df_pop, df_sample)
  df_pop$detected[is.na(df_pop$detected)]=0
  
  df_pop$n_firms <- parms$n_firms
  df_pop$rho_start <- parms$rho_start
  df_pop$theta_len <- parms$theta_len
  df_pop$structured <- parms$structured
  return(df_pop)
}



# Workflow --------------------------------------------------------------

seed_start <- 100

allperiods <- 1000
periodsNoLen <- allperiods/2
periodsLen <- allperiods/2
n_industries <- 100
timefactor <- 12

r_1 <- 0.03 #interest rate

n_sim <- 36

# todo: make function with structured and leniency
parms <- tibble(
  n_firms = c(rep(2, n_sim/6), rep(3, n_sim/6), rep(4, n_sim/6), rep(5, n_sim/6), rep(6, n_sim/6), rep(7, n_sim/6)),
  rho_start = rep(c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35), n_sim/6),
  theta_len = rep(c(1), n_sim),
  structured = rep(c(1), n_sim),
  gamma = rep(0.9, n_sim)
)

parms <- parms %>%
  arrange(n_firms, rho_start, theta_len, structured)

get_duration_cartels_blank <- function() { 
  tibble(
  industry = numeric(),
  cartel = numeric(),
  start = numeric(),
  end = numeric(),
  duration = numeric(),
  startyear = numeric(),
  endyear = numeric(),
  duration_year = numeric(),
  detected = numeric(),
  n_firms = numeric(),
  structured = numeric(),
  rho_start = numeric(),
  theta_len = numeric()
  )
}

# todo: add leniency as parameters
parms$structured <- 1
duration_cartels_struc <- get_duration_cartels_blank()
for (k in 1:nrow(parms)) {
  duration_cartels_struc <- bind_rows(duration_cartels_struc, sim_parms(n_industries, parms[k,], k))
}

write.table(duration_cartels_struc, file = "analysis/data/pop_duration_struc.csv", row.names = FALSE, sep = ";")
sample_duration_struc <- filter(duration_cartels_struc, detected==1)
write.table(sample_duration_struc, file = "analysis/data/sample_duration_struc.csv", row.names = FALSE, sep = ";")

parms$structured <- 0
duration_cartels_unstruc <- get_duration_cartels_blank()
for (k in 1:nrow(parms)) {
  duration_cartels_unstruc <- bind_rows(duration_cartels_unstruc, sim_parms(n_industries, parms[k,], k))
}

write.table(duration_cartels_unstruc, file = "analysis/data/pop_duration_unstruc.csv", row.names = FALSE, sep = ";")
sample_duration_unstruc <- filter(duration_cartels_unstruc, detected==1)
write.table(sample_duration_unstruc, file = "analysis/data/sample_duration_unstruc.csv", row.names = FALSE, sep = ";")

