rm(list = ls())
library(psych) # describe()
library(dplyr)
library(purrr)
library(tidyr)
library(DescTools)
library(gtools) # cut quantiles
library(ggplot2)
library(ggfortify) # time series plots
library(forecast)

library(ggmosaic) # mosaic plots
library(NCmisc) # which packages are used

library(kableExtra)

# Which packages do we use?
p <- list.functions.in.file("analysis/scripts/simulation.R")
summary(p)



# Set basic parameters  --------------------------------------------------------------
seed_start <- 100

allperiods <- 1000
periodsNoLen <- allperiods/2
periodsLen <- allperiods/2
n_industries <- 100
timefactor <- 12

r_1 <- 0.03 #interest rate


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
  #  ts(deltas, start = c(1964,1), frequency = timefactor)
} 


# 1 industry with n firms, each with random walk deltas (same seed for every industry)
ind_delta <- function(count, n_firms){
  replicate(n_firms, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)})
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


# Plotting Functions  --------------------------------------------------------------

plot_cartels <- function(title, sum_cartels, filename) {
  f <- autoplot(sum_cartels) +
    guides(color=guide_legend("")) +
    ggtitle(title) +
    xlab("Time") +
  #  ylim(0,500) +
    ylab("Number of cartels")
  print(f)
  ggsave(filename)
}
