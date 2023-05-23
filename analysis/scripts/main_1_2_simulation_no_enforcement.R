source(file = "analysis/scripts/functions_simulation.R")

# Set basic parameters  --------------------------------------------------------------

# EC cartels start at 1964
allperiods <- 1000
periodsNoLen <- allperiods/2
periodsLen <- allperiods/2
timefactor <- 12
r_1 <- 0.03 #interest rate


# entry in cartel for certain number of firms
get_in_cartel <- function(ind, ICC){
  ind_entry <- if_else(ind > ICC, 1, 0) # big vector
  in_cartel <- matrix(as.numeric(ind_entry), ncol = ncol(ind)) # matrix with column for each firm
}



# Simulation  --------------------------------------------------------------

# simulate deltas. set seed different for every industry and every row of parms
n_max <- 10 # max number of firms
parms <- ICC_nfirms(n_max) # different ICC dependent on n


n_industries <- 300

  # array: dim = rows, columns, matrices
cartels <- array(0,dim = c(allperiods, n_industries, nrow(parms)))

# generate random seed from time
op <- options(digits.secs = 6)
sim_seed <- as.numeric(Sys.time())
set.seed(sim_seed)
sim_seed <- 1673800214

  
for (k in 1:nrow(parms)) {  # for all parms
  allcartels <- matrix(0, nrow = allperiods, ncol = n_industries)
  for (i in 1:n_industries) {
    count <- sim_seed + (i-1)*n_industries*10 + (k-1)*100   # different seed for each parm
    all_ind_delta <- ind_delta(count, parms$n_firms[k])
    # who wants to be in cartel?
    in_cartel <- get_in_cartel(all_ind_delta, parms$ICC[k])
    # who is in cartel? allow for incomplete cartels
    firms_in_cartel <- check_firm_share(in_cartel, 0.8) * in_cartel
    allcartels[, i] <- ifelse(rowSums(firms_in_cartel)>0, 1, 0)
  }
  cartels[,, k] <- allcartels
}  

saveRDS(cartels, file = "analysis/data/seeds_2/cartels_no_enforcement.rds")
cartels_no_enf <- readRDS("analysis/data/seeds_2/cartels_no_enforcement.rds")



# ----------------------------------------------------------------


# Calculate cartel durations
cd <- apply(cartels_no_enf, MARGIN=3, FUN=get_cartel_duration)
for (i in 1:nrow(parms)) {
  cd[[i]]$n_firms <- parms$n_firms[i]
}
cartels_duration <- do.call(rbind, cd)
write.table(cartels_duration, file = "analysis/data/seeds_2/duration_no_enforcement.csv", row.names = FALSE, sep = ";")


# ----------------------------------------------------------------


# mean sum of cartels and mean duration for every parameter (number of firms in industry)
parms$mean_sum_pop <- round(apply(cartels_no_enf, MARGIN=3, FUN=get_mean_sum_cartels), 2)
parms$mean_duration_pop <- round(apply(cartels_no_enf, MARGIN=3, FUN=get_mean_duration), 0)
parms$ICC <- round(parms$ICC, 2)


write.table(parms, file = "analysis/data/seeds_2/parms_no_enforcement.csv", row.names = FALSE, sep = ";")
k <- kbl(parms, "latex", booktabs = T, linesep = "")
save_kable(k, file = "analysis/data/seeds_2/parms_no_enforcement.tex")


