source(file = "analysis/scripts/set_up.R")


# Simulate Model 1: ICC depending on number of firms (cite Stigler 1964)
ICC_basic <- function(n) {
  1-1/n
}




# Basic ICC for different number of firms  
# todo: delete exit
ICC_nfirms <- function(n_max){
  n_firms <- 2:n_max
  ICC <- tibble(n_firms, ICC = ICC_basic(n_firms))
}

# entry in cartel for certain number of firms
get_in_cartel <- function(ind, ICC){
  ind_entry <- if_else(ind > ICC, 1, 0) # big vector
  in_cartel <- matrix(as.numeric(ind_entry), ncol = ncol(ind)) # matrix with column for each firm
}

# we have a cartel if enough firms want to be in a cartel. firm_share = 1 means complete cartels. vector with 0 and 1 in all times
check_firm_share <- function(firms, firm_share){
  as.numeric(rowSums(firms) >= ncol(firms)*firm_share)
}



plot_deltas <- function(n) {
#  count <- n*10  # plots different deltas for every graph
  count <- seed_start  # plots same deltas for every graph
  sim <- ts(replicate(n, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)}))
  f <- autoplot(sim) +
    guides(color="none") +
    ggtitle(paste("Discount Factors and ICC = 1-1/n for ", n, " Firms")) +
    xlab("Time") +
    ylab("Discount Factor delta") +
    geom_hline(aes(yintercept=ICC_basic(n), linetype="Entry"), colour="blue") +
    scale_linetype_manual(name="ICC", values = c(1), guide = guide_legend(override.aes = list(color = c("blue"))))
  print(f)
  filename <- paste("analysis/figures/no_enforcement_deltas_", n , "_firms.png")
  ggsave(filename)
  
}
for (i in 2:10) {
#  plot_deltas(i)
}




# Simulation  --------------------------------------------------------------

# simulate deltas. set seed different for every industry and every row of parms
n_max <- 10 # max number of firms
# todo: refactor ICC_nfirms()
parms <- ICC_nfirms(n_max) # different ICC dependent on n




  # array: dim = rows, columns, matrices
cartels <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
  
for (k in 1:nrow(parms)) {  # for all parms
  allcartels <- matrix(0, nrow = allperiods, ncol = n_industries)
  for (i in 1:n_industries) {
    #count <- seed_start + (i-1)*n_industries*10 + (k-1)*100   # different seed for each parm
    count <- seed_start + (i-1)*n_industries*10   # same seed for each parm
    
    all_ind_delta <- ind_delta(count, parms$n_firms[k])
    # who wants to be in cartel?
    in_cartel <- get_in_cartel(all_ind_delta, parms$ICC[k])
    # who is in cartel? allow for incomplete cartels
    firms_in_cartel <- check_firm_share(in_cartel, 0.8) * in_cartel
    allcartels[, i] <- ifelse(rowSums(firms_in_cartel)>0, 1, 0)
  }
  cartels[,, k] <- allcartels
}  

saveRDS(cartels, file = "analysis/data/cartels_no_enforcement.rds")


# mean sum of cartels and mean duration for every parameter (numer of firms in industry)
parms$mean_sum_pop <- round(apply(cartels, MARGIN=3, FUN=get_mean_sum_cartels), 2)
parms$mean_duration_pop <- round(apply(cartels, MARGIN=3, FUN=get_mean_duration), 0)
parms$ICC <- round(parms$ICC, 2)

write.table(parms, file = "analysis/data/parms_no_enforcement.csv", row.names = FALSE, sep = ";")
k <- kable(parms, "latex")
save_kable(k, file = "analysis/data/parms_no_enforcement.tex")





# Calculate cartel durations

cd <- apply(cartels, MARGIN=3, FUN=get_cartel_duration)

for (i in 1:nrow(parms)) {
  cd[[i]]$n_firms <- parms$n_firms[i]
}
cartels_duration <- do.call(rbind, cd)

write.table(cartels_duration, file = "analysis/data/pop_duration_no_enforce.csv", row.names = FALSE, sep = ";")




# Test array

# x1 <- cd[[1]]
# x2 <- cd[[2]]
# x3 <- cd[[3]]
# x4 <- cd[[4]]
# x5 <- cd[[5]]
# x6 <- cd[[6]]
# x7 <- cd[[7]]
# x8 <- cd[[8]]
# x9 <- cd[[9]]
# 
# describe(x1)
# describe(x2)
# describe(x3)
# describe(x4)
# describe(x5)
# describe(x6)









