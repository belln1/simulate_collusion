source(file = "analysis/scripts/set_up.R")
# TODO: Combine evaluation.R and evaluation_all.R, for all struc and leniency

# Make one big duration dataset
parms_in <- tibble(
  theta = c(1, 0.5, 0, 1, 0.5, 0),
  struc = c(0, 0, 0, 1, 1, 1)
)
filename <- paste("analysis/data/duration_enforce_", parms_in$struc, "struc_", parms_in$theta, "theta.csv", sep = "")
pop_duration <- read.table(filename[1], header = TRUE, sep = ";")
for (i in 2:length(filename)) {
  x <- read.table(filename[i], header = TRUE, sep = ";")
  pop_duration <- bind_rows(pop_duration, x)
}

# Prepare data
#pop_duration$n_firms_factor <- factor(pop_duration$n_firms)
#pop_duration$rho_start_factor <- factor(pop_duration$rho_start)
#pop_duration$theta_len_factor <- factor(pop_duration$theta_len)
pop_duration$duration_quant <-  quantcut(pop_duration$duration, q = 4)


# Chi-Square test for leniency and duration
# only struc=0, all leniency, rho = 0.2
data <- filter(pop_duration, structured==0, rho_start==0.2)
t_len <- table(data$theta_len, data$duration_quant)
t_len
chisq.test(t_len)
chisq.test(t_len, simulate.p.value = TRUE)


# combine all mean_duration files and save as csv and tex
filename <- paste("analysis/data/parms_enforcement_", parms_in$struc, "struc_", parms_in$theta, "theta.csv", sep = "")
mean_duration <- read.table(filename[1], header = TRUE, sep = ";")
for (i in 2:length(filename)) {
  x <- read.table(filename[i], header = TRUE, sep = ";")
  mean_duration <- bind_rows(mean_duration, x)
}
name <- "all"
write.table(mean_duration, file = paste("analysis/data/parms_enforcement_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")
k <- kbl(mean_duration, "latex", booktabs = T, linesep = "")
save_kable(k, file = paste("analysis/data/parms_enforcement_", name, ".tex", sep = ""))


# mean duration rho = 0.1
data_mean_dur <- mean_duration %>%
  filter(structured==0, rho_start==0.1, theta_len==1) %>%
  arrange(n_firms) %>%
  select(n_firms, rho_start, mean_sum_detected, mean_duration_detected, mean_sum_population, mean_duration_population)

name <- paste(0, "struc_", "0-1", "rho_", 1, "theta", sep = "")
write.table(data_mean_dur, file = paste("analysis/data/parms_enforcement_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")
k <- kable(data_mean_dur, "latex",  booktabs = T, linesep = "")
save_kable(k, file = paste("analysis/data/parms_enforcement_", name, ".tex", sep = ""))


# mean duration rho = 0.3
data_mean_dur <- mean_duration %>%
#  filter(structured==0, rho_start==0.25, theta_len==1) %>%
  filter(n_firms>3, structured==0, theta_len==1) %>%
  arrange(n_firms) %>%
  select(n_firms, rho_start, mean_sum_detected, mean_duration_detected, mean_sum_population, mean_duration_population)

name <- paste(0, "struc_", "4-7", "n_", 1, "theta", sep = "")
write.table(data_mean_dur, file = paste("analysis/data/parms_enforcement_", name, ".csv", sep = ""), row.names = FALSE, sep = ";")
k <- kable(data_mean_dur, "latex",  booktabs = T, linesep = "")
save_kable(k, file = paste("analysis/data/parms_enforcement_", name, ".tex", sep = ""))



# Make Figures
# Different ICC over all the same discount factors
plot_deltas_n <- function(n) {
  #  count <- n*10  # plots different deltas for every graph
  count <- seed_start  # plots same deltas for every graph
  sim <- ts(replicate(n, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)}))
  f <- autoplot(sim) +
    guides(color="none") +
#    ggtitle(paste("Discount Factors and ICC = 1-1/n for ", n, " Firms")) +
    xlab("Time") +
    ylab("Discount Factor delta") +
    geom_hline(aes(yintercept=ICC_basic(n), linetype="Entry"), colour="blue") +
    scale_linetype_manual(name="ICC", values = c(1), guide = guide_legend(override.aes = list(color = c("blue")))) +
    theme(legend.position = "bottom")
  print(f)
#  filename <- paste("analysis/figures/no_enforcement_deltas_", n , "_firms.png")
#  ggsave(filename)
}
print(plot_deltas_n(6))

plot_deltas <- function(sim) {
  f <- autoplot(sim) +
    #    guides(color="none") +
    guides(color=guide_legend("")) +
#    ggtitle(title) +
    xlab("Time") +
    ylab("Discount Factor delta")
  #    geom_hline(aes(yintercept=get_ICC_entry(n, rho, gamma), linetype="Entry"), colour="blue") +
  #    geom_hline(aes(yintercept=get_ICC_exit(n, rho, gamma, theta), linetype="Exit"), colour="red") +
#  scale_linetype_manual(name="ICC", values = c(1, 1), guide = guide_legend(override.aes = list(color = c("blue", "red"))))
 # print(f)
#  ggsave(filename)
}


# 5 firms, 0.15 rho
n_firms <- 5
rho <- 0.15
theta_len <- 1
#structured <- 0
gamma <- 0.9
count <- seed_start  # plots same deltas for every graph
deltas <- ts(replicate(n_firms, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)}))
ICC_no_enforc <- ICC_basic(n_firms)
ICC_entry_0struc <- get_ICC_entry(n_firms, rho, theta_len)
ICC_exit_0struc <- round(get_ICC_exit(n_firms, rho, gamma, theta_len),3)

parms <- tibble(
  n_firms = n_firms,
  rho_start = rho,
  theta_len = theta_len,
  structured = 1,
  gamma = gamma
)
parms <- parms %>%
  arrange(n_firms, rho_start, theta_len, structured)

sim_list <- simulation(1, parms[1,], 1)
ICC_entry_1struc <- sim_list$ICC_entry[,5]
ICC_exit_1struc <- sim_list$ICC_exit[,5]

sim <- ts(data = cbind(ICC_no_enforc, ICC_entry_0struc, ICC_exit_0struc, ICC_entry_1struc, ICC_exit_1struc, deltas))
colnames(sim) <- c("ICC without detection", "ICC entry constant detection", "ICC exit constant detection", "ICC entry increasing detection", "ICC exit increasing detection", paste("firm", 1:ncol(deltas)))

p <- plot_deltas_fines(sim)
#print(p)
ggsave(filename = "analysis/figures/ICC/all_5firms_0-15rho_1theta.png")
