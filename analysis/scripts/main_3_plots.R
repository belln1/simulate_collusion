source(file = "analysis/scripts/functions_simulation.R")
# TODO: Combine evaluation.R and evaluation_all.R, for all struc and leniency

library(tframePlus)

# shared legend (plot_layout(guides = "collect")). install from cran bc. there are other package versions around!
install.packages("patchwork",repos = "https://cloud.r-project.org")
library("patchwork")


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
# PLOT: Discount Factors and ICC for Different Models

# 5 firms, 0.15 rho
n_firms <- 5
rho <- 0.15
gamma <- 0.9
theta_len <- 1
n_industries <- 300
#structured <- 0
seed_start <- 100
count <- seed_start  # plots same deltas for every graph
deltas <- ts(replicate(n_firms, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)}))
ICC_no_enforc <- ICC_basic(n_firms)
ICC_entry_0struc <- get_ICC_entry(n_firms, rho, gamma)
ICC_exit_0struc <- get_ICC_exit(n_firms, rho, gamma, theta_len)
ICC_entry_0struc
ICC_exit_0struc

parms <- tibble(
  n_firms = n_firms,
  rho_start = rho,
  theta_len = theta_len,
  structured = 1,
  gamma = gamma
)
parms <- parms %>%
  arrange(n_firms, rho_start, theta_len, structured)

sim_list <- simulate_firms(1, parms[1,], 1, seed_start)
ICC_entry_1struc <- sim_list$ICC_entry[,5]
ICC_exit_1struc <- sim_list$ICC_exit[,5]

# a)
sim <- ts(data = cbind(ICC_no_enforc, ICC_exit_0struc, ICC_entry_0struc, 0, deltas))
colnames(sim) <- c("ICC without detection", "ICC exit (IIa)", "ICC entry (= ICC exit full leniency)", "ICC exit (0.5 leniency, IIb)", paste("firm", 1:ncol(deltas)))
pallete <- c('blue4', 'red', 'blue', 'deeppink', 'lightcoral', 'aquamarine3', 'cyan4', 'brown', 'orange')
p_a <- autoplot(sim) +
  ylim(0.79, 0.95) +
  xlab("Time") +
  ylab("Discount Factor \u03b4") +
  labs(caption = "a) Model IIa") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5, vjust = 0, size = 13)# move caption to the middle
  )
p_a

# b) constant rho, Plot with different ICC for different leniency
theta_len <- c(0.5) #0 - full leniency, 0.5 - 50% leniency
theta = c(rep(1, periodsNoLen), rep(theta_len, periodsLen))

ICC_entry <- get_ICC_entry(n_firms, rho, gamma)
ICC_exit <- get_ICC_exit(n_firms, rho = rho, theta = theta, gamma = gamma)
#sim <- ts(data = cbind(deltas, ICC_no_enforc, ICC_exit[1], ICC_exit[2], ICC_exit[3]))
sim <- ts(data = cbind(ICC_no_enforc, 0, ICC_entry, ICC_exit, deltas))
#sim <- ts(data = cbind(ICC_no_enforc, 0, ICC_exit[1], ICC_exit[2], deltas))
colnames(sim) <- c("ICC without detection", "ICC exit (IIa)", "ICC entry (= ICC exit full leniency)", "ICC exit (0.5 leniency, IIb)", paste("firm", 1:ncol(deltas)))
pallete <- c('blue4', 'red', 'blue', 'deeppink', 'lightcoral', 'aquamarine3', 'cyan4', 'brown', 'orange')
p_b <- autoplot(sim) +
  ylim(0.79, 0.95) +
  xlab("Time") +
  ylab("Discount Factor \u03b4") +
  labs(caption = "b) Model IIb") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5, vjust = 0, size = 13)# move caption to the middle
  )
p_b
plot_ab <- p_a + p_b + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete)
plot_ab
ggsave("analysis/figures/ICC/ICC_ab.png")


# c)  increasing rho, no leniency (theta=1)
sim <- ts(data = cbind(ICC_no_enforc, ICC_exit_1struc, ICC_entry_1struc, 0, deltas))
colnames(sim) <- c("ICC without detection", "ICC exit (IIIa)", "ICC entry (= ICC exit full leniency)", "ICC exit (0.5 leniency, IIIb)", paste("firm", 1:ncol(deltas)))
#colnames(sim) <- c("ICC without detection", "ICC exit", "ICC entry", paste("firm", 1:ncol(deltas)))
plot_deltas <- function(sim) {
  f <- autoplot(sim) +
    ylim(0.79, 0.95) +
    xlab("Time") +
    ylab("Discount Factor \u03b4") +
    labs(caption = "a) Model IIIa") +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0.5, size = 13)# move caption to the middle
    )
}
p_c <- plot_deltas(sim) 
p_c

# d) increasing rho, Plot with different ICC for leniency = 0.5
parms$theta_len <- 0.5
sim_list <- simulate_firms(1, parms[1,], 1, seed_start)
sim <- ts(data = cbind(ICC_no_enforc, 0, sim_list$ICC_entry[,5], sim_list$ICC_exit[,5], deltas))
colnames(sim) <- c("ICC without detection", "ICC exit (IIIa)", "ICC entry (= ICC exit full leniency)", "ICC exit (0.5 leniency, IIIb)", paste("firm", 1:ncol(deltas)))
#colnames(sim) <- c(paste("firm", 1:ncol(deltas)), "ICC without detection", "ICC_entry = ICC exit (full leniency)", "ICC exit (0.5 leniency fine reduction", "ICC exit (no leniency)")
p_d <- autoplot(sim) +
  ylim(0.79, 0.95) +
  xlab("Time") +
  ylab("Discount Factor \u03b4") +
  labs(caption = "b) Model IIIb") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5, size = 13)# move caption to the middle
  )
p_d
plot_cd <- p_c + p_d + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete)
plot_cd
ggsave("analysis/figures/ICC/ICC_cd.png")


# increasing rho, different leniency => too complicated
# theta <- c(0, 0.5, 1)
# parms_d <- parms[rep(seq_len(nrow(parms)), 3), ]
# parms_d$theta_len <- theta
# sim_list_0 <- simulate_firms(1, parms_d[1,], 1, seed_start)
# sim_list_05 <- simulate_firms(1, parms_d[2,], 1, seed_start)
# sim_list_1 <- simulate_firms(1, parms_d[3,], 1, seed_start)
# 
# ICC_entry_0 <- sim_list_0$ICC_entry[,5]
# ICC_exit_0 <- sim_list_0$ICC_exit[,5]
# ICC_entry_05 <- sim_list_05$ICC_entry[,5]
# ICC_exit_05 <- sim_list_05$ICC_exit[,5]
# ICC_entry_1 <- sim_list_1$ICC_entry[,5]
# ICC_exit_1 <- sim_list_1$ICC_exit[,5]

#sim <- ts(data = cbind(ICC_entry_0, ICC_entry_05, ICC_entry_1, ICC_exit_0, ICC_exit_05, ICC_exit_1, deltas))
#colnames(sim) <- c("ICC_entry_0", "ICC_entry_05", "ICC_entry_1", "ICC_exit_0", "ICC_exit_05", "ICC_exit_1", paste("firm", 1:ncol(deltas)))



#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------


# PLOT: ALL CARTELS OVER TIME