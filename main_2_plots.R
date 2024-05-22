rm(list = ls())
source(file = "analysis/scripts/functions_simulation.R")

# PLOT: Discount Factors and ICC for Different Models
allperiods <- 1000
periodsNoLen <- 0 # thetas remain constant for all time periods
periodsLen <- allperiods
timefactor <- 12
r_1 <- 0.03 #interest rate

# 5 firms, 0.15 rho
n_firms <- 5
rho <- 0.15
gamma <- 0.9
theta <- 1
i <- 1
k <- 1

seed_start <- 100
n_industries <- 300
count <- seed_start + (i-1)*n_industries*10 + (k-1)*100
deltas <- ts(replicate(n_firms, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)}))

ICC_no_enforc <- get_ICC_model1(n_firms)
ICC_entry_0struc <- get_ICC_entry_model3(n_firms, rho, gamma)
ICC_exit_0struc <- get_ICC_exit_model3(n_firms, rho, gamma, theta)

parms <- tibble(
  n_firms = n_firms,
  rho_start = rho,
  theta = theta,
  structured = 1,
  gamma = gamma
)
parms <- parms %>%
  arrange(n_firms, rho_start, theta, structured)

sim_list <- simulate_firms_model3(1, parms[1,], 1, seed_start)
ICC_entry_1struc <- sim_list$ICC_entry[,5]
ICC_exit_1struc <- sim_list$ICC_exit[,5]


# II and III in one plot
# II) constant rho
theta_in <- c(0.5) #0 - full leniency, 0.5 - 50% leniency
theta = c(rep(1, periodsNoLen), rep(theta_in, periodsLen))

ICC_entry <- get_ICC_entry_model3(n_firms, rho, gamma)
ICC_exit <- get_ICC_exit_model3(n_firms, rho = rho, theta = theta, gamma = gamma)
sim <- ts(data = cbind(ICC_no_enforc, ICC_entry, ICC_exit, deltas))
colnames(sim) <- c("ICC without detection", "ICC entry", "ICC exit", paste("firm", 1:ncol(deltas)))
pallete <- c('blue4', 'blue', 'red', 'lightcoral', 'aquamarine3', 'cyan4', 'brown', 'orange')
p_2 <- autoplot(sim) +
  ylim(0.79, 0.95) +
  xlab("Time") +
  ylab("Discount Factor \u03b4") +
  labs(caption = "a) Model IIIa") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5, vjust = 0, size = 10), # move caption to the middle
        axis.title = element_text(size = 10)
  )
#p_2


# III) Increasing rho, Plot with different ICC for leniency = 0.5, Plot ICCs for firm 1
parms$theta <- 0.5
sim_list <- simulate_firms_model3(1, parms[1,], 1, seed_start)
sim <- ts(data = cbind(ICC_no_enforc, sim_list$ICC_entry[,1], sim_list$ICC_exit[,1], deltas))
colnames(sim) <- c("ICC without detection", "ICC entry", "ICC exit", paste("firm", 1:ncol(deltas)))
p_3 <- autoplot(sim) +
  ylim(0.79, 0.95) +
  xlab("Time") +
  ylab("Discount Factor \u03b4") +
  labs(caption = "b) Model IIIb") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5, size = 10),# move caption to the middle
        axis.title = element_text(size = 10)
  )
#p_3
plot_2_3 <- p_2 + p_3 + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete) 
plot_2_3




###-------------------------------------------------------------------------------------------------------------------------------------
###-------------------------------------------------------------------------------------------------------------------------------------


# PLOT: ALL CARTELS OVER TIME

### ADJUST y axis FOR OTHER DATA ###
y_axis <- c(0,60)


pallete <- c('blue', 'red')
y_label <- "Percentage of cartels"
x_label <- "Time"
status_all <- c("population", "detected", "undetected")
options(repr.plot.width =5.5, repr.plot.height =6)

# Model I
directory <- "model1"
cartels_population <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[1], ".rds", sep = ""))
cartels_detected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[2], ".rds", sep = ""))
parmname <- paste("analysis/data/", directory, "/parms.csv", sep = "")
parms <- read.table(parmname, header = TRUE, sep = ";")
c_det <- rowSums(cartels_detected)/(dim(cartels_detected)[2] * dim(cartels_detected)[3])
c_pop <- rowSums(cartels_population)/(dim(cartels_population)[2] * dim(cartels_population)[3])
sim_cartels <- ts(data = cbind(c_pop, c_det))
colnames(sim_cartels) <- c("Population", "Sample")
plot1 <- autoplot(sim_cartels*100) +
  ylim(y_axis) +
  xlab(x_label) +
  ylab(y_label) +
  labs(caption = "a) Model I") +
  theme(
    plot.caption = element_text(hjust = 0.5, vjust = 0, size = 10), # move caption to the middle
    axis.title = element_text(size = 10)
  ) 
plot1

# Model II
directory <- "model2"
cartels_population <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[1], ".rds", sep = ""))
cartels_detected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[2], ".rds", sep = ""))
parmname <- paste("analysis/data/", directory, "/parms.csv", sep = "")
parms <- read.table(parmname, header = TRUE, sep = ";")
c_det <- rowSums(cartels_detected)/(dim(cartels_detected)[2] * dim(cartels_detected)[3])
c_pop <- rowSums(cartels_population)/(dim(cartels_population)[2] * dim(cartels_population)[3])
sim_cartels <- ts(data = cbind(c_pop, c_det))
colnames(sim_cartels) <- c("Population", "Sample")
plot2 <- autoplot(sim_cartels*100) +
  ylim(y_axis) +
  xlab(x_label) +
  ylab(y_label) +
  labs(caption = "b) Model II") +
  theme(
    plot.caption = element_text(hjust = 0.5, vjust = 0, size = 10), # move caption to the middle
    axis.title = element_text(size = 10)
  ) 
plot2
plot12 <- plot1 + plot2 + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete)
plot12



# Model III
directory <- "model3"
cartels_population <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[1], ".rds", sep = ""))
cartels_detected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[2], ".rds", sep = ""))
parmname <- paste("analysis/data/", directory, "/parms.csv", sep = "")

# cartels_population <- readRDS(file = paste("C:/Users/bell/ZHAW/Research Collusion - General/Code/simulation/simulate_collusion_1_2_constant_theta/analysis/data/", seeds_dir, "/cartels/cartels_", status_all[1], ".rds", sep = ""))
# cartels_detected <- readRDS(file = paste("C:/Users/bell/ZHAW/Research Collusion - General/Code/simulation/simulate_collusion_1_2_constant_theta/analysis/data/", seeds_dir, "/cartels/cartels_", status_all[2], ".rds", sep = ""))
# parmname <- paste("C:/Users/bell/ZHAW/Research Collusion - General/Code/simulation/simulate_collusion_1_2_constant_theta/analysis/data/", seeds_dir, "/parms.csv", sep = "")
parms <- read.table(parmname, header = TRUE, sep = ";")

# Model IIIa, unstructured
x_unstruc <- which(parms$structured == 0)
y_det_unstruc <- cartels_detected[,,x_unstruc]
y_pop_unstruc <- cartels_population[,,x_unstruc]
c_det_unstruc <- rowSums(y_det_unstruc)/(dim(y_det_unstruc)[2] * dim(y_det_unstruc)[3])
c_pop_unstruc <- rowSums(y_pop_unstruc)/(dim(y_pop_unstruc)[2] * dim(y_pop_unstruc)[3])

sim_cartels <- ts(data = cbind(c_pop_unstruc, c_det_unstruc))
colnames(sim_cartels) <- c("Population", "Sample")
plot3a <- autoplot(sim_cartels*100) +
#  ylim(y_axis) +
  xlab(x_label) +
  ylab(y_label) +
  labs(caption = "a) Model IIIa") +
  theme(
    plot.caption = element_text(hjust = 0.5, vjust = 0, size = 10), # move caption to the middle
    axis.title = element_text(size = 10)
  ) 
plot3a

# Model IIIb, structured
x_struc <- which(parms$structured == 1)
y_det_struc <- cartels_detected[,,x_struc]
y_pop_struc <- cartels_population[,,x_struc]
c_det_struc <- rowSums(y_det_struc)/(dim(y_det_struc)[2] * dim(y_det_struc)[3])
c_pop_struc <- rowSums(y_pop_struc)/(dim(y_pop_struc)[2] * dim(y_pop_struc)[3])

sim_cartels_struc <- ts(data = cbind(c_pop_struc, c_det_struc))
colnames(sim_cartels_struc) <- c("Population", "Sample")
plot3b <- autoplot(sim_cartels_struc*100) +
#  theme(text = element_text(family = "Arial")) +
#  ylim(y_axis) +
  xlab(x_label) +
  ylab(y_label) +
  labs(caption = "a) Model IIIb") +
  theme(
    plot.caption = element_text(hjust = 0.5, vjust = 0, size = 10), # move caption to the middle
    axis.title = element_text(size = 10)
  ) 
plot3b
plot3ab <- plot3a + plot3b + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete) 
plot3ab

#& theme(text = element_text(family = "Times New Roman")) 
#ggsave(filename="plot3ab.eps", plot = last_plot(), device = cairo_ps, width = 7, height = 4.5)
#dev.off()


