# Evaluation  --------------------------------------------------------------
source(file = "analysis/scripts/set_up.R")



# Workflow  --------------------------------------------------------------

cartels <- readRDS(file = "analysis/data/cartels_no_enforcement.rds")

sum_cartels <- ts(data = rowSums(cartels))
title <- paste("Simulated cartels: 900 industries with each between 2 and 10 firms")
filename <- "analysis/figures/no_enforcement_cartels.png"
plot_cartels(title, sum_cartels, filename)


cartels_duration <- read.table("analysis/data/pop_duration_no_enforce.csv", header = TRUE, sep = ";")


parms <- read.table("analysis/data/parms_no_enforcement.csv", header = TRUE, sep = ";")





# Plotting  --------------------------------------------------------------

ggplot(parms, aes(x=n_firms, y=mean_sum_pop))  +
  geom_col(color = "black", fill = "white") +
  scale_x_continuous(breaks = c(2:10)) +
  xlab("Number of Firms in Industry") +
  ylab("Mean Number of Cartels in one Time Period") +
  theme_bw()
ggsave("analysis/figures/no_enforcement_mean_sum.png")

ggplot(parms, aes(x=n_firms, y=mean_duration_pop))  +
  geom_col(color = "black", fill = "white") +
  scale_x_continuous(breaks = c(2:10)) +
  xlab("Number of Firms in Industry") +
  ylab("Mean Duration of Cartels") +
  theme_bw()
ggsave("analysis/figures/no_enforcement_mean_duration.png")



