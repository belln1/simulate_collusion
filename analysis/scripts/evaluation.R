rm(list = ls())
library(dplyr)
library(purrr)
library(tidyr)
library(DescTools)
library(gtools) # cut quantiles
library(ggplot2)
library(ggfortify)
library(ggmosaic) # mosaic plots
library(NCmisc) # which packages are used

# todo: put common used functions in separate script
# source(file = "analysis/scripts/simulation.R")


# Plotting Functions --------------------------------------------------------------

# mean duration depending on parameters

get_mean_duration_by_detection <- function(duration_cartels) {
  duration_cartels %>%
    group_by(n_firms, rho_start, detected, structured) %>%
    summarise(mean_duration = mean(duration),
              num_cartels = n(),
              mean_num_cartels = sum(duration)/allperiods) %>%
    arrange(n_firms, rho_start, detected, structured)
}

get_mean_duration_all <- function(duration_cartels) {
  duration_cartels %>%
    group_by(n_firms, rho_start, structured) %>%
    summarise(mean_duration = mean(duration),
              num_cartels = n(),
              mean_num_cartels = sum(duration)/allperiods) %>%
    arrange(structured, rho_start, n_firms)
}


#mean_duration_cartels <- get_mean_duration_cartels(duration_cartels)


plot_mean_duration <- function(mean_duration_cartels) {
  ggplot(mean_duration_cartels, aes(x = n_firms, y = rho_start, size = mean_duration)) +
    geom_point() +
    xlab("number of firms") +
    ylab("detection probability") +
    labs(size = "mean duration of cartels")
}


plot_mean_dur_n_rho <- function(data, plotname){
  mean_data <- get_mean_duration_all(data)
  plot_mean_duration(mean_data)
  filename <- paste("analysis/figures/mean_duration_n_rho_", plotname, ".png", sep = "")
  ggsave(filename)
}





# Read and prepare data --------------------------------------------------------------
allperiods <- 1000

pop_duration_unstruc <- read.table("analysis/data/pop_duration_unstruc.csv", header = TRUE, sep = ";")
pop_duration_struc <- read.table("analysis/data/pop_duration_struc.csv", header = TRUE, sep = ";")
# factor for boxplots
pop_duration_struc$n_firms_factor <- factor(pop_duration_struc$n_firms)
pop_duration_unstruc$n_firms_factor <- factor(pop_duration_unstruc$n_firms)
pop_duration_struc$rho_start_factor <- factor(pop_duration_struc$rho_start)
pop_duration_unstruc$rho_start_factor <- factor(pop_duration_unstruc$rho_start)
pop_duration_struc$duration_quant <-  quantcut(pop_duration_struc$duration, q = 4)
pop_duration_unstruc$duration_quant <-  quantcut(pop_duration_unstruc$duration, q = 4)

undetected_duration_unstruc <- filter(pop_duration_unstruc, detected==0)
undetected_duration_struc <- filter(pop_duration_struc, detected==0)

sample_duration_unstruc <- filter(pop_duration_unstruc, detected==1)
sample_duration_struc <- filter(pop_duration_struc, detected==1)

data_unstruc <- get_mean_duration_by_detection(pop_duration_unstruc)
data_struc <- get_mean_duration_by_detection(pop_duration_struc)

data_struc_plot <- data_struc
data_unstruc_plot <- data_unstruc

data_struc_plot$detection <- ifelse(data_struc$detected==0, "undetected", "detected")
data_unstruc_plot$detection <- ifelse(data_unstruc$detected==0, "undetected", "detected")

# change x-values to plot symbols side by side
data_unstruc_plot$x_value <- ifelse(data_unstruc$detected==0, data_unstruc$n_firms+0.05, data_unstruc$n_firms-0.05)
data_struc_plot$x_value <- ifelse(data_struc$detected==0, data_struc$n_firms+0.05, data_struc$n_firms-0.05)


mean_duration_diffs <- pivot_wider(data_struc, names_from = detected,
                                   values_from = c(mean_duration, mean_num_cartels, num_cartels))
mean_duration_diffs <- mean_duration_diffs %>%
  mutate(diff_mean_dur = mean_duration_0 - mean_duration_1)
## ==>> next: add diff num_cartels



# Analyze Correlations --------------------------------------------------------------


cor(pop_duration_struc$n_firms, pop_duration_struc$duration)
cor(pop_duration_unstruc$n_firms, pop_duration_unstruc$duration)

cor(pop_duration_struc$rho_start, pop_duration_struc$duration)
cor(pop_duration_unstruc$rho_start, pop_duration_unstruc$duration)


# Contingency Table
table(sample_duration_struc$n_firms, sample_duration_struc$rho_start)
chisq.test(sample_duration_struc$n_firms, sample_duration_struc$rho_start, simulate.p.value=TRUE)

fisher.test(sample_duration_struc$n_firms, sample_duration_struc$rho_start)


table(pop_duration_struc$n_firms, pop_duration_struc$duration_quant)
chisq.test(pop_duration_struc$n_firms, pop_duration_struc$duration_quant)

table(pop_duration_struc$duration_quant, pop_duration_struc$detected)
chisq.test(pop_duration_struc$duration_quant, pop_duration_struc$detected)

table(pop_duration_unstruc$duration_quant, pop_duration_unstruc$detected)
chisq.test(pop_duration_unstruc$duration_quant, pop_duration_unstruc$detected)


# Chi Square Test duration versus n firms

table(pop_duration_unstruc$n_firms, pop_duration_unstruc$duration_quant, pop_duration_unstruc$detected)
t_pop_unstruc <- table(pop_duration_unstruc$n_firms, pop_duration_unstruc$duration_quant)
chisq.test(pop_duration_unstruc$n_firms, pop_duration_unstruc$duration_quant)
chisq.test(t_pop_unstruc, simulate.p.value = TRUE)

table(sample_duration_unstruc$n_firms, sample_duration_unstruc$duration_quant)
t_sample_unstruc <- table(sample_duration_unstruc$n_firms, sample_duration_unstruc$duration_quant)
chisq.test(t_sample_unstruc[1:4,])
chisq.test(t_sample_unstruc, simulate.p.value = TRUE)
chisq.test(t_sample_unstruc)


table(undetected_duration_unstruc$n_firms, undetected_duration_unstruc$duration_quant)
t_undetected_unstruc <- table(undetected_duration_unstruc$n_firms, undetected_duration_unstruc$duration_quant)
chisq.test(t_undetected_unstruc, simulate.p.value = TRUE)

table(sample_duration_struc$n_firms, sample_duration_struc$duration_quant)
t_sample_struc <- table(sample_duration_struc$n_firms, sample_duration_struc$duration_quant)
chisq.test(t_sample_struc, simulate.p.value = TRUE)

table(undetected_duration_struc$n_firms, undetected_duration_struc$duration_quant)
t_undetected_struc <- table(undetected_duration_struc$n_firms, undetected_duration_struc$duration_quant)
chisq.test(t_undetected_struc, simulate.p.value = TRUE)

ggplot(sample_duration_struc, aes(x=n_firms_factor, y=duration_quant)) +
  geom_mosaic()

ggplot(data = sample_duration_struc) +
  geom_mosaic(aes(x = product(n_firms_factor, duration_quant), fill=n_firms_factor)) + 
  labs(title='f(n firms)') +
  theme_mosaic()
  

mosaicplot(~n_firms_factor + duration_quant, data=pop_duration_struc, main="Number of Firms versus Duration, Population with Changing Detection Probability", col=TRUE)
mosaicplot(~n_firms_factor + duration_quant, data=pop_duration_unstruc, main="Number of Firms versus Duration, Population with Constant Detection Probability", col=TRUE)

mosaicplot(~n_firms_factor + duration_quant, data=sample_duration_struc, main="Number of Firms versus Duration, Sample with Changing Detection Probability", col=TRUE)
mosaicplot(~n_firms_factor + duration_quant, data=sample_duration_unstruc, main="Number of Firms versus Duration, Sample with Constant Detection Probability", col=TRUE)

mosaicplot(~n_firms_factor + duration_quant, data=undetected_duration_struc, main="Number of Firms versus Duration, Undetected with Changing Detection Probability", col=TRUE)
mosaicplot(~n_firms_factor + duration_quant, data=undetected_duration_unstruc, main="Number of Firms versus Duration, Undetected with Constant Detection Probability", col=TRUE)


# Plotting --------------------------------------------------------------

plot_mean_dur_n_rho(pop_duration_struc, "pop_struc")
plot_mean_dur_n_rho(pop_duration_unstruc, "pop_unstruc")



ggplot(data_struc, aes(x = n_firms, y = rho_start, size = mean_duration, color = mean_num_cartels)) +
  geom_point() +
  xlab("number of firms") +
  ylab("detection probability") +
  facet_grid(. ~ detected)


ggplot(data_unstruc_plot, aes(x = n_firms, y = rho_start, size = mean_duration, color = detection)) +
  geom_jitter(width = 0.05, height = 0, shape = 0) +
  xlab("number of firms") +
  ylab("detection probability")

ggplot(data_struc_plot, aes(x = x_value, y = rho_start, size = mean_duration, color = detection)) +
  geom_point(shape = 0) +
  xlab("number of firms") +
ylab("detection probability")



## Boxplots

title <- "Duration depending on n firms, population with constant detection probability"
ggplot(pop_duration_unstruc, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw() 

title <- "Duration depending on n firms, population with changing detection probability"
ggplot(pop_duration_struc, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw()

title <- "Duration depending on rho_start, sample with constant detection probability"
ggplot(sample_duration_unstruc, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability") +
  ylab("duration") +
  theme_bw()

title <- "Duration depending on rho_start, population with changing detection probability"
ggplot(pop_duration_struc, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability at beginning") +
  ylab("duration") +
  theme_bw()


title <- "Duration depending on n firms, sample with constant detection probability"
ggplot(sample_duration_unstruc, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw()

title <- "Duration depending on n firms, sample with changing detection probability"
ggplot(sample_duration_struc, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw()

title <- "Duration depending on rho_start, sample with constant detection probability"
ggplot(sample_duration_unstruc, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability") +
  ylab("duration") +
  theme_bw()

title <- "Duration depending on rho_start, sample with changing detection probability"
ggplot(sample_duration_struc, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability at beginning") +
  ylab("duration") +
  theme_bw()


# Facets mit jeweils zwei Boxplots (detected, undetected) nebeneinander
title <- "Duration depending on n firms, undetected (0) and detected (1) cartels with constant detection probability"
ggplot(pop_duration_unstruc, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw() +
  facet_grid(. ~ detected)


title <- "Duration depending on n firms, undetected (0) and detected (1) cartels with changing detection probability"
ggplot(pop_duration_struc, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw() +
  facet_grid(. ~ detected)

title <- "Duration depending on rho_start, undetected (0) and detected (1) cartels with constant detection probability"
ggplot(pop_duration_unstruc, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability") +
  ylab("duration") +
  theme_bw() +
  facet_grid(. ~ detected)

title <- "Duration depending on rho_start, undetected (0) and detected (1) cartels with changing detection probability"
ggplot(pop_duration_struc, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability at beginning") +
  ylab("duration") +
  theme_bw() +
  facet_grid(. ~ detected)






# todo: add B&E, Hazard
