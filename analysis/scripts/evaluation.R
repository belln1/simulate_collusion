source(file = "analysis/scripts/set_up.R")

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




dotplot_mean_duration <- function(data, filename){
  ggplot(data, aes(x = n_firms, y = rho_start, size = mean_duration)) +
    geom_point() +
    xlab("number of firms") +
    ylab("detection probability") +
    labs(size = "mean duration of cartels")
  ggsave(filename)
}





# Read and prepare data --------------------------------------------------------------
filename <- paste("analysis/data/duration_enforce_", struc, "struc_", theta, "theta.csv", sep = "")

pop_duration <- read.table(filename, header = TRUE, sep = ";")
pop_duration$n_firms_factor <- factor(pop_duration$n_firms)
pop_duration$rho_start_factor <- factor(pop_duration$rho_start)
pop_duration$duration_quant <-  quantcut(pop_duration$duration, q = 4)

sample_duration <- filter(pop_duration, detected==1)
undetected_duration <- filter(pop_duration, detected==0)

data <- get_mean_duration_by_detection(pop_duration)

# Prepare data for plotting

data_plot <- data
data_plot$detection <- ifelse(data_plot$detected==0, "undetected", "detected")
data_plot$x_value <- ifelse(data_plot$detected==0, data_plot$n_firms+0.05, data_plot$n_firms-0.05)


mean_duration_diffs <- pivot_wider(data, names_from = detected,
                                   values_from = c(mean_duration, mean_num_cartels, num_cartels))
mean_duration_diffs <- mean_duration_diffs %>%
  mutate(diff_mean_dur = mean_duration_0 - mean_duration_1) %>%
  mutate(diff_mean_num_cartels = mean_num_cartels_0 - mean_num_cartels_1)
  


# Workflow  --------------------------------------------------------------

# Correlation

cor(pop_duration$n_firms, pop_duration$duration)
cor(pop_duration$rho_start, pop_duration$duration)

# Chi Square Tests n firms versus rho

table(sample_duration$n_firms, sample_duration$rho_start)
chisq.test(sample_duration$n_firms, sample_duration$rho_start, simulate.p.value=TRUE)
# fisher.test(sample_duration$n_firms, sample_duration$rho_start) # too large

# Chi Square Test duration versus detection (sample/undetected)

table(pop_duration$duration_quant, pop_duration$detected)
chisq.test(pop_duration$duration_quant, pop_duration$detected)


# Chi Square Test duration versus n firms

#table(pop_duration$n_firms, pop_duration$duration_quant, pop_duration$detected)
t_pop <- table(pop_duration$n_firms, pop_duration$duration_quant)
chisq.test(pop_duration$n_firms, pop_duration$duration_quant)
chisq.test(t_pop, simulate.p.value = TRUE)

table(sample_duration$n_firms, sample_duration$duration_quant)
t_sample <- table(sample_duration$n_firms, sample_duration$duration_quant)
chisq.test(t_sample[1:4,])
chisq.test(t_sample, simulate.p.value = TRUE)
chisq.test(t_sample)

table(undetected_duration$n_firms, undetected_duration$duration_quant)
t_undetected <- table(undetected_duration$n_firms, undetected_duration$duration_quant)
chisq.test(t_undetected, simulate.p.value = TRUE)


# Mosaic Plots
path <- "analysis/figures/mosaicplots/"

png(paste(path, "enforce_mosaic_n-firms_duration_pop_", name, ".png", sep = ""))
mosaicplot(~n_firms_factor + duration_quant, data=pop_duration, main="Number of Firms versus Duration, Population", col=TRUE, xlab="number of firms", ylab="duration")
dev.off()

png(paste(path, "enforce_mosaic_n-firms_duration_sample_", name, ".png", sep = ""))
mosaicplot(~n_firms_factor + duration_quant, data=sample_duration, main="Number of Firms versus Duration, Sample", col=TRUE, xlab="number of firms", ylab="duration")
dev.off()

png(paste(path, "enforce_mosaic_n-firms_duration_undetected_", name, ".png", sep = ""))
mosaicplot(~n_firms_factor + duration_quant, data=undetected_duration, main="Number of Firms versus Duration, Undetected", col=TRUE, xlab="number of firms", ylab="duration")
dev.off()




# 3-dim Dot Plots
path <- "analysis/figures/dotplots/"

dotplot_mean_duration(get_mean_duration_all(pop_duration), paste(path, "enforce_dotplot_mean_duration_pop_", name, ".png", sep = ""))
dotplot_mean_duration(get_mean_duration_all(sample_duration), paste(path, "enforce_dotplot_mean_duration_sample_", name, ".png", sep = ""))
dotplot_mean_duration(get_mean_duration_all(undetected_duration), paste(path, "enforce_dotplot_mean_duration_undetected_", name, ".png", sep = ""))





# 4 dimensions (better not)

ggplot(data, aes(x = n_firms, y = rho_start, size = mean_duration, color = mean_num_cartels)) +
  geom_point() +
  xlab("number of firms") +
  ylab("detection probability") +
  facet_grid(. ~ detected)


ggplot(data_plot, aes(x = n_firms, y = rho_start, size = mean_duration, color = detection)) +
  geom_jitter(width = 0.05, height = 0, shape = 0) +
  xlab("number of firms") +
  ylab("detection probability")

ggplot(data_plot, aes(x = x_value, y = rho_start, size = mean_duration, color = detection)) +
  geom_point(shape = 0) +
  xlab("number of firms") +
ylab("detection probability")



## Boxplots

path <- "analysis/figures/boxplots/"

title <- "Duration depending on n firms, population"
ggplot(pop_duration, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw()
ggsave(paste(path, "enforce_boxplot_n-firms_duration_pop_", name, "_.png", sep = ""))

title <- "Duration depending on rho_start, population"
ggplot(pop_duration, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability at beginning") +
  ylab("duration") +
  theme_bw()
ggsave(paste(path, "enforce_boxplot_rho_duration_pop_", name, "_.png", sep = ""))


title <- "Duration depending on n firms, sample"
ggplot(sample_duration, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw()
ggsave(paste(path, "enforce_boxplot_n-firms_duration_sample_", name, "_.png", sep = ""))


title <- "Duration depending on rho_start, sample"
ggplot(sample_duration, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability at beginning") +
  ylab("duration") +
  theme_bw()
ggsave(paste(path, "enforce_boxplot_rho_duration_sample_", name, "_.png", sep = ""))


# Facets mit jeweils zwei Boxplots (detected, undetected) nebeneinander
title <- "Duration depending on n firms, undetected (0) and detected (1) cartels"
ggplot(pop_duration, aes(x = n_firms_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("number of firms") +
  ylab("duration") +
  theme_bw() +
  facet_grid(. ~ detected)
ggsave(paste(path, "enforce_boxplot_n-firms_duration_undet_detect_", name, "_.png", sep = ""))



title <- "Duration depending on rho_start, undetected (0) and detected (1) cartels"
ggplot(pop_duration, aes(x = rho_start_factor, y = duration)) +
  geom_boxplot() +
  ggtitle(title) +
  xlab("detection probability") +
  ylab("duration") +
  theme_bw() +
  facet_grid(. ~ detected)
ggsave(paste(path, "enforce_boxplot_rho_duration_undet_detect_", name, "_.png", sep = ""))






# todo: add B&E, Hazard
