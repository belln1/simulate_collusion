source(file = "analysis/scripts/functions_simulation.R")
library(tframePlus)
library(tidyverse)

# shared legend (plot_layout(guides = "collect"))
library("patchwork")


# take only relevant rows, delete columns that are no years
prepare_firms <- function(firms){
  firms <- firms[,1:1000]
  firms <- firms %>% 
    filter(rowSums(firms)>0)
  firms <- t(data.matrix(firms))
  get_endtimes(firms)
}

# Annualize data: Summarize every 1:12 cols to get years
annualize <- function(firms) {
  firms_ts <- ts(firms, start = c(1964,1), frequency = 12)
  firms_annual <- as.annually(firms_ts, FUN = max)
  firms_annual[84,] <- colSums(firms_ts[996:1000,]) # last 4 columns do not fit into year, just take the max value
  firms_annual[84,] <- ifelse(firms_annual[84,]>0, 1, 0)
  firms_cr_annual <- data.frame(t(firms_annual))
}

# prepare dataformat for MARK
prepare_cr_input <- function(firms) {
  df <- firms %>% 
    unite("z", 1:ncol(firms), sep = '', remove = TRUE)
  df$a <- 1
  df <- df %>% unite("b", z:a, sep = '\t', remove = TRUE)
}

#####################################################################################
# SEED_2

firms <- read_csv2("analysis/data/seeds_2/firms/firms_detected/firms_detected.csv", col_names = TRUE, col_types = "n")
file_out_0struc_small <- "analysis/data/seeds_2/capture_recapture/input_cr/det_0struc_small.inp"
file_out_0struc_large <- "analysis/data/seeds_2/capture_recapture/input_cr/det_0struc_large.inp"
file_out_1struc_small <- "analysis/data/seeds_2/capture_recapture/input_cr/det_1struc_small.inp"
file_out_1struc_large <- "analysis/data/seeds_2/capture_recapture/input_cr/det_1struc_large.inp"

firms_0struc_small <- filter(firms, firms$n_firms <= 3 & firms$structured==0)
firms_0struc_large <- filter(firms, firms$n_firms > 3 & firms$structured==0)
firms_1struc_small <- filter(firms, firms$n_firms <= 3 & firms$structured==1)
firms_1struc_large <- filter(firms, firms$n_firms > 3 & firms$structured==1)

#firms_cr <- prepare_firms(firms_0struc_small) # 77.930 firms
#firms_cr <- prepare_firms(firms_0struc_large) #106.671 firms
#firms_cr <- prepare_firms(firms_1struc_small) # 78.244 firms
firms_cr <- prepare_firms(firms_1struc_large)  #106.686 firms
firms_cr_annual <- annualize(firms_cr)
df_cr <- prepare_cr_input(firms_cr_annual)
#write.table(df_cr, file = file_out_0struc_small, row.names = FALSE, col.names = FALSE, sep = "", eol = ";\n", quote = FALSE)
#write.table(df_cr, file = file_out_0struc_large, row.names = FALSE, col.names = FALSE, sep = "", eol = ";\n", quote = FALSE)
#write.table(df_cr, file = file_out_1struc_small, row.names = FALSE, col.names = FALSE, sep = "", eol = ";\n", quote = FALSE)
write.table(df_cr, file = file_out_1struc_large, row.names = FALSE, col.names = FALSE, sep = "", eol = ";\n", quote = FALSE)


# SOFTWARE MARK: estimate phi(./.)p(t)
# http://www.phidot.org/software/mark/index.html


get_rho_increment <- function(rho_firms) {
  rho_pop <- rho_firms[,1:1000]
  rho_avg <- colMeans(rho_pop)
  rho_ts <- ts(rho_avg, start = c(1964,1), frequency = 12)
  rho_annual <- as.annually(rho_ts, FUN = max)
  rho_annual <- ts(rho_annual[1:83,1]) # last 4 columns do not fit into year, just take the last value
  pop_rho_incr <- 20*rho_annual/(allperiods/timefactor)
}

get_rho_constant <- function(rho_firms) {
  pop_rho_constant <- mean(rho_firms$rho_start)
  20*pop_rho_constant/(allperiods/timefactor)
}

name <- "seed2_det_0struc_small"
file_in <- paste("analysis/data/seeds_2/capture_recapture/output_cr/", name, ".txt", sep = "")
colnames <- c("Parameter", "Estimate", "Standard_Error", "CI_Lower", "CI_Upper")
estimates <- read.table(file = file_in, header = FALSE, skip = 7, sep = "", col.names = colnames)
estimates_detection_0struc_small <- estimates[3:nrow(estimates),]
mav_0struc_small <- stats::filter(estimates_detection_0struc_small$Estimate, sides=2, filter=rep(1/3, 3))

name <- "seed2_det_0struc_large"
file_in <- paste("analysis/data/seeds_2/capture_recapture/output_cr/", name, ".txt", sep = "")
colnames <- c("Parameter", "Estimate", "Standard_Error", "CI_Lower", "CI_Upper")
estimates <- read.table(file = file_in, header = FALSE, skip = 7, sep = "", col.names = colnames)
estimates_detection_0struc_large <- estimates[3:nrow(estimates),]
mav_0struc_large <- stats::filter(estimates_detection_0struc_large$Estimate, sides=2, filter=rep(1/3, 3))

name <- "seed2_det_1struc_small"
file_in <- paste("analysis/data/seeds_2/capture_recapture/output_cr/", name, ".txt", sep = "")
colnames <- c("Parameter", "Estimate", "Standard_Error", "CI_Lower", "CI_Upper")
estimates <- read.table(file = file_in, header = FALSE, skip = 7, sep = "", col.names = colnames)
estimates_detection_1struc_small <- estimates[3:nrow(estimates),]
mav_1struc_small <- stats::filter(estimates_detection_1struc_small$Estimate, sides=2, filter=rep(1/3, 3))

name <- "seed2_det_1struc_large"
file_in <- paste("analysis/data/seeds_2/capture_recapture/output_cr/", name, ".txt", sep = "")
colnames <- c("Parameter", "Estimate", "Standard_Error", "CI_Lower", "CI_Upper")
estimates <- read.table(file = file_in, header = FALSE, skip = 7, sep = "", col.names = colnames)
estimates_detection_1struc_large <- estimates[3:nrow(estimates),]
mav_1struc_large <- stats::filter(estimates_detection_1struc_large$Estimate, sides=2, filter=rep(1/3, 3))

rho_firms <- read.table("analysis/data/seeds_2/firms/rho_firms/rho_firms_new.csv", header = TRUE, dec = ".", row.names = NULL, sep = ";")
rho_firms$X1 <- as.numeric(rho_firms$X1)
headers <- which(is.na(rho_firms$X1)) #Delete too many headers
x <- rho_firms[-headers,]
write.table(x, "analysis/data/seeds_2/firms/rho_firms/rho_firms_no_headers.csv", row.names = FALSE, sep = ";", quote = FALSE)
rho_firms <- read.table("analysis/data/seeds_2/firms/rho_firms/rho_firms_no_headers.csv", header = TRUE, dec = ".", row.names = NULL, sep = ";", colClasses = "numeric")


rho_0struc_small <- filter(rho_firms, rho_firms$n_firms <= 3 & rho_firms$structured==0)
rho_constant_0struc_small <- get_rho_constant(rho_0struc_small)
rho_0struc_large <- filter(rho_firms, rho_firms$n_firms > 3 & rho_firms$structured==0)
rho_constant_0struc_large <- get_rho_constant(rho_0struc_large)

rho_1struc_small <- filter(rho_firms, rho_firms$n_firms <= 3 & rho_firms$structured==1)
rho_constant_1struc_small <- get_rho_constant(rho_1struc_small)
rho_increasing_1struc_small <- get_rho_increment(rho_1struc_small)
rho_1struc_large <- filter(rho_firms, rho_firms$n_firms > 3 & rho_firms$structured==1)
rho_constant_1struc_large <- get_rho_constant(rho_1struc_large)
rho_increasing_1struc_large <- get_rho_increment(rho_1struc_large)

p_detection_0struc_small <- cbind(estimates_detection_0struc_small$Estimate, mav_0struc_small, rho_constant_0struc_small)
colnames(p_detection_0struc_small) <- c("CR detection prob", "MAV CR detection prob", "AVG pop constant detection prob")
plot_0struc_small <- autoplot(p_detection_0struc_small) +
  ylim(0, 0.12) +
  xlab("Time") +
  ylab("Probability of detection") 

p_detection_0struc_large <- cbind(estimates_detection_0struc_large$Estimate, mav_0struc_large, rho_constant_0struc_large)
colnames(p_detection_0struc_large) <- c("CR detection prob", "MAV CR detection prob", "AVG pop constant detection prob")
plot_0struc_large <- autoplot(p_detection_0struc_large) +
  ylim(0, 0.12) +
  xlab("Time") +
  ylab("Probability of detection") 

plot_0struc_small + plot_0struc_large + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank())
ggsave("analysis/figures/capture_recapture/prob_detection_0struc.png", 
       width=85*(14/5), height = 53*(14/5), units = "mm")


p_detection_1struc_small <- cbind(estimates_detection_1struc_small$Estimate, mav_1struc_small, rho_constant_1struc_small, rho_increasing_1struc_small)
colnames(p_detection_1struc_small) <- c("CR detection prob", "MAV CR detection prob", "AVG pop constant detection prob", "AVG pop increasing detection prob")
plot_1struc_small <- autoplot(p_detection_1struc_small) +
  ylim(0, 0.12) +
  xlab("Time") +
  ylab("Probability of detection") +
  labs(caption = "a) Model IIIa,b (Small Number of Firms)") +
  theme(plot.caption = element_text(hjust = 0.5, size = 13))# move caption to the middle
        

p_detection_1struc_large <- cbind(estimates_detection_1struc_large$Estimate, mav_1struc_large, rho_constant_1struc_large, rho_increasing_1struc_large)
colnames(p_detection_1struc_large) <- c("CR detection prob", "MAV CR detection prob", "AVG pop constant detection prob", "AVG pop increasing detection prob")
plot_1struc_large <- autoplot(p_detection_1struc_large) +
  ylim(0, 0.12) +
  xlab("Time") +
  ylab("Probability of detection") +
  labs(caption = "a) Model IIIa,b (Large Number of Firms)") +
  theme(plot.caption = element_text(hjust = 0.5, size = 13))# move caption to the middle

plot_1struc_small + plot_1struc_large + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank())
ggsave("analysis/figures/capture_recapture/prob_detection_1struc.png", 
       width=85*(14/5), height = 53*(14/5), units = "mm")


