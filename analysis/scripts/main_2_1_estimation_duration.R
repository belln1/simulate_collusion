source(file = "analysis/scripts/functions_simulation.R")

# set basic parameters
timefactor <- 12
allperiods <- 1000
periodsNoLen <- allperiods/2
periodsLen <- allperiods/2



# Calculate cartel durations
status_all <- c("population", "detected", "undetected")

seeds_dir <- "seed_1673465635_2ind"

cartels_pop <- readRDS(file = paste("analysis/data/", seeds_dir, "/cartels/cartels_", status_all[1], ".rds", sep = ""))
cartels_detected <- readRDS(file = paste("analysis/data/", seeds_dir, "/cartels/cartels_", status_all[2], ".rds", sep = ""))
cartels_undetected <- readRDS(file = paste("analysis/data/", seeds_dir, "/cartels/cartels_", status_all[3], ".rds", sep = ""))

parms <- read.table(file = paste("analysis/data/", seeds_dir, "/parms.csv", sep = ""), header = TRUE, sep = ";")


# Calculate cartel durations
cartels_detected_duration <- get_enforcement_duration(cartels_detected, parms) 
cartels_detected_duration$detected <- 1
cartels_undetected_duration <- get_enforcement_duration(cartels_undetected, parms) 
cartels_undetected_duration$detected <- 0
cartels_duration <- rbind(cartels_detected_duration, cartels_undetected_duration)

cartels_duration$model_2a <- if_else(cartels_duration$structured==0 & cartels_duration$theta_len==1, 1, 0)
cartels_duration$model_2b <- if_else(cartels_duration$structured==0 & cartels_duration$theta_len<1, 1, 0)
cartels_duration$model_3a <- if_else(cartels_duration$structured==1 & cartels_duration$theta_len==1, 1, 0)
cartels_duration$model_3b <- if_else(cartels_duration$structured==1 & cartels_duration$theta_len<1, 1, 0)
cartels_duration$theta <- if_else(cartels_duration$end <= periodsNoLen, 1.0, cartels_duration$theta_len) # leniency only starts after 500 time periods
cartels_duration$leniency_periods <- if_else(cartels_duration$end <= periodsNoLen, 0, 1)

write.table(cartels_duration, file = paste("analysis/data/", seeds_dir, "/duration_enforcement.csv", sep = ""), row.names = FALSE, sep = ";")
cartels_duration <- read.table(file = paste("analysis/data/", seeds_dir, "/duration_enforcement.csv", sep = ""), header = TRUE, sep = ";")


# SUMMARY STATISTICS
sumstats <- describe(cartels_duration, fast = FALSE)
sumstats <- select(sumstats, mean, median, sd, min, max, skew, n)
sumstats$mean <- round(sumstats$mean,2)
sumstats$sd <- round(sumstats$sd,2)
sumstats$skew <- round(sumstats$skew,2)
k <- kbl(sumstats, "latex", booktabs = T, linesep = "")
save_kable(k, file = paste("analysis/data/seeds_2/sumstats_cartels_enforcement_3.tex", sep = ""))


cartels_duration_sample <- filter(cartels_duration, detected==1)
sumstats <- describe(cartels_duration_sample)
sumstats <- select(sumstats, mean, median, sd, min, max, skew, n)
sumstats$mean <- round(sumstats$mean,2)
sumstats$sd <- round(sumstats$sd,2)
sumstats$skew <- round(sumstats$skew,2)
k <- kbl(sumstats, "latex", booktabs = T, linesep = "")
save_kable(k, file = paste("analysis/data/seeds_2/sumstats_cartels_enforcement_sample_3.tex", sep = ""))


# PERCENTAGE OF MODELS
# model 2a: constant rho (structured=0), no leniency (theta=1)
# model 2b: constant rho (structured=0), leniency (theta=0 or 0.5)
# model 3a: increasing rho (structured=0), no leniency (theta=1)
# model 3b: increasing rho (structured=0), leniency (theta=0 or 0.5)
cartels_model2a <- filter(cartels_duration_sample, structured==0, theta_len==1)
cartels_model2b <- filter(cartels_duration_sample, structured==0, theta_len<1)
cartels_model3a <- filter(cartels_duration_sample, structured==1, theta_len==1)
cartels_model3b <- filter(cartels_duration_sample, structured==1, theta_len<1)

percentage_2a <- nrow(cartels_model2a)/nrow(cartels_duration_sample)
percentage_2b <- nrow(cartels_model2b)/nrow(cartels_duration_sample)
percentage_3a <- nrow(cartels_model3a)/nrow(cartels_duration_sample)
percentage_3b <- nrow(cartels_model3b)/nrow(cartels_duration_sample)



###--------------------------------------------------------------------------------------------------------------------------

# MEAN DURATION PER N_FIRMS
get_mean_duration_firms <- function(duration_cartels) {
  duration_cartels %>%
    group_by(structured, n_firms) %>%
    summarise(mean_duration = mean(duration),
          #    num_cartels = n(),
              mean_num_cartels = sum(duration)/allperiods) %>%
    arrange(structured, n_firms)
}
mean_duration <- get_mean_duration_firms(cartels_duration)
mean_duration <- select(mean_duration, structured, n_firms, mean_num_cartels, mean_duration)
k <- kbl(mean_duration, "latex", booktabs = T, linesep = "")
save_kable(k, file = "mean_duration_enforcement.tex")

k <- kbl(parms, "latex", booktabs = T, linesep = "")
save_kable(k, file = "analysis/data/seeds_2/parms_no_enforcement.tex")


###--------------------------------------------------------------------------------------------------------------------------

# Hazard rate estimation following Levenstein/Suslow: see main_3_1_hazard.do

###--------------------------------------------------------------------------------------------------------------------------
###--------------------------------------------------------------------------------------------------------------------------


# Bryan and Eckard duration estimation
calc_bryan_eckard <- function(cartels, space, model) {
  tibble(
    model = model,
    space = space,
    theta_min_1 = allperiods/length(cartels[,1]),  # theta^-1 = avg interarrival time
    theta = 1/theta_min_1,
    lambda_min_1 = mean(cartels$duration),
    lambda = 1/lambda_min_1,
    number_alive = round(theta/lambda),
    size = length(cartels[,1]))
}

path <- "analysis/data/seeds_2/"
duration_model_1 <- read.table(paste(path, "duration_no_enforcement.csv", sep = ""), header = TRUE, sep = ";")
b_and_e <- calc_bryan_eckard(duration_model_1, "population", 1)

cartels_duration <- read.table(paste(path, "duration_enforcement.csv", sep = ""), header = TRUE, sep = ";")

b_and_e[2,] <- calc_bryan_eckard(filter(cartels_duration, structured == 0), "population", 2)
b_and_e[3,] <- calc_bryan_eckard(filter(cartels_duration, structured == 0, detected == 1), "sample", 2)
b_and_e[4,] <- calc_bryan_eckard(filter(cartels_duration, structured == 0, detected == 0), "undetected", 2)

b_and_e[5,] <- calc_bryan_eckard(filter(cartels_duration, structured == 1), "population", 3)
b_and_e[6,] <- calc_bryan_eckard(filter(cartels_duration, structured == 1, detected == 1), "sample", 3)
b_and_e[7,] <- calc_bryan_eckard(filter(cartels_duration, structured == 1, detected == 0), "undetected", 3)

b_and_e[3:5] <- round(b_and_e[3:5], 2)
b_and_e$lambda <- round(b_and_e$lambda, 3)

write.table(b_and_e, file = paste(path, "estimations/b_and_e.csv", sep = ""), row.names = FALSE, sep = ";")
k <- kbl(b_and_e, "latex", booktabs = T, linesep = "")
save_kable(k, file = paste(path, "estimations/b_and_e.tex", sep = ""))


# Bryan and Eckard duration estimation for small n and rho
describe(filter(cartels_duration, structured==0))
describe(filter(cartels_duration, structured==0, detected==1, rho_start <= 0.2, n_firms <= 3)) # 208.34  223.09
describe(filter(cartels_duration, structured==0, detected==1, rho_start > 0.2, n_firms>3)) # 132.59  114.63
describe(filter(cartels_duration, structured==0, detected==0, rho_start <= 0.2, n_firms <= 3)) # 127.12   320.62
describe(filter(cartels_duration, structured==0, detected==0, rho_start > 0.2, n_firms > 3)) # 84.18   50.73

describe(filter(cartels_duration, structured==1, detected==1, rho_start <= 0.2, n_firms <= 3)) # 157.60
describe(filter(cartels_duration, structured==1, detected==1, rho_start > 0.2, n_firms>3)) # 114.95
describe(filter(cartels_duration, structured==1, detected==0, rho_start <= 0.2, n_firms <= 3)) # 167.57
describe(filter(cartels_duration, structured==1, detected==0, rho_start > 0.2, n_firms > 3)) # 42.18

describe(filter(duration_model_1, n_firms <= 3)) # 1000.0
describe(filter(duration_model_1, n_firms > 3)) # 116.16
describe(filter(cartels_duration, structured==0, n_firms <= 3)) # 181.92
describe(filter(cartels_duration, structured==0, n_firms>3)) # 104.00
describe(filter(cartels_duration, structured==0, detected==1, n_firms <= 3)) # 167.70
describe(filter(cartels_duration, structured==0, detected==1, n_firms>3)) # 154.86
describe(filter(cartels_duration, structured==0, detected==0, n_firms <= 3)) # 242.69
describe(filter(cartels_duration, structured==0, detected==0, n_firms > 3)) # 72.19

describe(filter(cartels_duration, structured==1, n_firms <= 3)) # 123.92
describe(filter(cartels_duration, structured==1, n_firms>3)) # 91.90
describe(filter(cartels_duration, structured==1, detected==1, n_firms <= 3)) # 126.66
describe(filter(cartels_duration, structured==1, detected==1, n_firms>3)) # 153.39
describe(filter(cartels_duration, structured==1, detected==0, n_firms <= 3)) # 111.58
describe(filter(cartels_duration, structured==1, detected==0, n_firms > 3)) # 56.12

b_and_e_sep <- calc_bryan_eckard(filter(duration_model_1, n_firms <= 3), "population, small n", 1)
b_and_e_sep[2,] <- calc_bryan_eckard(filter(duration_model_1, n_firms > 3), "population, large n", 1)

b_and_e_sep[3,] <- calc_bryan_eckard(filter(cartels_duration, structured==0, detected==1, n_firms <= 3), "sample, small n", 2)
b_and_e_sep[4,] <- calc_bryan_eckard(filter(cartels_duration, structured==0, detected==1, n_firms > 3), "sample, large n", 2)

b_and_e_sep[5,] <- calc_bryan_eckard(filter(cartels_duration, structured==0, detected==0, n_firms <= 3), "undetected, small n", 2)
b_and_e_sep[6,] <- calc_bryan_eckard(filter(cartels_duration, structured==0, detected==0, n_firms > 3), "undetected, large n", 2)

b_and_e_sep[7,] <- calc_bryan_eckard(filter(cartels_duration, structured==0, n_firms <= 3), "population, small n", 2)
b_and_e_sep[8,] <- calc_bryan_eckard(filter(cartels_duration, structured==0, n_firms > 3), "population, large n", 2)

b_and_e_sep[9,] <- calc_bryan_eckard(filter(cartels_duration, structured==1, detected==1, n_firms <= 3), "sample, small n", 3)
b_and_e_sep[10,] <- calc_bryan_eckard(filter(cartels_duration, structured==1, detected==1, n_firms > 3), "sample, large n", 3)

b_and_e_sep[11,] <- calc_bryan_eckard(filter(cartels_duration, structured==1, detected==0, n_firms <= 3), "undetected, small n", 3)
b_and_e_sep[12,] <- calc_bryan_eckard(filter(cartels_duration, structured==1, detected==0, n_firms > 3), "undetected, large n", 3)

b_and_e_sep[13,] <- calc_bryan_eckard(filter(cartels_duration, structured==1, n_firms <= 3), "population, small n", 3)
b_and_e_sep[14,] <- calc_bryan_eckard(filter(cartels_duration, structured==1, n_firms > 3), "population, large n", 3)

b_and_e_sep[3:5] <- round(b_and_e_sep[3:5], 2)
b_and_e_sep$lambda <- round(b_and_e_sep$lambda, 3)

write.table(b_and_e_sep, file = paste(path, "estimations/b_and_e_sep.csv", sep = ""), row.names = FALSE, sep = ";")
k <- kbl(b_and_e_sep, "latex", booktabs = T, linesep = "")
save_kable(k, file = paste(path, "estimations/b_and_e_sep.tex", sep = ""))

