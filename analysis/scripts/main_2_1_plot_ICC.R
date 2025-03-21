rm(list = ls())
source(file = "analysis/scripts/functions_simulation.R")

library(extrafont)
font_import() # only do this one time - it takes a while
loadfonts(device = "win")
loadfonts(device = "postscript")  # for saving as .eps or .png, together with device=cairo_ps (see below)
#postscriptFonts()

# PLOT: Discount Factors and ICC for Different Models
allperiods <- 1000
periodsNoLen <- 0 # thetas remain constant for all time periods
periodsLen <- allperiods
timefactor <- 12
#r_1 <- 0.03 #interest rate (Version 1, 2)

#min_share <- 0.8 # minimum percentage of firms needed to form a cartel  (Version 1)
min_share <- 1 # minimum percentage of firms needed to form a cartel

n_firms <- 4

gamma <- 0.9
theta <- 1
i <- 1
k <- 1
n_industries <- 300

seed_start <- 123
r_min <- 0.001
r_max <- 0.1

#################

ICC_no_enforc <- get_ICC_model1(n_firms)

# ----------------
# III) Increasing sigma, Plot with different ICC for leniency = 0 and 1, Plot ICCs for firm 1
n_cartels <- 5
sigma_all <- 0.2
sigma_t <- 1 - (1-sigma_all)^(1/200) # 200 is an approximation of mean duration, in simulation run with sigma=0
parms <- tibble(
  n_firms = n_firms,
  sigma_start = sigma_t,
  theta = 1,
  structured = 1,
  gamma = gamma
)
parms <- parms %>%
  arrange(n_firms, sigma_start, theta, structured)

deltas0 <- matrix(0, nrow=allperiods, ncol = n_cartels)
deltas1 <- matrix(0, nrow=allperiods, ncol = n_cartels)
icc0 <- matrix(0, nrow=allperiods, ncol = n_cartels)
icc1 <- matrix(0, nrow=allperiods, ncol = n_cartels)
seed_start <- 321
count <- k*1000000+i*1000+ seed_start
for (j in 0:5) {
  seed <- count+j
  parms$theta <- 1
  sim_list_1theta <- simulate_industry_model3(1, parms, 1, seed, min_share)
  parms$theta <- 0
  sim_list_0theta <- simulate_industry_model3(1, parms, 1, seed, min_share)
  deltas1[,j] <- sim_list_1theta$deltas
  deltas0[,j] <- sim_list_0theta$deltas
  icc0[,j] <- sim_list_0theta$ICC_entry
  icc1[,j] <- sim_list_1theta$ICC_entry
}
iccm1 <- rep(ICC_no_enforc, allperiods)
df <- cbind(iccm1, icc1[,4], icc0[,4], deltas0)
df <- data.frame(df)
sim <- ts(df)
colnames(sim) <- c("ICC MI, II", "ICC MIII (no leniency)", "ICC MIII (full leniency)", paste("Industry", 1:ncol(deltas0)))
pallete <- c('blue4', 'blue', 'red', 'lightcoral', 'aquamarine3', 'cyan4', 'brown', 'orange')
p_3 <- autoplot(sim) +
  xlab("Time") +
  ylab("Discount Factor \u03b4") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5, size = 10),# move caption to the middle
        axis.title = element_text(size = 10)
  )
p_3 + plot_layout(guides = "collect") & theme(legend.position='right') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete) & theme(text = element_text(family = "Times New Roman"))
# ggsave("analysis/figures/Fig1_ICC_2.png")
# ggsave(filename="analysis/figures/Fig1_ICC_2.eps", plot = last_plot(), device = cairo_ps, width = 8, height = 6)
