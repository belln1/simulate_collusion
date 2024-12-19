rm(list = ls())
source(file = "analysis/scripts/functions_simulation.R")

library(extrafont)
font_import() # only do this one time - it takes a while
loadfonts(device = "win")
loadfonts(device = "postscript")  # for saving as .eps or .png, together with device=cairo_ps (see below)
postscriptFonts()

# PLOT: Discount Factors and ICC for Different Models
allperiods <- 1000
periodsNoLen <- 0 # thetas remain constant for all time periods
periodsLen <- allperiods
timefactor <- 12
r_1 <- 0.03 #interest rate
#min_share <- 0.8 # minimum percentage of firms needed to form a cartel
min_share <- 1 # minimum percentage of firms needed to form a cartel

# 5 firms, 0.15 sigma
n_firms <- 5
gamma <- 0.9
theta <- 1
i <- 1
k <- 1


seed_start <- 3215
n_industries <- 300
count <- k*1000000+i*1000+ seed_start
deltas <- ts(replicate(n_firms, {count <<- count+1; get_deltas_r(r_1, allperiods, seed=count)}))

ICC_no_enforc <- get_ICC_model1(n_firms)

# ----------------
# III) Increasing sigma, Plot with different ICC for leniency = 0 and 1, Plot ICCs for firm 1

sigma_all <- 0.15
sigma_t <- 1 - (1-sigma_all)^(1/200) # 200 is an approximation of mean duration, in simulation run with sigma=0

sigma_t
parms <- tibble(
  n_firms = n_firms,
  sigma_start = sigma_t,
  theta = 1,
  structured = 1,
  gamma = gamma
)
parms <- parms %>%
  arrange(n_firms, sigma_start, theta, structured)

sim_list_1theta <- simulate_firms_model3(1, parms, 1, seed_start, min_share)
parms$theta <- 0
sim_list_0theta <- simulate_firms_model3(1, parms, 1, seed_start, min_share)
sim <- ts(data = cbind(ICC_no_enforc, sim_list_1theta$ICC_entry[,1], sim_list_0theta$ICC_entry[,1], deltas))
colnames(sim) <- c("ICC MI, II", "ICC MIII (no leniency)", "ICC MIII (full leniency)", paste("Firm", 1:ncol(deltas)))
pallete <- c('blue4', 'blue', 'red', 'lightcoral', 'aquamarine3', 'cyan4', 'brown', 'orange')
p_3 <- autoplot(sim) +
  ylim(0.79, 0.95) +
  xlab("Time") +
  ylab("Discount Factor \u03b4") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5, size = 10),# move caption to the middle
        axis.title = element_text(size = 10)
  )
p_3 + plot_layout(guides = "collect") & theme(legend.position='right') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete) & theme(text = element_text(family = "Times New Roman"))
#ggsave("analysis/figures/ICC_m3_new.png")
#ggsave(filename="analysis/figures/Fig1_ICC.eps", plot = last_plot(), device = cairo_ps, width = 8, height = 6)


# find time period of increased sigma
x <- sim_list_0theta$ICC_entry
table(x)
2275/5 # after 455 periods, ICC increase

y <- sim_list_0theta$firms
deltas[455:456,] > x[455,1]

###-------------------------------------------------------------------------------------------------------------------------------------
###-------------------------------------------------------------------------------------------------------------------------------------


