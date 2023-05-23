# Probit
# 1. stage: Pr(detected) depending on parameters. Save error term. 
# 2. stage: Pr(Cartel) depending on parameters and error term.
# 2. hazard: duration (nur detected?) ~ incl error term

# plot residuals => non normal?

# Read in simulated cartels
seeds_dir <- "seed_1673465635_2ind"
n_industries <- 2
allperiods <- 1000


cartels_duration <- read.table(file = paste("analysis/data/", seeds_dir, "/duration_enforcement.csv", sep = ""), header = TRUE, sep = ";")


myprobit <- glm(detected ~  startyear + endyear + n_firms + rho_start + theta_len + gamma + structured, family = binomial(link = "probit"), 
                data = cartels_duration)
summary(myprobit)

panel
p(t=start)
p(cartel=1)
p(detected=1) ~ year + n_firms



# build panel dataset
nrow(parms)
parms$parm_id <- 1:nrow(parms)
data <- expand.grid(period = 1:allperiods, industry = 1:n_industries, parms$parm_id)
data <- data[order(data$industry, data$parm_id, data$period),]

cartels_duration <- arrange(cartels_duration, industry, parm_id, start)
cols <- c('industry', colnames(cartels_duration)[9:21])
w <- cartels_duration[,cols]
w <- select(w, -c("detected", "theta", "leniency_periods")) # remove detected
# get Unique rows
unique_cartel_parms <- distinct(w)

# join basic data with unique parms
#data_w <- left_join(data, unique_cartel_parms, by=c("industry", "parm_id"))
data_w <- left_join(data, parms, by=c("parm_id"))


# add rows with periods between start and end
lst1 <- Map(`:`, cartels_duration$start, cartels_duration$end)
y <- transform(cartels_duration[rep(seq_len(nrow(cartels_duration)), lengths(lst1)), cols], period = unlist(lst1))
y$in_cartel <- 1

# combine to panel data (use only individual values from y)
y_individual <- select(y, c("industry", "parm_id", "period", "detected", "in_cartel"))
paneldata <- left_join(data_w, y_individual, by=c("industry", "parm_id", "period"))
paneldata$detected <- replace(paneldata$detected, is.na(paneldata$detected), 0)
paneldata$in_cartel <- replace(paneldata$in_cartel, is.na(paneldata$in_cartel), 0)
describe(paneldata)
summary(paneldata)
median(paneldata$in_cartel)
skew(paneldata$in_cartel)
write.table(paneldata, file = paste("analysis/data/", seeds_dir, "/paneldata.csv", sep = ""), row.names = FALSE, sep = ";")
