rm(list = ls())
source(file = "analysis/scripts/functions_simulation.R")

# PLOT: ALL CARTELS OVER TIME
# #font_import() only do this one time - it takes a while
#loadfonts(device = "postscript")  # this works when saving as .eps or .png, together with device=cairo_ps (see below)

### ADJUST y axis FOR OTHER DATA ###
y_axis <- c(0,8)
# -------------------------

pallete <- c('blue', 'red')
y_label <- "Percentage of cartels"
x_label <- "Time"
status_all <- c("population", "detected", "undetected")
options(repr.plot.width =5.5, repr.plot.height =6)

# Model I
directory <- "smooth_r0_1_model1_100nf_seed123_set" 

cartels_population <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[1], ".rds", sep = ""))
cartels_detected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[2], ".rds", sep = ""))
parmname <- paste("analysis/data/", directory, "/parms.csv", sep = "")
parms <- read.table(parmname, header = TRUE, sep = ";")
c_det <- rowSums(cartels_detected)/(dim(cartels_detected)[2] * dim(cartels_detected)[3]) # number of detected cartels / number of industries
c_pop <- rowSums(cartels_population)/(dim(cartels_population)[2] * dim(cartels_population)[3]) # number of cartels / number of industries
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
mean(sim_cartels[,'Population'])
mean(c_pop) # average share of cartels:  0.5182764
mean(c_det) # average share of detected cartels: 0.178117

# Model II
directory <- "smooth_r0_1_model2_100nf_seed123_set" 

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
mean(c_pop) 
mean(c_det) 

# Model III
directory <- "smooth_r0_1_model3_100nf_seed123_set" 

cartels_population <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[1], ".rds", sep = ""))
cartels_detected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_", status_all[2], ".rds", sep = ""))
parmname <- paste("analysis/data/", directory, "/parms.csv", sep = "")
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
  ylim(y_axis) +
  xlab(x_label) +
  ylab(y_label) +
  labs(caption = expression("c) Model III ("~italic(structured )~"= 0)")) +
  theme(
    plot.caption = element_text(hjust = 0.5, vjust = 0, size = 10), # move caption to the middle
    axis.title = element_text(size = 10)
  ) 
mean(c_pop_unstruc) 
mean(c_det_unstruc) 

# Model IIIb, structured
x_struc <- which(parms$structured == 1)
y_det_struc <- cartels_detected[,,x_struc]
y_pop_struc <- cartels_population[,,x_struc]
c_det_struc <- rowSums(y_det_struc)/(dim(y_det_struc)[2] * dim(y_det_struc)[3])
c_pop_struc <- rowSums(y_pop_struc)/(dim(y_pop_struc)[2] * dim(y_pop_struc)[3])

sim_cartels_struc <- ts(data = cbind(c_pop_struc, c_det_struc))
colnames(sim_cartels_struc) <- c("Population", "Sample")
plot3b <- autoplot(sim_cartels_struc*100) +
  ylim(y_axis) +
  xlab(x_label) +
  ylab(y_label) +
  labs(caption = expression("d) Model III ("~italic(structured )~"= 1)")) +
  theme(
    plot.caption = element_text(hjust = 0.5, vjust = 0, size = 10), # move caption to the middle
    axis.title = element_text(size = 10)
  ) 
plot3ab <- plot3a + plot3b + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete) 
mean(c_pop_struc)
mean(c_det_struc) 

## PLOT all in one plot
plot_all <- plot1 + plot2 + plot3a + plot3b + plot_layout(guides = "collect")  &  theme(legend.position='bottom') & theme(legend.title = element_blank()) & scale_colour_manual(values=pallete) & theme(text = element_text(family = "Times New Roman"))
plot_all
# ggsave(filename="analysis/figures/Fig2_Cartels.eps", plot = last_plot(), device = cairo_ps, width = 7, height = 7)
# ggsave(filename="analysis/figures/Fig2_Cartels.png", plot = last_plot(), width = 7, height = 7)



