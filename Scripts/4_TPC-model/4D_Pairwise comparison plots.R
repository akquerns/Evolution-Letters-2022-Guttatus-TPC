
#### PROJECT: Guttatus TPC project
#### PURPOSE: Plot pairwise comparisons between ranges, 
####          and among all populations
#### AUTHOR: Rachel Wooliver


###########################
# load packages
###########################
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggpubr)




###### 
###### PAIRWISE COMPARISONS BETWEEN NATIVE AND INVASIVE GROUPS
###### 
tidy_perf_pops <- read.csv("TPC-outputs/tidy_perf_pops_subset.csv")[,-1] # draws from the model

ndraws <- max(tidy_perf_pops$draw)

tidy_range <- data.frame(draw=rep(1:ndraws,2),
                         range=rep(c("I","N"), each=ndraws),
                         x_minBT=rep(NA, ndraws*2),
                         x_maxBT=rep(NA, ndraws*2),
                         maximaBT=rep(NA, ndraws*2),
                         breadthBT=rep(NA, ndraws*2),
                         B50=rep(NA, ndraws*2),
                         max_RGR=rep(NA, ndraws*2),
                         area=rep(NA,ndraws*2)) 

# fill out invasive & native averages
for(i in 1:dim(tidy_range)[1]){
  # invasive averages:
  tidy_range$x_minBT[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$x_minBT[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$x_maxBT[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$x_maxBT[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$maximaBT[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$maximaBT[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$breadthBT[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$breadthBT[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$B50[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$B50[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$max_RGR[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$max_RGR[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$area[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$area[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  
  # native averages:
  tidy_range$x_minBT[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$x_minBT[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$x_maxBT[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$x_maxBT[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$maximaBT[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$maximaBT[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$breadthBT[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$breadthBT[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$B50[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$B50[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$max_RGR[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$max_RGR[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$area[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$area[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
}

# now calculate comparisons: native - invasive (positive means that native is greater than invasive, and vice versa)
range_comps <- data.frame(x_minBT=rep(NA, ndraws),
                          x_maxBT=rep(NA, ndraws),
                          maximaBT=rep(NA, ndraws),
                          breadthBT=rep(NA, ndraws),
                          B50=rep(NA, ndraws),
                          max_RGR=rep(NA, ndraws),
                          area=rep(NA,ndraws))


for(i in 1:dim(range_comps)[1]){
  range_comps$x_minBT[i] <- tidy_range$x_minBT[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$x_minBT[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$x_maxBT[i] <- tidy_range$x_maxBT[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$x_maxBT[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$maximaBT[i] <- tidy_range$maximaBT[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$maximaBT[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$breadthBT[i] <- tidy_range$breadthBT[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$breadthBT[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$B50[i] <- tidy_range$B50[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$B50[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$max_RGR[i] <- tidy_range$max_RGR[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$max_RGR[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$area[i] <- tidy_range$area[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$area[which(tidy_range$draw==i & tidy_range$range=="I")]
}

write.csv(range_comps, "TPC-outputs/Range-comparisons.csv")


###### 
###### RANGE PAIRWISE COMPARISON TABLE FOR ALL PARAMETERS
###### 

range_comps <- read.csv("TPC-outputs/Range-comparisons.csv")


## calculate mean difference in each parameter between native and invasive
means <- round(c(mean(range_comps$x_minBT), 
                 mean(range_comps$x_maxBT), 
                 mean(range_comps$maximaBT), 
                 mean(range_comps$breadthBT), 
                 mean(range_comps$B50), 
                 mean(range_comps$max_RGR),
                 mean(range_comps$area)),3)
## calculate credible interval for the differences
lowers <- as.vector(round(c(quantile(range_comps$x_minBT, probs = c(0.025)), 
                            quantile(range_comps$x_maxBT, probs = c(0.025)), 
                            quantile(range_comps$maximaBT, probs = c(0.025)), 
                            quantile(range_comps$breadthBT, probs = c(0.025)), 
                            quantile(range_comps$B50, probs = c(0.025)), 
                            quantile(range_comps$max_RGR, probs = c(0.025)),
                            quantile(range_comps$area, probs = c(0.025))),3))
uppers <- as.vector(round(c(quantile(range_comps$x_minBT, probs = c(0.975)), 
                            quantile(range_comps$x_maxBT, probs = c(0.975)), 
                            quantile(range_comps$maximaBT, probs = c(0.975)), 
                            quantile(range_comps$breadthBT, probs = c(0.975)), 
                            quantile(range_comps$B50, probs = c(0.975)), 
                            quantile(range_comps$max_RGR, probs = c(0.975)),
                            quantile(range_comps$area, probs = c(0.975))),3))
## paste together for a table
pwc <- paste(means, " [", lowers, ", ", uppers, "]", sep="")

## now export to csv
pwctable <- data.frame(Parameter=c("Lower thermal limit (°C)",
                                   "Upper thermal limit (°C)",
                                   "Thermal optimum (°C)",
                                   "Critical breadth (°C)",
                                   "B50 (°C)",
                                   "Performance maximum (cm/cm/day)",
                                   "Area"),
                       Pairwise.comparison=pwc)
pwctable <- pwctable[c(1,2,3,6,4,5,7),]
write.csv(pwctable, "TPC-outputs/Range-comparison-table.csv")


## visualize differences

density_x_minBT <- range_comps %>% 
  ggplot(aes(x_minBT, ..scaled..)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$x_minBT), y=0), color="purple", shape=17, size=5) +
  xlab("Lower thermal limit (°C)") + 
  ylab("Probability") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


density_x_maxBT <- range_comps %>% 
  ggplot(aes(x_maxBT,..scaled..)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$x_maxBT), y=0), color="purple", shape=17, size=5) +
  xlab("Upper thermal limit (°C)") + 
  ylab("Probability") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_maximaBT <- range_comps %>% 
  ggplot(aes(maximaBT,..scaled..)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$maximaBT), y=0), color="purple", shape=17, size=5) +
  xlab("Thermal optimum (°C)") + 
  ylab("Probability") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_breadthBT <- range_comps %>% 
  ggplot(aes(breadthBT,..scaled..)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$breadthBT), y=0), color="purple", shape=17, size=5) +
  xlab("Critical breadth (°C)") + 
  ylab("Probability") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_B50 <- range_comps %>% 
  ggplot(aes(B50,..scaled..)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$B50), y=0), color="purple", shape=17, size=5) +
  xlab("B50 (°C)") + 
  ylab("Probability") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_max_RGR <- range_comps %>% 
  ggplot(aes(max_RGR,..scaled..)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$max_RGR), y=0), color="purple", shape=17, size=5) +
  xlab("Performance maximum (cm/cm/day)") + 
  ylab("Probability") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_area <- range_comps %>% 
  ggplot(aes(area,..scaled..)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$area), y=0), color="purple", shape=17, size=5) +
  xlab("Area") + 
  ylab("Probability") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



pdf("Figures/Supplementary/Fig-S2_FULL.pdf", height=10, width=10)
ggarrange(density_x_minBT, density_x_maxBT,
          density_maximaBT, density_max_RGR,
          density_breadthBT, density_B50,
          density_area,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 3)
dev.off()