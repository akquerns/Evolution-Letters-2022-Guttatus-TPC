################################################################################
###PROJECT: Mimulus guttatus TPC Project
###PURPOSE: Graph clines (or lack thereof) of TPC variables vs temp variables/latitude
###Significance of clines is determined by linear models (see 5A_Linear-mods.R)

###OUTPUT: Fig. 4
###4A: Topt vs lat
###4B: Topt vs MAT
###4C: Tbreadth vs lat
###4D: Tbreadth vs seasonality

################################################################################

#Load Libraries
library(rstan)
library(devtools)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggpubr)
library(plotly)
library(lemon)
library(readxl)
library(car)
library(lme4)
library(gridExtra)
library(MuMIn)
library("dplyr")

#Load Data
data <- read.csv("Processed-data/mean_df_subsetwithclim.csv")
dataN <- read.csv("Processed-data/mean_df_subsetwithclim.csv") %>% dplyr::filter(Range=="N")
dataI <- read.csv("Processed-data/mean_df_subsetwithclim.csv") %>% dplyr::filter(Range=="I")

#Set colors
group.colors <- c(I = "firebrick3", N = "deepskyblue1")
group.colors1 <- c(UK = "firebrick3", US = "deepskyblue1")

#set potential linetypes
group.linesNsig <- c(I = "dotted", N = "solid")
group.linesNsig1 <- c(UK = "dotted", US = "solid")
group.linesUKsig <- c(I = "solid", N = "dotted")
group.linesUKsig1 <- c(UK = "solid", US = "dotted")
group.linesneithersig <- c(I = "dashed", N = "dotted")

###############################################################
##################   Cline figs of latitude              ######
##############################################################


#Fig. 4A- Optima by lat-NS
z1<-ggplot() + geom_jitter(data=data, aes(x = lat, y = maximaBT, group=Range, col=Range, shape=Range), size=3, alpha=1) +
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)+
  scale_x_continuous(name=expression(paste("Latitude (°N)")))+
  scale_y_continuous(name=expression(paste(T[opt], " (°C)"))) 
z1

#Fig. 4C: Thermal breadth by lat--NS
z5<-ggplot() + geom_jitter(data=data, aes(x = lat, y = B50, group=Range, col=Range, shape=Range), size=3, alpha=1)+
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors) +  scale_x_continuous(name=expression(paste("Latitude (°N)")))+
  scale_y_continuous(name=expression(paste(T[breadth], " (°C)")))
z5


#################################################################
##############   Cline fig of Topt by Mean annual temp    ##########
###############################################################

# 4B: Optima v MAT--overall effect of MAT is significant
#however, because the cline is influenced by patterns in the native range, for graphing purposes the invasive range is dashed
y1<-ggplot() + geom_jitter(data=data, aes(x = MAT, y = maximaBT, group=Range, col=Range, shape=Range), size=3, alpha=1) +
 geom_smooth(data=data, aes(x = MAT, y = maximaBT, col=Range), method="lm", level=0.95, col="black", size=1.25, alpha=0.3) + 
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors) +
  scale_x_continuous(name=expression(paste("MAT (°C)")))+
  scale_y_continuous(name=expression(paste(T[opt], " (°C)")))+ geom_smooth(data=dataI, aes(x = MAT, y = maximaBT, group=Range, col=Range), linetype="dashed", method="lm", level=0.95, size=1, alpha=0.3, se=FALSE)+ geom_smooth(data=dataN, aes(x = MAT, y = maximaBT, group=Range, col=Range), linetype="solid", method="lm", level=0.95, size=1, se=FALSE)
y1



#################################################################
##############   Cline fig of Tbreadth by TS         ##########
###############################################################

#Fig. 4D: breadth by TS --NS
y5<-ggplot() + geom_jitter(data=data, aes(x = tempseason, y = B50, group=Range, col=Range, shape=Range), size=3, alpha=1) +
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)+
  scale_y_continuous(name=expression(paste(T[breadth], "(°C)")))+
  scale_x_continuous(name=expression(paste("Seasonality (°C)")))

###################################################################################################
##################################Create Full Fig#####################################################################################################################################################

##FIG 5

grid.arrange(z1,y1, z5, y5, ncol=2) # export as image: 700 x 700; save in Figures, edit in Manuscript-figs

#Reduced figs: 1200x600 (for ppt)

grid.arrange(z1,y1, nrow=1)
grid.arrange(z3,y3, nrow=1)
grid.arrange(z4,y4, nrow=1)
grid.arrange(z5,y5, nrow=1)
