################################################################################
###PROJECT: Mimulus guttatus TPC Project

###PURPOSE: Conduct linear models relating TPC variables (Topt and Tbreadth) to temperature variables/latitude
###AKA--let's find out if any clines are significant

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

##First we need to check for normality of resids##

shapiro.test(data$maximaBT)
shapiro.test(data$x_maxBT)
shapiro.test(data$x_minBT)
shapiro.test(data$B50)
shapiro.test(data$max_RGR)

#should be all good

#Terms:
#maximaBT: Thermal optima (Topt)
#max_RGR: performance maxima (Pmax)
#B50: Thermal breadth (Tbreadth)
#lat: Latitude
#MAT: Mean annual temperature
#TS: Temperature seasonality
#x_maxBT: Upper thermal limit (Tmax)
#x_minBT: Lower thermal limit (Tmin)


###########################Linear mods for each TPC Var#####################


######################################1:  Optima################################

#Optima by Lat + Range + LatxRange
optvslat1<- lm(maximaBT~lat*Range, data=data)
summary(optvslat1)

#w/o Interaction term
optvslat2<- lm(maximaBT~lat+Range, data=data)
summary(optvslat2)

#AIC

AIC(optvslat1, k=2)
AIC(optvslat2, k=2)

##########################

#Optima by MAT + Range + MATxRange
optvsMAT1<- lm(maximaBT~MAT*Range, data=data)
summary(optvsMAT1)

#w/o Interaction term
optvsMAT2<- lm(maximaBT~MAT+Range, data=data)
summary(optvsMAT2)

#AIC
AIC(optvsMAT1, k=2)
AIC(optvsMAT2, k=2) #<-better model

#Overall effect of MAT is significant

#Let's try to further understand the low R-square in this relationship by looking at how significance differs between ranges

ToptvsMATN <- lm (maximaBT~MAT, data=data%>% dplyr::filter(Range=="N"))
summary(ToptvsMATN)

ToptvsMATI <- lm (maximaBT~MAT, data=data %>% dplyr::filter(Range=="I"))
summary(ToptvsMATI)


###################################  2: TBreadth  #############################

#Breadth by Lat*Range
B50vslat1<- lm(B50~lat*Range, data=data)
summary(B50vslat1)

#w/o interaction term
B50vslat2<- lm(B50~lat+Range, data=data)
summary(B50vslat2)

#AIC
AIC(B50vslat1, k=2)
AIC(B50vslat2, k=2)

##################

#Breadth by TS*Range
B50vsTS1<- lm(B50~tempseason*Range, data=data)
summary(B50vsTS1)

#w/o interaction term
B50vsTS2<- lm(B50~tempseason+Range, data=data)
summary(B50vsTS2)

#AIC

AIC(B50vsTS1, k=2)
AIC(B50vsTS2, k=2)

