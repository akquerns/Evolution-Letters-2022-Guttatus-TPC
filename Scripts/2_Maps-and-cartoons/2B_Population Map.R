################################################################################
###PROJECT: Mimulus guttatus TPC Project
###PURPOSE: Create Map of populations used in study and corresponding temperature variables

### Fig. 2A: Map of US pops
###Fig. 2B: Map of UK pops
###Fig. 2C: MAT vs Latitude for pops in study
### Fig. 2D: Temperature seasonality vs latitude for pops in study

#############################################################################

#Load libraries

library(lemon)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(tidyr)
library(plotly)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggsn)
library(viridis)
#install.packages("ggplot2", "grid", "maptools", "maps", dependencies = TRUE)

#library(extrafont)
#font_import()
#loadfonts(device="win")      

#Register fonts for Windows bitmap output; helpful if wanting to reformat fonts
#fonts()  

northA <- c("Canada", "USA") #specifying that north america includes Canada and USA

UK <- c("UK", "ireland")  #specifying that UK for mapping purposes includes UK and Ireland

global1<- map_data("world", region=northA) #pull map data for mapping North America

global2<- map_data("world", region=UK) #pull map data for mapping UK

pop <- read.csv("Processed-data/mean_df_subsetwithclim.csv") #Full population datapoints dataset

pop1 <- pop %>% dplyr::filter(Range=="N") #population datapoints--North American 

pop2 <- pop %>% dplyr::filter(Range=="I") #Population datapoints--UK

#########################################
############ Fig. 2A: US Pop Map ########
#########################################

sb <- c(x=-164,y=77) #anchor scalebar
nor<- c(x=-160,y=34) #anchor compass

labslongUS<- c("170°W", "160°W", "150°W", "140°W", "120°W", "110°W", "100°W")


labslatUS<- c("30°N", "40°N", "50°N", "60°N", "70°N", "80°N")

p1<-ggplot() +
  geom_polygon(data = global1, aes(x = long, y = lat, group = group), color="black", fill="grey")+ 
  coord_fixed(xlim = c(-170, -115), ylim=c(30,80), ratio=1.6) +
  geom_jitter(data=pop1, aes(x=long, y=lat), shape=24, size=6, color= "black", fill="deepskyblue1") +
  theme_bw()+ theme(text = element_text(size=16), axis.text.x = element_text(angle=45, size=12, margin = margin(t = 20)), axis.text.y = element_text(size=12), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  scale_x_continuous(name="Longitude", labels=labslongUS) +
  scale_y_continuous(name="Latitude", labels=labslatUS) + scalebar(x.min=-170, x.max = -110, y.min=30, y.max= 80, dist=200, dist_unit="km", transform=TRUE, location="topleft", anchor=sb, st.dist=0.04, st.size=3.5) +north(data=NULL, x.min=-170, x.max = -110, y.min=30, y.max= 80, symbol=3, anchor=nor)

p1


######################################
######  Fig. 2B:  UK Pop Map  #######
#####################################


anch <- c(x=-11,y=60.5) #necessary for anchoring scalebar
anch1 <- c(x=-10,y=51)  #necessary for anchoring compass

labslongUK<- c("12°W", "8°W", "4°W", "0°W", "°W")


labslatUK<- c("50°N", "52.5°N", "55°N", "57.5°N", "60°N", "°N")

p2<-ggplot() +
  geom_polygon(data = global2, aes(x = long, y = lat, group = group), color="black", fill="grey") +
  coord_fixed(xlim= c(-12,2), ylim= c(50,61), ratio=1.6) +
  geom_jitter(data=pop2, aes(x=long, y=lat), shape=21, size=6, color= "black", fill="firebrick3") +
  theme_bw() +
  scale_x_continuous(name="Longitude", labels=labslongUK) +
  scale_y_continuous(name="Latitude", labels=labslatUK) +
theme(text = element_text(size=16), axis.text.x = element_text(angle=45, margin = margin(t = 20), size=12), axis.text.y = element_text(size=12), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
 scalebar(x.min=-12, x.max = 2, y.min=50, y.max= 61, dist=100, dist_unit="km", transform=TRUE, location="topleft", anchor=anch, st.dist=0.04, st.size=3.5) +
north(data=NULL, x.min=-12, x.max = 2, y.min=50, y.max= 61, symbol=3, anchor=anch1)

p2

###########################################
#### Fig. 2C-D :  Climate Data   ##########
##########################################
group.colors <- c(I = "firebrick3", N = "deepskyblue1")


#upload data for seasonality and MAT vs latitude for populations used
data <- read.csv("Processed-data/mean_df_subsetwithclim.csv")

#Fig. 2C: MAT vs latitude
p4 <- ggplot() + geom_jitter(data=data, aes(x=lat, y=MAT, group=Population, col=Range, shape=Range, size=3), alpha=0.65, width=0.6) + geom_smooth(data=data, aes(x=lat, y=MAT, group=Range, col=Range), method="lm", level=0.95)+scale_color_manual(values=group.colors)+
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=10), axis.text.y = element_text(size=10), legend.position="none") +
  scale_x_continuous(name="Latitude (°N)") +
  scale_y_continuous(name="Mean Annual Temperature (°C)")
p4

#Fig. 2D: seasonality vs latitude


p3 <- ggplot() + geom_jitter(data=data, aes(x=lat, y=tempseason, group=Population, col=Range, shape=Range, size=6), alpha=0.65, width=0.6) + geom_smooth(data=data, aes(x=lat, y=tempseason, group=Range, col=Range), method="lm", level=0.95) +scale_color_manual(values=group.colors)+
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+
  scale_x_continuous(name="Latitude (°N)") +
  scale_y_continuous(name="Temperature Seasonality (°C)")
p3


grid.arrange(p1, p2, nrow=1)
#save img 1200x600: FIG 2-MAPS

grid.arrange(p4, p3, nrow=1)
#save img 1000x500: Fig 2-CLIMATE

#These two figures (maps and clines) will later be edited into one figure and saved to Figures/Manuscript
