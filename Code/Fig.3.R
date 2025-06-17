################################################################################
###  R-Code for the Manuscript of St Helena                                  ###
###  Author:  Xueye Wang                                                     ###
###  Date: 15 June 2025                                                      ###
###  R version 4.3.2                                                         ###
###                                                                          ###
################################################################################

# Install required packages
install.packages("tidyverse")
install.packages("sf")
install.packages("terra")
install.packages("rnaturalearth")
install.packages("readxl")
install.packages("assignR")
install.packages("viridisLite")
install.packages("ggpubr")
install.packages("ggspatial")

# Load required packages
library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(readxl)
library(assignR)
library(viridisLite)
library(ggpubr)
library(ggspatial)

# Set working directory
setwd("D:/ky/R/sthelenaSr")

#######################################Modeling the geographic origins of individuals with both 87Sr/86Sr and ancient DNA (aDNA) data###########################################
# Load Sr isoscape and associated prediction uncertainty from Wang et al. (2024)
sr_model <- terra::rast("results/panaf_model_repeatedCV.tif")
sr_model_se <- terra::rast("results/panaf_model_repeatedCV_se.tif")

# Stack isoscape and prediction error map
sr_stack_1 <- c(sr_model,sr_model_se) 

# Load 87Sr/86Sr values of individuals also included in the aDNA analysis
df<-read_excel("data/Fig3_data.xlsx")
df_1<-df[c(1:7),]
colnames(df_1)[9]<-"Sriso"
colnames(df_1)[1]<-"individual"
examples<-data.frame(ID=c(df_1$individual),Sriso=c(df_1$Sriso))

# Calculate suitability maps for each individual based on Sr isotope values
iso_examples<- pdRaster(sr_stack_1,examples,genplot=T)

# Normalize to 0-1 scale by dividing by the maximum value
iso_examples<-sapp(iso_examples,fun=function(x,...){x*(1/max(values(x),na.rm=T))})

# Aggregate raster to reduce resolution for plotting
iso_examples_agg <- aggregate(iso_examples, fact = 4)

# Transform projection to WGS84
iso_examples_wgs <- project(iso_examples_agg, "+proj=longlat +datum=WGS84")

# Create breaks from 0 to 1 with intervals of 0.1
breaks01<-seq(0,1,length.out=11)

# Convert raster to data frame for plotting
iso_examples_plot<-as.data.frame(iso_examples_wgs,xy=T)

# Check if all values are present for each individual 
table(cut(iso_examples_plot$I_289,breaks01)) 
table(cut(iso_examples_plot$I_248,breaks01))
table(cut(iso_examples_plot$I_344,breaks01))
table(cut(iso_examples_plot$I_253,breaks01)) 
table(cut(iso_examples_plot$I_347,breaks01))
table(cut(iso_examples_plot$I_284,breaks01))
table(cut(iso_examples_plot$I_281,breaks01))

# need to add a high value to some samples to make sure that we same color class
iso_examples_plot$I_289[1]<-0.91
iso_examples_plot$I_248[1]<-0.91
iso_examples_plot$I_344[1]<-0.91
iso_examples_plot$I_253[1]<-0.91
iso_examples_plot$I_253[2]<-0.81
iso_examples_plot$I_347[1]<-0.91
iso_examples_plot$I_347[2]<-0.81
iso_examples_plot$I_347[3]<-0.71
iso_examples_plot$I_347[4]<-0.61
iso_examples_plot$I_347[5]<-0.51
iso_examples_plot$I_284[1]<-0.91
iso_examples_plot$I_284[2]<-0.81
iso_examples_plot$I_284[3]<-0.71
iso_examples_plot$I_284[4]<-0.61
iso_examples_plot$I_284[5]<-0.51
iso_examples_plot$I_281[1]<-0.91
iso_examples_plot$I_281[2]<-0.81
iso_examples_plot$I_281[3]<-0.71
iso_examples_plot$I_281[4]<-0.61
iso_examples_plot$I_281[5]<-0.51

## Plot Fig. 3
# Load country boundaries
afr<- ne_countries(continent = "Africa",returnclass = "sf")
afr <- ne_countries(continent = "Africa", returnclass = "sf") %>%
  filter(name != "Madagascar")  

# Plotting
example_1<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_289,breaks01)))+
  geom_sf(data=afr,fill=NA,color="#E5E5E5",linewidth=0.3)+
  scale_x_continuous(limits=c(-15.8,49), expand = c(0, 0))+
  scale_y_continuous(limits=c(-34.5, 11), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.2,text_cex = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        axis.title=element_blank(),legend.position = "none")
					
example_4<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_248,breaks01)))+
  geom_sf(data=afr,fill=NA,color="#E5E5E5",linewidth=0.3)+
  scale_x_continuous(limits=c(-15.8,49), expand = c(0, 0))+
  scale_y_continuous(limits=c(-34.5, 11), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.2,text_cex = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        axis.title=element_blank(),legend.position = "none")

example_5<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_344,breaks01)))+
  geom_sf(data=afr,fill=NA,color="#E5E5E5",linewidth=0.3)+
  scale_x_continuous(limits=c(-15.8,49), expand = c(0, 0))+
  scale_y_continuous(limits=c(-34.5, 11), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.2,text_cex = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        axis.title=element_blank(),legend.position = "none")

example_6<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_253,breaks01)))+
  geom_sf(data=afr,fill=NA,color="#E5E5E5",linewidth=0.3)+
  scale_x_continuous(limits=c(-15.8,49), expand = c(0, 0))+
  scale_y_continuous(limits=c(-34.5, 11), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.2,text_cex = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        axis.title=element_blank(),legend.position = "none")

example_7<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_347,breaks01)))+
  geom_sf(data=afr,fill=NA,color="darkgrey",linewidth=0.3)+
  scale_x_continuous(limits=c(-15.8,49), expand = c(0, 0))+
  scale_y_continuous(limits=c(-34.5, 11), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01
  ) +
  theme_bw()+
  annotation_scale(width_hint=0.2,text_cex = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        axis.title=element_blank(),legend.position = "none")

example_8<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_284,breaks01)))+
  geom_sf(data=afr,fill=NA,color="darkgrey",linewidth=0.3)+
  scale_x_continuous(limits=c(-15.8,49), expand = c(0, 0))+
  scale_y_continuous(limits=c(-34.5, 11), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.2,text_cex = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        axis.title=element_blank(),legend.position = "none")

example_9<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_281,breaks01)))+
  geom_sf(data=afr,fill=NA,color="darkgrey",linewidth=0.3)+
  scale_x_continuous(limits=c(-15.8,49), expand = c(0, 0))+
  scale_y_continuous(limits=c(-34.5, 11), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.2,text_cex = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        axis.title=element_blank(),legend.position = "none")



# Arrange plots into a 3×3 grid
Figure3<-ggarrange(example_1, example_1, example_1,example_4,example_5,example_6,example_7, example_8, example_9,nrow=3,ncol=3,common.legend = T,legend = TRUE)

# Export the figure as a high-resolution PDF
ggsave("figures/Fig.3.pdf",Figure3,width=20, height=20,units="cm")

# we used adobe illustrator to further revise the figures 
