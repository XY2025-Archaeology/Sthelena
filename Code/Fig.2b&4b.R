################################################################################
###  R-Code for the Manuscript of St Helena                                  ###
###  Author:  Xueye Wang et al.                                              ###
###  Date: 15 June 2025                                                      ###
###  R version 4.3.2                                                         ###
###                                                                          ###
################################################################################

######################################This script is for creating Figure 2b and Figure 4b#####################################################################################
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

#######################################Modeling the geographic origins of individuals in Groups 1–6 shown in Figure 2b and Figure 4b###########################################
# Load Sr isoscape and associated prediction uncertainty from Wang et al. (2024), NC paper 
sr_model <- terra::rast("results/panaf_model_repeatedCV.tif")
sr_model_se <- terra::rast("results/panaf_model_repeatedCV_se.tif")

# Crop raster to the target region shown in Figure 2
r_eck<-terra::rast("results/wc2.1_30s_elev.tif") #Any global raster file can be used
r_eck_terra <- terra::rast(r_eck)
e <- ext(1.74, 28, -18, 7.5)
bioe <- crop(r_eck_terra, e)
bioe_project <- terra::project(bioe, "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Stack isoscape and prediction error map, then crop to West and West-Central Africa
sr_stack_1 <- c(sr_model,sr_model_se) 
ex<-ext(160509.3,2632056,-2349398,986956)
sr_stack_1<-crop(sr_stack_1,ex)

# Load human Sr isotope data
df<-read_excel("data/Fig2_and_4_data.xlsx")
df_1<-df[c(1:10),]
colnames(df_1)[9]<-"Sriso"
colnames(df_1)[1]<-"individual"
examples<-data.frame(ID=c(df_1$individual),Sriso=c(df_1$Sriso))

# Calculate suitability maps for each individual based on Sr isotope values
iso_examples<- pdRaster(sr_stack_1,examples,genplot=T)

# Normalize all raster layers to a 0–1 scale
iso_examples<-sapp(iso_examples,fun=function(x,...){x*(1/max(values(x),na.rm=T))})

# Aggregate rasters to reduce spatial resolution for faster plotting
iso_examples_agg <- aggregate(iso_examples, fact = 2)

# Reproject rasters to WGS84 for visualization
iso_examples_wgs <- project(iso_examples_agg, "+proj=longlat +datum=WGS84")

# Create breaks from 0 to 1 with intervals of 0.1
breaks01<-seq(0,1,length.out=11)

# Convert raster to data frame for plotting
iso_examples_plot<-as.data.frame(iso_examples_wgs,xy=T)

# Check if all values are present for each individual 
table(cut(iso_examples_plot$I_226,breaks01)) 
table(cut(iso_examples_plot$I_248,breaks01))
table(cut(iso_examples_plot$I_344,breaks01))
table(cut(iso_examples_plot$I_245,breaks01))
table(cut(iso_examples_plot$I_384,breaks01))
table(cut(iso_examples_plot$I_347,breaks01))
table(cut(iso_examples_plot$I_242_1,breaks01)) 
table(cut(iso_examples_plot$I_242_2,breaks01))
table(cut(iso_examples_plot$I_267_1,breaks01))
table(cut(iso_examples_plot$I_267_2,breaks01))

# Add a high value to some samples to make sure the same color class
iso_examples_plot$I_226[1]<-0.91
iso_examples_plot$I_248[1]<-0.91
iso_examples_plot$I_344[1]<-0.91
iso_examples_plot$I_245[1]<-0.91
iso_examples_plot$I_245[2]<-0.81
iso_examples_plot$I_384[1]<-0.91
iso_examples_plot$I_384[2]<-0.81
iso_examples_plot$I_347[1]<-0.91
iso_examples_plot$I_347[2]<-0.81
iso_examples_plot$I_347[3]<-0.71
iso_examples_plot$I_347[4]<-0.61
iso_examples_plot$I_242_1[1]<-0.91
iso_examples_plot$I_242_1[2]<-0.81
iso_examples_plot$I_242_2[1]<-0.91
iso_examples_plot$I_267_1[1]<-0.91
iso_examples_plot$I_267_1[2]<-0.81
iso_examples_plot$I_267_2[1]<-0.91

# Plot individuals with 87Sr/86Sr values < 0.74 (Group1-6 in Figure 2b)
# Load African country boundaries and river shapefile
afr<- ne_countries(continent = "Africa",returnclass = "sf")
river1<-sf::read_sf("data/rivers/ne_110m_rivers_lake_centerlines.shp")

#  Set CRS and reproject rivers to Eckert IV
st_crs(river1) <- crs('+proj=longlat')
river1 %>% 
  st_transform('+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

# Plotting Figure 2b (Groups 1-6)
# The first two plots (example_1, example_2) are empty base maps used for layout alignment
# Subsequent plots (example_3 to example_8) show individual-level 87Sr/86Sr suitability maps
example_1<-ggplot()+
  geom_sf(data=afr,fill=NA,color="white",size=0.7)+
  geom_sf(data=river1,color="white",size=0.5)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  theme_classic()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),axis.ticks= element_blank(),
        axis.title=element_blank(),axis.line = element_blank())

example_2<-ggplot()+
  geom_sf(data=afr,fill=NA,color="white",size=0.7)+
  geom_sf(data=river1,color="white",size=0.5)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  theme_classic()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),axis.ticks= element_blank(),
        axis.title=element_blank(),axis.line = element_blank())

example_3<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_226,breaks01)))+
  geom_sf(data=afr,fill=NA,color="white",linewidth=0.5)+
  geom_sf(data=river1,color="#00FFFF",linewidth=0.3)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_4<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_248,breaks01)))+
  geom_sf(data=afr,fill=NA,color="white",linewidth=0.5)+
  geom_sf(data=river1,color="#00FFFF",linewidth=0.3)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_5<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_344,breaks01)))+
  geom_sf(data=afr,fill=NA,color="white",linewidth=0.5)+
  geom_sf(data=river1,color="#00FFFF",linewidth=0.3)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_6<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_245,breaks01)))+
  geom_sf(data=afr,fill=NA,color="white",linewidth=0.5)+
  geom_sf(data=river1,color="#00FFFF",linewidth =0.3)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_7<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_384,breaks01)))+
  geom_sf(data=afr,fill=NA,color="white",linewidth=0.5)+
  geom_sf(data=river1,color="blue",linewidth=0.3)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_8<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_347,breaks01)))+
  geom_sf(data=afr,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=river1,color="blue",linewidth=0.3)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")


#######################################Modeling the geographic origins of individuals in Groups 7–10 shown in Figure 2b###########################################
# Load Sr isoscape and predicted standard error map for Africa
sr_model_2<-terra::rast("results/panaf_model_repeatedCV.tif")
sr_model_se_2<-terra::rast("results/panaf_model_repeatedCV_se.tif")

# Crop raster for Central and Southern Africa
r_eck_2<-terra::rast("results/wc2.1_30s_elev.tif")
e_2<-ext(3,43, -36, 1.34)
bioe_2<-crop(r_eck_2,e_2)
bioe_project_2<-terra::project(bioe_2,"+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Stack isoscape and error maps, crop to target region
sr_stack_2<-c(sr_model_2,sr_model_se_2)
ex_2<-ext(259842.1,4042515,-4554833,176849.7)
sr_stack_2<-crop(sr_stack_2,ex_2)

# Load and process human Sr isotope data (higher Sr individuals)
df_2<-read_excel("data/Fig2_and_4_data.xlsx")
df_3<-df[c(11:14),]
colnames(df_3)[9]<-"Sriso"
colnames(df_3)[1]<-"individual"
examples_2<-data.frame(ID=c(df_3$individual),Sriso=c(df_3$Sriso))

# Calculate suitability map for high-Sriso individuals
iso_examples_2<- pdRaster(sr_stack_2,examples_2,genplot=T)

# Normalize to 0-1 scale
iso_examples_2<-sapp(iso_examples_2,fun=function(x,...){x*(1/max(values(x),na.rm=T))})

# Aggregate and project for plotting
iso_examples_agg_2<-terra::aggregate(iso_examples_2,fact=5)
iso_examples_wgs_2<-project(iso_examples_agg_2,y="+proj=longlat +datum=WGS84")

# Create breaks and convert to data frame for plotting
breaks01<-seq(0,1,length.out=11)
iso_examples_plot_2<-as.data.frame(iso_examples_wgs_2,xy=T)

# Check values for all individuals
table(cut(iso_examples_plot_2$I_284,breaks01)) 
table(cut(iso_examples_plot_2$I_220,breaks01))
table(cut(iso_examples_plot_2$I_281,breaks01)) 
table(cut(iso_examples_plot_2$I_476,breaks01))

# Add a high value to some samples to make sure the same color class
iso_examples_plot_2$I_284[1]<-0.91
iso_examples_plot_2$I_284[2]<-0.81
iso_examples_plot_2$I_284[3]<-0.71
iso_examples_plot_2$I_284[4]<-0.61
iso_examples_plot_2$I_284[5]<-0.51
iso_examples_plot_2$I_220[1]<-0.91
iso_examples_plot_2$I_220[2]<-0.81
iso_examples_plot_2$I_220[3]<-0.71
iso_examples_plot_2$I_220[4]<-0.61
iso_examples_plot_2$I_220[5]<-0.51
iso_examples_plot_2$I_281[1]<-0.91
iso_examples_plot_2$I_281[2]<-0.81
iso_examples_plot_2$I_281[3]<-0.71
iso_examples_plot_2$I_281[4]<-0.61
iso_examples_plot_2$I_281[5]<-0.51
iso_examples_plot_2$I_476[1]<-0.91
iso_examples_plot_2$I_476[2]<-0.81
iso_examples_plot_2$I_476[3]<-0.71
iso_examples_plot_2$I_476[4]<-0.61
iso_examples_plot_2$I_476[5]<-0.51


#Plotting Figure 2b (Groups 7-10)
example_9<-ggplot()+
  geom_tile(data=iso_examples_plot_2,aes(x=x,y=y,fill=cut(I_284,breaks01)))+
  geom_sf(data=afr,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=river1,color="blue",linewidth=0.3)+
  scale_x_continuous(limits=c(3,43), expand = c(0, 0))+
  scale_y_continuous(limits=c(-36, 1.34), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_10<-ggplot()+
  geom_tile(data=iso_examples_plot_2,aes(x=x,y=y,fill=cut(I_220,breaks01)))+
  geom_sf(data=afr,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=river1,color="blue",linewidth=0.3)+
  scale_x_continuous(limits=c(3,43), expand = c(0, 0))+
  scale_y_continuous(limits=c(-36, 1.34), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_11<-ggplot()+
  geom_tile(data=iso_examples_plot_2,aes(x=x,y=y,fill=cut(I_281,breaks01)))+
  geom_sf(data=afr,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=river1,color="blue",linewidth=0.3)+
  scale_x_continuous(limits=c(3,43), expand = c(0, 0))+
  scale_y_continuous(limits=c(-36, 1.34), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_12<-ggplot()+
  geom_tile(data=iso_examples_plot_2,aes(x=x,y=y,fill=cut(I_476,breaks01)))+
  geom_sf(data=afr,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=river1,color="blue",linewidth=0.3)+
  scale_x_continuous(limits=c(3,43), expand = c(0, 0))+
  scale_y_continuous(limits=c(-36, 1.34), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.1,text_cex = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=4),
        axis.ticks = element_line(linewidth = 0.3), 
        axis.title=element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3), 
        legend.position = "none")

# Arrange 12 subplots (including base maps and individual assignment maps) into a 3×4 grid
Figure2b<-ggarrange(example_1,example_2,example_3,example_4,example_5,example_6,example_7,example_8,example_9,example_10,example_11,example_12,nrow=3,ncol=4,common.legend = T,legend = "right")

# Save the final composite figure as a PDF
ggsave("figures/Fig.2b.pdf",Figure2b,width=30, height=21,units="cm")



#######################################Plotting one individual with paried teeth shown in Figure 4b###########################################
example_13<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_242_1,breaks01)))+
  geom_sf(data=afr,fill=NA,color="#E0E0E0",linewidth=0.5)+
  geom_sf(data=river1,color="blue",linewidth=0.3)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

example_14<-ggplot()+
  geom_tile(data=iso_examples_plot,aes(x=x,y=y,fill=cut(I_242_2,breaks01)))+
  geom_sf(data=afr,fill=NA,color="#E0E0E0",linewidth=0.5)+
  geom_sf(data=river1,color="#00FFFF",linewidth=0.3)+
  scale_x_continuous(limits=c(1.74,28), expand = c(0, 0))+
  scale_y_continuous(limits=c(-18, 7.5), expand = c(0, 0))+
  scale_fill_viridis_d(
    option = "mako", direction = -1, na.value = "#FAEBDDFF",
    name = "Probability\nsurface", label = breaks01) +
  theme_bw()+
  annotation_scale(width_hint=0.15,text_cex = 0.7)+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=8),
        axis.title=element_blank(),legend.position = "none")

# Arrange plots
Figure4b<-ggarrange(example_13,example_14,nrow=1,ncol=2,common.legend = T,legend = "right")

# Export the figure as a high-resolution PDF
ggsave("figures/Fig.4b.pdf",Figure4b,width=18, height=20,units="cm")

# we used Adobe Illustrator to further revise the figures 








