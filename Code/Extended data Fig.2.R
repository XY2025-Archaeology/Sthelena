################################################################################
###  R-Code for the Manuscript of St Helena                                  ###
###  Author:  Xueye Wang                                                     ###
###  Date: 15 June 2025                                                      ###
###  R version 4.3.2                                                         ###
###                                                                          ###
################################################################################


###########################This script is for creating Extended Data Figure 2##############################
# Install required packages
install.packages("ggplot2")
install.packages("readxl")
install.packages("ggforce")
install.packages("RColorBrewer")

# Load required packages
library(ggplot2)
library(readxl)
library(ggforce)
library(RColorBrewer)

# set working directory
setwd("D:/ky/R/sthelenaSr")

# Read strontium isotope dataset
df<-read_excel("data/Extended_Data_Fig2_Data.xlsx")
colnames(df)[3]<-"Sriso"

# Define custom color palette
mycolors = c(brewer.pal("Paired", n = 5), brewer.pal(name="Dark2", n = 7))

# Create violin plot of 87Sr/86Sr values by region/site
p<-ggplot(df,aes(x=ID,y=Sriso,fill=ID))+
  geom_violin(alpha = 0.5,trim=T,linewidth=0.15,show.legend=FALSE,scale="width",draw_quantiles = c(0.25, 0.5, 0.75))+ 
  labs(x = "Sites",y = expression(paste({}^{87}, "Sr/", {}^{86}, "Sr"))) +
  scale_fill_manual(values=c("#6A9ACE","#BFEFFF","#DB614F","#FDD5C0","#1E803D","#97D1A0","#F18C25","#FAECA8","#715EA9","#CAC0E1","#E64825","#F6C0CC"))+
  geom_jitter(aes(color=ID),shape=16,size=0.65)+
  scale_color_manual(values=c("#6A9ACE","#8EE5EE","#DB614F","#FDD5C0","#1E803D","#97D1A0","#F18C25","#FFD700","#715EA9","#CAC0E1","#E64825","#F6C0CC"))+
  scale_y_continuous(limits = c(0.700, 0.780), breaks = seq(0.70, 0.780, by = 0.01),labels = scales::number_format(accuracy = 0.001))+
  theme_bw()+ 
  theme(panel.grid = element_blank())+theme(legend.position = "none")

# Export the plot as a high-resolution PDF
ggsave("figures/Extended_data_Fig2.pdf",p,width=8.9,height=7,units="cm")

# we used adobe illustrator to further revise the figures 


