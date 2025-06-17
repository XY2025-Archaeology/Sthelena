################################################################################
###  R-Code for the Manuscript of St Helena                                  ###
###  Author:  Xueye Wang                                                     ###
###  Date: 15 June 2025                                                      ###
###  R version 4.3.2                                                         ###
###                                                                          ###
################################################################################


####################This script is for creating Extended Data Figure 6#######################
# The figure includes three violin plots (panels a–c) comparing individual-level 87Sr/86Sr variation
# by burial direction, biological sex, and presence/absence of dental modification.
# Each plot includes violin density shapes, overlaid boxplots, and individual data points.
# Pairwise differences are statistically tested using Mann–Whitney–Wilcoxon test.

# Install required packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("readxl")
install.packages("RColorBrewer")

# Load required packages
library(ggplot2)
library(dplyr)
library(ggpubr)
library(readxl)
library(RColorBrewer)

# Set working directory
setwd("D:/ky/R/sthelenaSr")

# Load dataset for Extended Data Figure 6
df<-read_excel("data/Extended_Data_Fig6_Data.xlsx")
colnames(df)[9]<-"Sriso"
colnames(df)[3]<-"Direction"
colnames(df)[6]<-"DM"

# Define color palette
mycolors = c(brewer.pal("Paired", n = 5), brewer.pal(name="Dark2", n = 7))

#############################################Extended Data Fig. 6a############################
#  Violin plot comparing 87Sr/86Sr values by burial direction (North vs. South)
p <- ggplot(df, aes(x = Direction, y = Sriso, fill = Direction))+
  geom_violin(alpha = 0.5,linewidth=0.3,trim=FALSE)+  
  scale_fill_manual(values=mycolors)+
  geom_jitter(aes(color=Direction),shape=16,size=1.5)+
  scale_color_manual(values=mycolors)+
  geom_boxplot(width = 0.1,linewidth=0.3, fill = "white",outlier.shape = NA)+
  scale_y_continuous(limits=c(0.70, 0.79), expand = c(0, 0))+
  theme_bw()+ 
  theme(panel.grid = element_blank())+theme(axis.text = element_text(size = 6),legend.position = "none")
 
#Add Wilcoxon test p-value and significance stars
my_comparisons <- list(c("N", "S"))
  
p1<-p+stat_compare_means(method = "wilcox.test", 
                       label = "p.signif",
                       label.y = 0.78,
                       comparisons = my_comparisons,size=4)
 
 
###############################Extended Data Fig. 6b#########################################
# Violin plot comparing 87Sr/86Sr values by sex (female vs. male)
filtered_sex_data <- df %>%
   filter(Sex %in% c("female", "male"))
 
p2 <- ggplot(filtered_sex_data, aes(x = Sex, y = Sriso, fill = Sex))+
   geom_violin(alpha = 0.5,linewidth=0.3,trim=FALSE)+  
   scale_fill_manual(values=mycolors[3:4])+
   geom_jitter(aes(color=Sex),shape=16,size=1.5)+
   scale_color_manual(values=mycolors[3:4])+
   geom_boxplot(width = 0.1, linewidth=0.3,fill = "white",outlier.shape = NA)+  
   scale_y_continuous(limits=c(0.70, 0.79), expand = c(0, 0))+theme_bw()+ 
   theme(panel.grid = element_blank())+theme(axis.text = element_text(size = 6),legend.position = "none")

#Add Wilcoxon test p-value and significance stars
my_comparisons <- list(c("female", "male"))
 
p3<-p2+stat_compare_means(method = "wilcox.test", 
                          label = "p.signif",
                          label.y = 0.78,
                          comparisons = my_comparisons,size=4) 
 
#############################Extended Data Fig. 6c############################################
# Violin plot comparing 87Sr/86Sr values by dental modification status (Yes vs. No)
p4 <- ggplot(df, aes(x = DM, y = Sriso, fill = DM))+
   geom_violin(alpha = 0.5,linewidth=0.3,trim=FALSE)+  
   scale_fill_manual(values=mycolors[6:7])+
   geom_jitter(aes(color=DM),shape=16,size=1.5)+
   scale_color_manual(values=mycolors[6:7])+
   geom_boxplot(width = 0.1, linewidth=0.3,fill = "white",outlier.shape = NA)+ 
   scale_y_continuous(limits=c(0.70, 0.79), expand = c(0, 0))+theme_bw()+ 
   theme(panel.grid = element_blank())+theme(axis.text = element_text(size = 6),legend.position = "none")
 

#Add Wilcoxon test p-value and significance stars
my_comparisons <- list(c("Yes", "No"))
 
p5 <- p4+stat_compare_means(method = "wilcox.test", 
                           label = "p.signif",
                           label.y = 0.78,
                           comparisons = my_comparisons,size=4) 
 
 
############################# Arrange and export final figure ################################
# Combine all three subplots into a single figure (3 columns)
Extended_Data_Fig6<-ggarrange(p1,p3,p5,ncol=3,common.legend = F)
 
 
# Save figure as high-resolution PDF
ggsave("figures/Extended_Data_Fig6.pdf", Extended_Data_Fig6,width = 18, height = 8,units="cm")
