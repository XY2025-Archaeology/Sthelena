################################################################################
###  R-Code for the Manuscript of St Helena                                  ###
###  Author:  Xueye Wang                                                     ###
###  Date: 15 June 2025                                                      ###
###  R version 4.3.2                                                         ###
###                                                                          ###
################################################################################


###########################This script is for creating Extended Data Figure 5##############################
# Install required packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("ggpubr")
install.packages("RColorBrewer")

# Load required packages
library(readxl)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(RColorBrewer)

# Set working directory
setwd("D:/ky/R/sthelenaSr")

# Generate 11 distinct colors using a combined palette
colorPalette <- colorRampPalette(brewer.pal(12, "Paired"))(11)

# Define input and output file paths
input_file  <- "data/Extended_Data_Fig5_Data.xlsx"
output_file <- "figures/Sr_isotope_panels.pdf"

# Read the Excel file containing intra-tooth 87Sr/86Sr data
df <- read_excel(input_file, col_names = TRUE)
df$TestOrder <- 1:nrow(df)

# Reshape the data to long format for plotting and remove NA values
df_long <- melt(df, id.vars = "TestOrder", variable.name = "Sample", value.name = "Sr_ratio") %>%
  na.omit()

# Compute the mean 87Sr/86Sr value for each sample and sort from high to low
sample_means <- df_long %>%
  group_by(Sample) %>%
  summarize(mean_Sr = mean(Sr_ratio)) %>%
  arrange(desc(mean_Sr)) %>%
  pull(Sample)

# Assign colors based on sample order
color_map <- setNames(colorPalette, sample_means)

# Set sample order for plotting (factor levels sorted by mean Sr ratio)
df_long <- df_long %>%
  mutate(Sample = factor(Sample, levels = sample_means))

# Define a common minimalist theme 
common_theme <- theme_minimal() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    panel.grid.major = element_line(color = "grey80", size = 0.2),
    panel.grid.minor = element_line(color = "grey90", size = 0.1),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    axis.line.y.right = element_line(color = "black", size = 0.3),
    axis.line.x.top   = element_line(color = "black", size = 0.3),
    axis.ticks.y.right = element_blank(),
    axis.ticks.x.top   = element_blank(),
    axis.text.y.right  = element_blank(),
    axis.text.x.top    = element_blank()
  )

# Left panel: Line plots showing intra-tooth Sr profiles from cusp to cervix
p1 <- ggplot(df_long, aes(x = TestOrder, y = Sr_ratio, color = Sample)) +
  geom_line(size = 0.4) +
  labs(
    x = "Laser Ablation Track from Cusp to Cervix",
    y = expression(paste({}^{87}, "Sr/", {}^{86}, "Sr"))
  ) +
  common_theme +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.3, "cm")
  ) +
  scale_color_manual(values = color_map) +
  scale_y_continuous(
    breaks = seq(0.710, 0.740, by = 0.005),
    limits = c(0.710, 0.740),
    sec.axis = dup_axis(name = NULL)
  ) +
  scale_x_continuous(
    breaks = seq(0, max(df_long$TestOrder), by = 100),
    sec.axis = dup_axis(name = NULL)
  )

# Right panel: Boxplots summarizing the 87Sr/86Sr variation per sample
p2 <- ggplot(df_long, aes(x = Sample, y = Sr_ratio, fill = Sample)) +
  geom_boxplot(
    linewidth = 0.3,
    outlier.size = 0.8,
    outlier.shape = 1,
    outlier.color = "black"
  ) +
  labs(x = "Sample", y = "") +
  common_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(5, 7, 5, 2, "mm"),
    axis.line.x.top = element_line(color = "black", linewidth = 0.3),
    axis.text.x.top = element_blank()
  ) +
  scale_fill_manual(values = color_map) +
  scale_y_continuous(
    breaks = seq(0.710, 0.740, by = 0.005),
    limits = c(0.710, 0.740),
    sec.axis = dup_axis(name = NULL)
  ) +
  scale_x_discrete() 

# Export both panels as a side-by-side figure in high-resolution PDF
# Output dimensions: 210 × 100 mm (landscape)
ggsave(
  filename = output_file,
  plot = ggarrange(p1, p2, ncol = 2,
                   widths = c(1.2, 1),
                   align = "h",
                   common.legend = FALSE),
  width = 210 / 25.4,
  height = 100 / 25.4,
  dpi = 600
)
# we used adobe illustrator to further revise the figures 