#### Load required R packages
library(readxl)       # To read Excel files
library(tidyr)        # For data tidying (e.g., separating columns)
library(dplyr)        # For data manipulation
library(tidyverse)    # Includes ggplot2, dplyr, tidyr, etc. (some redundancy here)
library(ggtree)       # For visualizing phylogenetic trees
library(treeio)       # For reading tree formats like IQ-TREE, Newick, etc.
library(ggplot2)      # For plotting
library(phangorn)     # For midpoint rooting of trees

## Load metadata from Excel
metadata <- read_xlsx("cox2_sequence_metadata.xlsx", sheet = 1)
allData <- read_xlsx("cox2_sequence_metadata.xlsx", sheet = 2)    

# Merge metadata tables by 'Acession_no' and separate 'Country' column into 'Country' and 'Location'
coxMetadata <- left_join(metadata, allData, by = "Acession_no") %>% 
  separate(Country, into = c("Country", "Location"), sep = ":", extra = "merge", fill = "right")

## Import phylogenetic tree from IQ-TREE output (Newick format)
cox2_tree <- read.iqtree("cox2.treefile")

if (inherits(cox2_tree, "treedata")) {
  cox2_tree <- cox2_tree@phylo
}

# Midpoint root the tree to improve visualization
cox2_tree <- midpoint(cox2_tree)

# Merge tree tip labels with metadata using Acession_no
coxtree_data <- full_join(as_tibble(cox2_tree), coxMetadata, by = c("label" = "Acession_no"))

# Define custom colors for each country
country_colors <- c(
  "Brazil" = "blue",
  "Burundi" = "#D95F02",
  "Democratic Republic of the Congo" = "purple",
  "Ecuador" = "#E7298A",
  "Kenya" = "black",
  "Madagascar" = "#00AB66"
)

# Plot the phylogenetic tree 
Figure4a <- ggtree(cox2_tree, layout = "rectangular") %<+% coxMetadata +  
  geom_tippoint(aes(color = Country), size = 4) +                         
  scale_color_manual(values = country_colors) +                          
  theme_void() +                                                         
  theme(
    legend.position = "right",                                           
    legend.title = element_text(face = "bold", size = 13, hjust = 0.5, 
                                margin = margin(b = 5), color = "black"), 
    legend.text = element_text(size = 12, face = "bold"),                
    legend.background = element_rect(fill = "gray100", color = "black", linetype = "blank"),  
    legend.key = element_rect(fill = "gray100"),                         
    legend.spacing.y = unit(0.5, "cm")                                   
  ) +
  guides(
    color = guide_legend(title.position = "top")                         
  ) +
  xlim(0, max(cox2_tree$edge.length) * 1.5)                               

# Save the figure
ggsave("Figure4a.tiff", plot = Figure4a, width = 10, height = 8, dpi = 600, units = "in", device = "jpeg")
