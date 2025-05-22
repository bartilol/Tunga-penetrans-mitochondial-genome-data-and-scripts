# Load required R packages for phylogenetics and plotting
library(ggtree)      # For visualizing phylogenetic trees using ggplot2
library(treeio)      # For importing and handling phylogenetic trees
library(ggplot2)     # For general plotting
library(dplyr)       # For data manipulation
library(phangorn)    # Provides the midpoint() function for rooting trees

# Read in a tree file produced by IQ-TREE in Newick format
tree <- read.iqtree("flea.treefile")
if (inherits(tree, "treedata")) {
  tree <- tree@phylo
}

# Read in metadata 
metadata <- read.csv("metadata2.csv")

# Midpoint root the tree 
tree <- midpoint(tree)

# Convert tree to tibble and merge it with metadata
tree_data <- full_join(as_tibble(tree), metadata, by = c("label" = "Name"))

# Build the phylogenetic tree
Figure3 <- ggtree(tree, layout = "rectangular") %<+% metadata +  
  geom_tippoint(aes(color = Family, shape = Order), size = 6) +  
  geom_tiplab(aes(label = paste0("bolditalic('", Names2, "')")),  
              hjust = -0.2, size = 6, parse = TRUE) +  
  scale_color_brewer(palette = "Dark2") +  
  theme_void() +  
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold", size = 14, hjust = 0.5, 
                                margin = margin(b = 5)),  
    legend.text = element_text(size = 12),  
    legend.spacing.x = unit(2, "cm"),  
    legend.key = element_rect(fill = "gray90"), 
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank()    # ✅ Moved inside the theme() block
  ) +
  guides(
    color = guide_legend(title.position = "top"),  
    shape = guide_legend(title.position = "top")
  ) +
  xlim(0, max(tree$edge.length) * 1.3)


# Display the plot
print(Figure3)

# Save the plot 
ggsave("Figure 3.tiff", plot = Figure3, width = 13, height = 8, dpi = 300, units = "in", device = "tiff")
