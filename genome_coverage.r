# Load packages
library(ggplot2)     # For plotting
library(viridis)     # For color palettes that are perceptually uniform
library(dplyr)       # For data manipulation
library(readr)       # For reading data efficiently
library(ggbreak)     # For axis breaks (e.g., y-axis compression)

# Load RNA-seq read depth data
Rnaseq_coverage <- read.csv("RNASeq_coverage.csv")  

# Load gene annotation data 
gene_annotations <- read_csv("tunga_gene_locations.csv")  

# Load a second nanopore reads coverage
coverage_ref <- read.csv("neosome_coverage.csv")

# Filter gene annotations to only include protein-coding and rRNA genes
protein_coding_genes <- gene_annotations %>% 
  filter(Gene_type %in% c("Protein_coding", "rRNA"))

# Define a color palette using viridis (option "D" is colorblind-friendly)
viridis_colors <- viridis(20, option = "D")  # Generates a palette of 20 colors

# Create the coverage plot for both nanopore and RNA-seq data
Figure2 <- ggplot() +
  geom_line(data = Rnaseq_coverage, aes(x = Position, y = Coverage), 
            color = "blue", size = 1.2) +
  geom_line(data = coverage_ref, aes(x = Position, y = Coverage), 
            color = "red", size = 0.8) +
  theme_light() +
  labs(x = "Genome position", y = "Read depth") +
  geom_rect(data = protein_coding_genes, 
            aes(xmin = Start, xmax = Stop, ymin = 0, 
                ymax = max(Rnaseq_coverage$Coverage) * 1.05, fill = Gene), 
            alpha = 0.3, inherit.aes = FALSE) +
  geom_text(data = protein_coding_genes, 
            aes(x = (Start + Stop) / 2, 
                y = max(Rnaseq_coverage$Coverage) * 0.95, 
                label = Gene), 
            angle = 90, vjust = 0.5, hjust = 0.5, 
            size = 4, inherit.aes = FALSE) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 12, face = "bold", colour = "black"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none") +  
  scale_fill_viridis_d(option = "D") +
  scale_y_break(c(8000, 15000), scales = 0.5)

# Display the plot
print(Figure2)

# Save the plot to a high-resolution TIFF file
ggsave("Figure 2.tiff", plot = Figure2, 
       width = 14, height = 7, dpi = 300, units = "in", device = "tiff")
