# Author: Arif Rahman
# Title: Nutritional Composition Analysis of Various Feed Types in R
# Description: This script performs a comprehensive analysis and visualization 
# of macronutrient contents (Protein, Fat, NDF, and TNC) in various feed types and parts. 
# It includes bar plots, heatmaps, boxplots, and ranking-based visualizations.

# =======================
# Load Required Libraries
# =======================
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(reshape2)
library(ggpubr)
library(RColorBrewer)
library(forcats)

# =======================
# Load and Explore the Data
# =======================
file_path <- "your_path.xlsx"  # Replace with your actual file path
df <- read_excel(file_path)

# =======================
# Bar Plot: Nutrient Composition by Feed Type
# =======================
ordered_feed <- c("Malaka (Ripe Fruit)", "Malaka (Half-Ripe Fruit)", "Malaka (Unripe Fruit)",
                  "Rambung Tapuk Pinang (Ripe Fruit)", "...")  # Fill with full names

df$Feed <- factor(df$Feed, levels = ordered_feed)

df_long <- pivot_longer(df, cols = c("Protein", "Fat", "NDF", "TNC"),
                        names_to = "Nutrient", values_to = "Content")
df_long$Nutrient <- factor(df_long$Nutrient, levels = c("NDF", "TNC", "Protein", "Fat"))

color_palette <- c("TNC" = "#D9BF77", "Protein" = "#2D6A4F", 
                   "NDF" = "#A7C957", "Fat" = "#1B4332")

plot_bar <- ggplot(df_long, aes(x = Feed, y = Content, fill = Nutrient)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), color = "black", width = 0.6) +
  scale_fill_manual(values = color_palette) +
  theme_bw() +
  labs(title = "Macronutrient Content Across Various Feed Types",
       x = "", y = "Macronutrient Content (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"))

ggsave("Barplot_Nutrients_All_Feed.png", plot = plot_bar, width = 12, height = 6, dpi = 300)

# =======================
# Heatmap: Nutrient Composition per Feed Type
# =======================
df_heatmap <- df[, c("Feed", "Protein", "Fat", "NDF", "TNC")]
df_melt <- melt(df_heatmap, id.vars = "Feed")
df_melt$variable <- factor(df_melt$variable, levels = c("NDF", "TNC", "Protein", "Fat"))

plot_heatmap <- ggplot(df_melt, aes(x = Feed, y = variable, fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  theme_bw() +
  labs(title = "Heatmap of Macronutrient Composition",
       x = "", y = "", fill = "% DM") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Heatmap_Nutrients.png", plot = plot_heatmap, width = 12, height = 6, dpi = 500)

# =======================
# Bar Plot: Top Nutrient per Category
# =======================
df_max <- df_long %>%
  group_by(Nutrient) %>%
  filter(Content == max(Content)) %>%
  arrange(Nutrient, desc(Content))

plot_max <- ggplot(df_max, aes(x = Feed, y = Content, fill = Nutrient)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  scale_fill_manual(values = color_palette) +
  theme_bw() +
  labs(title = "Feed Types with Highest Nutrient Content",
       x = "", y = "Content (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"))

ggsave("Barplot_Top_Nutrient_Content.png", plot = plot_max, width = 12, height = 6, dpi = 300)

# =======================
# Bar Plot: Top 5 Feeds per Nutrient
# =======================
df_top5 <- df_long %>%
  group_by(Nutrient) %>%
  slice_max(order_by = Content, n = 5) %>%
  ungroup()

plot_top5 <- ggplot(df_top5, aes(x = fct_reorder(Feed, Content), y = Content, fill = Nutrient)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6, color = "black") +
  geom_text(aes(label = round(Content, 1)), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = color_palette) +
  facet_wrap(~Nutrient, scales = "free_x", nrow = 1) +
  theme_bw() +
  labs(title = "Top 5 Macronutrient Contents by Feed Type",
       x = "", y = "Content (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
        strip.text = element_text(face = "bold"))

ggsave("Top5_Nutrient_Per_Feed.png", plot = plot_top5, width = 12, height = 6, dpi = 500)

# =======================
# Boxplots: Nutrient Content by Feed Parts
# =======================
data_parts <- read_excel("Macronutrients.xlsx")
data_parts$Part <- factor(data_parts$Part, levels = c(
  "Ripe Fruit", "Half-Ripe Fruit", "Unripe Fruit",
  "Ripe Seed", "Half-Ripe Seed",
  "Old Leaf", "Young Leaf",
  "Shoot", "Vegetative", "Cambium"
))

color_palette <- brewer.pal(n = length(levels(data_parts$Part)), name = "Set3")
theme_custom <- theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 11, face = "bold")
  )

plot_tnc <- ggplot(data_parts, aes(x = Part, y = TNC, fill = Part)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  labs(title = "TNC by Feed Part", x = "", y = "TNC (% DM)") +
  theme_custom

plot_protein <- ggplot(data_parts, aes(x = Part, y = Protein, fill = Part)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  labs(title = "Protein by Feed Part", x = "", y = "Protein (% DM)") +
  theme_custom

plot_fat <- ggplot(data_parts, aes(x = Part, y = Fat, fill = Part)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  labs(title = "Fat by Feed Part", x = "", y = "Fat (% DM)") +
  theme_custom

plot_ndf <- ggplot(data_parts, aes(x = Part, y = NDF, fill = Part)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  labs(title = "NDF by Feed Part", x = "", y = "NDF (% DM)") +
  theme_custom

combined_plot <- ggarrange(plot_tnc, plot_protein, plot_fat, plot_ndf,
                           ncol = 2, nrow = 2,
                           labels = c("A", "B", "C", "D"),
                           font.label = list(size = 14, face = "bold"))

ggsave("Boxplot_Nutrients_By_Feed_Part.jpeg", plot = combined_plot, width = 12, height = 8, dpi = 300)

# =======================
# Boxplot: Overall Nutrient Distribution
# =======================
data_long <- data_parts %>%
  pivot_longer(cols = c(Protein, Fat, NDF, TNC),
               names_to = "Nutrient", values_to = "Content") %>%
  filter(!is.na(Content))

plot_dominance <- ggplot(data_long, aes(x = Nutrient, y = Content, fill = Nutrient)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Overall Nutrient Concentration Comparison",
       x = "Nutrient Type", y = "Content (%)") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 15, face = "bold"),
    legend.position = "none"
  )

ggsave("Nutrient_Dominance.jpeg", plot = plot_dominance, width = 6, height = 5, dpi = 300)
