if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, ggrepel, patchwork)

# Import data
TR_emb_mat <- read_rds('Models/TR_emb_mat.RDS')
PG_emb_mat <- read_rds('Models/PG_emb_mat.RDS')
PCA <- read_rds('Models/PCA.RDS')

# Visualisation
## settings ----
pal <- c(
  "#FDA638",
  "#459395",
  "#EB7C69",
  '#2BE19E',
  '#972F5A',
  '#121333'
)
na_col <- "gray75"

### theming ----
theme_set(theme(panel.background = element_blank(),
                plot.title = element_text(face = 'bold'),
                axis.ticks = element_blank(),
                legend.title = element_blank(),
                panel.grid.major = element_line(linetype = 'solid',
                                                colour = 'gray97',
                                                linewidth = .3),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(colour = 'gray25'),
                axis.line.y = element_line(colour = 'gray25'),
                strip.background = element_blank()
))


## PCA ----

PCA_gg <- PCA |> 
  ggplot(aes(PC1, PC2, label = term, fill = query)) +
  geom_label_repel(colour = 'white') +
  facet_wrap(~city) +
  scale_fill_manual(values = pal)

ggsave('Plots/PCA_gg.pdf', PCA_gg, width = 12, height = 6)
