if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, ggrepel, patchwork)

# Import data
TR_emb_mat <- read_rds('Models/TR_emb_mat.RDS')
PG_emb_mat <- read_rds('Models/PG_emb_mat.RDS')
## PCA ----
### queries ----
Environment <- c('emissioni', 'inquinamento', 'riuso',
                 'ecosistema', 'rifiuti', 'inceneritore')
Industry <- c('industria', 'acciaio')
Transportation <- c('treno', 'aeroporto', 'ciclabile', 'mobilità', 'trasporti')
Query <- append(Environment, Industry) |> 
  append(Transportation)
### models ----
TR_PCR <- prcomp(TR_emb_mat[Query, ]) |>
  pluck('x') |> 
  as.data.frame() |> 
  rownames_to_column('term') |> 
  ggplot(aes(PC1, PC2)) +
  geom_label_repel(aes(label = term)) +
  labs(title = 'Terni')

PG_PCR <- prcomp(PG_emb_mat[Query, ]) |>
  pluck('x') |> 
  as.data.frame() |> 
  rownames_to_column('term') |> 
  ggplot(aes(PC1, PC2)) +
  geom_label_repel(aes(label = term)) +
  labs(title = 'Perugia')

TR_PCR + PG_PCR &
  plot_annotation(title = 'Semantic fields in city sub-corpora')
