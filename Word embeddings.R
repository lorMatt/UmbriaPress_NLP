if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, wordsalad)

# Import data ------------------------------------------------------------------
UmbriaPressDet <- read_rds('Data/UmbriaPressDet.RDS')

## Data wrangling ----

### Splitting corpus ----
UPTR <- UmbriaPressDet |>
  filter(city == 'TR') |> 
  pluck(3)

UPPG <- UmbriaPressDet |>
  filter(city == 'PG') |> 
  pluck(3)


## word embeddings ----
TR_emb <- wordsalad::glove(UPTR |> str_remove_all(regex("[:punct:]|[0-9]")) |> str_to_lower())
TR_emb_mat <- as.matrix(TR_emb[, 2:11])
rownames(TR_emb_mat) <- TR_emb$tokens

PG_emb <- wordsalad::glove(UPPG |> str_remove_all(regex("[:punct:]|[0-9]|italiadescriptionimagehttpscitynewsperugiatodaystgyovh")) |> str_to_lower())
PG_emb_mat <- as.matrix(PG_emb[, 2:11])
rownames(PG_emb_mat) <- PG_emb$tokens

### save results
write_rds(TR_emb_mat, 'Models/TR_emb_mat.RDS')
write_rds(PG_emb_mat, 'Models/PG_emb_mat.RDS')

# Analysis ---------------------------------------------------------------------

## Function to get best matches, TR ----
TRnorms <- sqrt(rowSums(TR_emb_mat^2)) # calculate length of vectors for normalization

best_match_tr <- function(term, n = 5){
  x <- TR_emb_mat[term, ]
  cosine <- (TR_emb_mat %*% as.matrix(x)) / (TRnorms * sqrt(sum(x^2))) # calculate cosine similarities between term in question and all the others
  best_n <- order(cosine, decreasing = T)[1:n] #extract highest n cosines
  tibble(
    word = rownames(TR_emb_mat)[best_n], 
    cosine = cosine[best_n]
  )
}

## Function to get best matches, PG ----
PGnorms <- sqrt(rowSums(PG_emb_mat^2)) # calculate length of vectors for normalization

best_match_pg <- function(term, n = 5){
  x <- PG_emb_mat[term, ]
  cosine <- (PG_emb_mat %*% as.matrix(x)) / (PGnorms * sqrt(sum(x^2))) # calculate cosine similarities between term in question and all the others
  best_n <- order(cosine, decreasing = T)[1:n] #extract highest n cosines
  tibble(
    word = rownames(PG_emb_mat)[best_n], 
    cosine = cosine[best_n]
  )
}

## PCA ----
### queries ----
Environment <- c('emissioni', 'inquinamento', 'riuso',
                 'ecosistema', 'rifiuti', 'inceneritore')
Industry <- c('industria', 'acciaio', 'manifattura', 'produzione')
Transportation <- c('treno', 'aeroporto', 'ciclabile', 'mobilità', 'trasporti')
Query <- append(Environment, Industry) |> 
  append(Transportation)

### models ----
TR_PCA <- prcomp(TR_emb_mat[Query, ]) |>
  pluck('x') |> 
  as.data.frame() |> 
  rownames_to_column('term') |> 
  mutate(city = 'Terni',
         query = case_when(term %in% Environment ~ 'Environment',
                           term %in% Industry ~ 'Industry',
                           term %in% Transportation ~ 'Transportation'))

PG_PCA <- prcomp(PG_emb_mat[Query, ]) |>
  pluck('x') |> 
  as.data.frame() |> 
  rownames_to_column('term') |> 
  mutate(city = 'Perugia',
         query = case_when(term %in% Environment ~ 'Environment',
                           term %in% Industry ~ 'Industry',
                           term %in% Transportation ~ 'Transportation'))
PCA <- TR_PCA |> 
  bind_rows(PG_PCA)

write_rds(PCR, 'Models/PCA.RDS')
