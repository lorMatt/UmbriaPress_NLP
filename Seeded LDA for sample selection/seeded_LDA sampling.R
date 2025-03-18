if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, ldatuning, quanteda, seededlda, scales)

# Import data ------------------------------------------------------------------
UmbriaPressFeat <- read_rds('Data/UmbriaPressFeat.RDS')

## convert to dtm
UP_dtm <- UmbriaPressFeat |> 
  cast_dtm(document = doc_id, term = lemma, value = n)

# LDA hyperparameter tuning ----------------------------------------------------
determine_k <- FindTopicsNumber(
  UP_dtm,
  topics = seq(from = 2, to = 250, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77)
)

## Visualisation ---------------------------------------------------------------
FindTopicsNumber_plot(determine_k)

# comment: Deveaud penalises LDAs with a high k. since k=20 seems to be the
# point at which the other metrics start stabilising, I will estimate models
# multiple models with k[18,24] and qualitatively assess the results.

# Seeded LDA ----
## Cast dfm ----
UP_dfm <- UmbriaPressFeat |>
  cast_dfm(document = doc_id, term = lemma, value = n)

## define dictionary -----
dict <- dictionary(
  list(
    Industry = c('acciaieria', 'Ast', 'industria', 'acciaio', 'Arvedi', 'Thyssen', 'Thyssen-Krupp', 'lavoro'),
    Transportation = c('treno', 'aeroporto', 'Trenitalia', 'Minimetrò', 'bicicletta', 'traffico', 'ciclabile', 'mobilità', 'BRT', 'autobus'),
    Pollution = c('Legambiente', 'aria', 'acqua', 'suolo', 'inquinamento', 'ambiente'),
    Narcotics = c('grammo', 'cocaina', 'hashish', 'eroina', 'marijuana'),
    Weather = c('venti', 'temperature', 'nuvoloso', 'precipitazione')
  )
)

## model ----
seed_LDA <- textmodel_seededlda(UP_dfm, 
                                   dict, 
                                   residual = 20,
                                   batch_size = 0.01,
                                   auto_iter = TRUE)


# Get document topics ----------------------------------------------------------
docs_LDA <- rownames(UP_dfm) |> 
  enframe(name = NULL, value = "doc_id") |> 
  bind_cols(seed_LDA$theta |> as_tibble()) |> 
  left_join(UmbriaPressFeat)

# Save results ----
write_rds(seed_LDA, 'Models/seed_LDA_k25.RDS')
write_rds(determine_k, 'Models/lda_tuning.RDS')
write_rds(docs_LDA, 'Models/docs_LDA_k25.RDS')
write_rds(dict, 'Models/dict_LDA_k25.RDS')
