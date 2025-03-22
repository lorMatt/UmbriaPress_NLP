if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, ldatuning, quanteda, topicmodels)

# Import data ------------------------------------------------------------------
UPTok <- readRDS('Data/UPTok_retrieval.RDS')
UPTok_sent <- readRDS('Models/Retrieval_UPTok_sent.RDS')
## Filter only environment related articles
UPTok_filt <- UPTok |>
  filter(Environment == 1)

## convert to dtm
UPTok_dtm <- UPTok_filt |>
  filter(newspaper == "Corriere dell'Umbria") |>
  add_count(doc_id, token) |> 
  cast_dtm(document = doc_id, term = token, value = n)

# LDA hyperparameter tuning ----------------------------------------------------
determine_k <- FindTopicsNumber(
  UPTok_dtm,
  topics = seq(from = 2, to = 64, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77)
)

## Visualisation ----
FindTopicsNumber_plot(determine_k)

# comment: Deveaud adds little information. 20 seems to be the optimal amount
# of topics.

# Modeling ---------------------------------------------------------------------
## defining model ----
ret_lda_K10 <- LDA(UPTok_dtm, 10, control = list(seed = 123))
## extracting topic by token
ret_lda_K10_tokens <- tidy(ret_lda_K10)
## extracting theta
ret_lda_K10_docs <- tidy(ret_lda_K10, matrix = 'gamma') |> 
  mutate(doc_id = as.numeric(document)) |> 
  inner_join(UPTok_sent |> select(!token:polarity) |> unique())

ret_lda_K10_docs |> 
  pivot_wider(names_from = topic, values_from = gamma) |> 
  View()

# Saving resuts
write_rds(determine_k, 'Models/ret_lda_tuning.RDS')
write_rds(ret_lda_K10_tokens, 'Models/ret_lda_K10_tokens.RDS')
write_rds(ret_lda_K10_docs, 'Models/ret_lda_K10_docs.RDS')
