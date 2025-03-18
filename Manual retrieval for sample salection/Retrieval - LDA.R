if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, ldatuning, quanteda, seededlda, scales)

# Import data ------------------------------------------------------------------
UPTok <- readRDS('Data/UPTok_retrieval.RDS')

## convert to dtm
UPTok_dtm <- UPTok |> 
  add_count(keyword, token) |> 
  cast_dtm(document = doc_id, term = token, value = n)

# LDA hyperparameter tuning ----------------------------------------------------
determine_k <- FindTopicsNumber(
  UPTok_dtm,
  topics = seq(from = 2, to = 4, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77)
)

## Visualisation ---------------------------------------------------------------
FindTopicsNumber_plot(determine_k)

# comment: Deveaud penalises LDAs with a high k. since k=20 seems to be the
# point at which the other metrics start stabilising, I will estimate models
# multiple models with k[18,24] and qualitatively assess the results.
