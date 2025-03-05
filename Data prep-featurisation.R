# Load required packages (using pacman)
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, spacyr, reticulate, stopwords)

# Import corpus and add doc_id -------------------------------------------------
UmbriaPress <- readRDS('Corpus.RDS') |> mutate(doc_id = row_number())

# Parse corpus with spaCy ------------------------------------------------------
## Initialize spaCy ----
spacy_initialize(model = "it_core_news_sm", entity = TRUE)
## Parse ----
UmbriaPressTok <- spacy_parse(UmbriaPress |> select(doc_id, text), entity = FALSE)

# Filter tokens: remove stopwords, punctuation, numbers, spaces, and symbols ----
## Define stopwords vector ----
stopwords_vec <- stopwords(language = 'it')
UmbriaPressTok <- UmbriaPressTok |> 
  filter(!lemma %in% stopwords_vec) |> 
  filter(!pos %in% c('NUM', 'SYM', 'SPACE', 'PUNCT')) |> 
  filter(!str_detect(lemma, regex('[[:punct:][:digit:]\\p{S}]')))

## Calculate tf-idf ----
UmbriaPress_tfidf <- UmbriaPressTok |> 
  count(doc_id, lemma, sort = TRUE) |> 
  bind_tf_idf(lemma, doc_id, n)

## Filter documents with less than 50 tokens ----
UmpriaPress_tfidf <- UmbriaPress_tfidf |> 
  filter(!doc_id %in% (UmbriaPressTok |> group_by(doc_id) |> summarise(token = n()) |> filter(token <= 50))$doc_id)

## Filter lemmas with tf < .2 and 1 < n <= 200 ----
UmbriaPress_tfidf <- UmbriaPress_tfidf |> 
  filter(tf < .2, n > 1, n <= 200)

## Calculate mean tf-idf for each lemma ----
tfidf_aggregated <- UmbriaPress_tfidf |> 
  group_by(lemma) |> 
  summarise(mean_tfidf = mean(tf_idf))

## Define tf-idf threshold ----
threshold <- 0.05

## Filter lemmas with mean tf-idf above threshold ----
UPress_filt <- UmbriaPress_tfidf |> 
  filter(lemma %in% (tfidf_aggregated |> filter(mean_tfidf > threshold))$lemma)

# Merge metadata ---------------------------------------------------------------
UmbriaPressFeat <- UmbriaPress |> 
  select(!text) |> 
  mutate(doc_id = as.character(doc_id)) |> 
  right_join(UPress_filt)

# Save data --------------------------------------------------------------------
write_rds(UmbriaPressFeat, 'UmbriaPressFeat.RDS')
write_rds(UmbriaPress, 'UmbriaPress.RDS')