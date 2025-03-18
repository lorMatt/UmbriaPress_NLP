if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, stopwords, SnowballC, ggridges)

# Import data ------------------------------------------------------------------
UmbriaPress <- read_rds('Data/UmbriaPress.RDS')
docs_LDA_class <- read_rds('Models/docs_LDA_K25_class.RDS') |> 
  mutate(doc_id = as.numeric(doc_id))

# Data wrangling ---------------------------------------------------------------
target_topics <- c('Industry', 'Pollution', 'Transportation')

docs_LDA_target <- docs_LDA_class |> 
  select(!Industry:other20) |> 
  filter(max_theta %in% target_topics) |> 
  inner_join(UmbriaPress)

# Dataprep ---------------------------------------------------------------------
docs_LDA_target_tok <- docs_LDA_target |> 
  unnest_tokens(word, text, token = 'words') |> # tokenisation
  filter(!word %in% stopwords(language = 'it')) |> # stopwords removal
  filter(!str_detect(word, "[0-9]")) |> # removing short words
  mutate(word = wordStem(word, language = "it")) # stemming

# Dictionary sentiment analysis ------------------------------------------------
## import lexicon
lexPos <- read_delim('Lexicon/readable_pos_words_list.txt', delim = '\t', col_names = 'word') |> 
  separate_wider_delim(word, ' ', names = c('word', 'lang')) |> 
  filter(lang == 'it' & word != 'di' & word != 'come' & word != 'molto' & word != 'modo') |> 
  select(!lang) |> 
  mutate(polarity = 'pos', stemmed = wordStem(word, language = 'it')) |> 
  filter(stemmed != 'lavor' & stemmed != 'tutt' & stemmed != 'chiar' & stemmed != 'qui' & stemmed != 'rispett')


lexNeg <- read_delim('Lexicon/readable_neg_words_list.txt', delim = '\t', col_names = 'word') |> 
  separate_wider_delim(word, ' ', names = c('word', 'lang')) |> 
  filter(word != 'fatto' & word != 'trovata' & word != 'trovarsi') |> 
  mutate(polarity = 'neg', stemmed = wordStem(word, language = 'it')) |> 
  filter(lang == 'it' & stemmed != 'rifiut' & stemmed != 'rif' & stemmed != 'in') |> 
  select(!lang) |> 
  filter(stemmed != 'comun' & stemmed != 'tutt' & stemmed != 'chiar' & stemmed != 'par')

lex <- bind_rows(lexPos, lexNeg)

## Analysis ----
docs_LDA_target_sent <- docs_LDA_target_tok |> 
  inner_join(lex, join_by(word == stemmed), relationship = 'many-to-many') 

docs_LDA_art_sent <- docs_LDA_target_sent|> 
  group_by(doc_id) |> 
  count(polarity) |> # count positive, negative words
  pivot_wider(names_from = polarity,
              values_from = n) |> 
  mutate(ratio = (pos - neg)/(pos + neg)) |> # compute pos/neg ratio
  inner_join(docs_LDA_target_tok) # retrieve all other info from original DF

# Data export ------------------------------------------------------------------
write_rds(docs_LDA_art_sent, 'Models/docs_LDA_art_sent.RDS')
write_rds(docs_LDA_target_sent, 'Models/docs_LDA_target_sent.RDS')
