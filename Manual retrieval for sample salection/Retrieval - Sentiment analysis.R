if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext)

# Import data ------------------------------------------------------------------
UPTok <- readRDS('Data/UPTok_retrieval.RDS')

# Dictionary sentiment analysis ------------------------------------------------
## import lexicon ----
lexPos <- read_delim('Lexicon/readable_pos_words_list.txt', delim = '\t', col_names = 'token') |> 
  separate_wider_delim(token, ' ', names = c('token', 'lang')) |> 
  filter(lang == 'it' & !token %in% c('chiaro', 'chiaramente', 'chiarire', 'chiarezza',
                                     'anzitutto', 'tutto', 'lavorare', 'lavorato', 'modo',
                                     'molto', 'come', 'di')) |> 
  select(!lang) |> 
  mutate(polarity = 'pos')


lexNeg <- read_delim('Lexicon/readable_neg_words_list.txt', delim = '\t', col_names = 'token') |> 
  separate_wider_delim(token, ' ', names = c('token', 'lang')) |> 
  filter(lang == 'it' & !token %in% c('fatto', 'trovata', 'trovarsi', 'in', 'sin',
                                     'coinvolto', 'comune', 'pari')) |> 
  select(!lang) |> 
  mutate(polarity = 'neg')

lex <- bind_rows(lexPos, lexNeg)

## analysis ----
UPTok_sent <- UPTok |> 
  inner_join(lex)
UPTok_sent <- UPTok_sent |> 
  group_by(doc_id) |> 
  count(polarity) |> # count positive, negative words
  pivot_wider(names_from = polarity,
              values_from = n) |> 
  mutate(ratio = (pos - neg)/(pos + neg)) |> # compute pos/neg ratio
  inner_join(UPTok_sent) # retrieve all other info from original DF

# Save results
write_rds(UPTok_sent, 'Models/Retrieval_UPTok_sent.RDS')
