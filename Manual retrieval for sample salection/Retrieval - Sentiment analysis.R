if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext)

# Import data ------------------------------------------------------------------
UPTok <- readRDS('Data/UPTok_retrieval.RDS')
UPngrams <- readRDS('Data/UPngrams_retrieval.RDS')

# Dictionary sentiment analysis ------------------------------------------------
## import lexicon ----
lexPos <- read_delim('Lexicon/readable_pos_words_list.txt', delim = '\t', col_names = 'token') |> 
  separate_wider_delim(token, ' ', names = c('token', 'lang')) |> 
  filter(lang == 'it' & !token %in% c('chiaro', 'chiaramente', 'chiarire', 'chiarezza',
                                     'anzitutto', 'tutto', 'lavorare', 'lavorato', 'modo',
                                     'molto', 'come', 'di', 'servizi', 'nuovo', 'economia', 'valore', 'rispetto')) |> 
  select(!lang) |> 
  mutate(polarity = 'pos')


lexNeg <- read_delim('Lexicon/readable_neg_words_list.txt', delim = '\t', col_names = 'token') |> 
  separate_wider_delim(token, ' ', names = c('token', 'lang')) |> 
  filter(lang == 'it' & !token %in% c('fatto', 'trovata', 'trovarsi', 'in', 'sin',
                                     'coinvolto', 'comune', 'pari', 'rifiuti', 'tempo')) |> 
  select(!lang) |> 
  mutate(polarity = 'neg')

lex <- bind_rows(lexPos, lexNeg)

## document-level analysis ----
UPTok_sent <- UPTok |> 
  inner_join(lex)
UPTok_sent <- UPTok_sent |> 
  group_by(doc_id) |> 
  count(polarity) |> # count positive, negative words
  pivot_wider(names_from = polarity,
              values_from = n) |> 
  mutate(ratio = (pos - neg)/(pos + neg)) |> # compute pos/neg ratio
  inner_join(UPTok_sent) # retrieve all other info from original DF


## n-grams ----
UPngrams_sent <- UPngrams |> 
  filter(str_detect(token1, regex("emissioni|PM10|inquinamento|ecolog|riuso|ecosistem|rifiuti|inceneritor", ignore_case = T)) |
           str_detect(token2, regex("emissioni|PM10|inquinamento|ecolog|riuso|ecosistem|rifiuti|inceneritor", ignore_case = T)),
         !str_detect(token1, regex("ginecolog", ignore_case = T)) &
           !str_detect(token2, regex("ginecolog", ignore_case = T)))
UPngrams_sent <- UPngrams_sent |>
  inner_join(lex, by = join_by('token2' == 'token')) |> 
  bind_rows(UPngrams_sent |> inner_join(lex, by = join_by('token1' == 'token'))) |>
  distinct() |>
  mutate(query = case_when(str_detect(token1, "emissioni|PM10") ~ "Emissions",
                           str_detect(token1, "inquinamento") ~ "Pollution",
                           str_detect(token1, "ecolog|ecosistem") ~ "Ecology",
                           str_detect(token1, "riuso|rifiuti|inceneritor") ~ "Waste management",
                           str_detect(token2, "emissioni|PM10") ~ "Emissions",
                           str_detect(token2, "inquinamento") ~ "Pollution",
                           str_detect(token2, "ecolog|ecosistem") ~ "Ecology",
                           str_detect(token2, "riuso|rifiuti|inceneritor") ~ "Waste management"
  )) |> 
  group_by(query, city) |> 
  count(polarity) |> 
  ungroup() |> 
  pivot_wider(names_from = polarity, values_from = n) |> 
  mutate(neg = ifelse(is.na(neg), 0, neg),
         pos = ifelse(is.na(pos), 0, pos),
         ratio = (pos - neg) / (pos + neg))

## Save results ----
write_rds(UPTok_sent, 'Models/Retrieval_UPTok_sent.RDS')
write_rds(UPngrams_sent, 'Models/Retrieval_UPngrams_sent.RDS')
  
