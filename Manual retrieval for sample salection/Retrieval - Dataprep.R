if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, stopwords)

# Data import ------------------------------------------------------------------
UmbriaPress <- read_rds('Data/UmbriaPress.RDS')

# Data cleaning ----------------------------------------------------------------
## Filtering ----
### Defining dictionaries ----
Industry <- c('acciaieria', 'industria', 'acciaio', 'Arvedi', 'Thyssen',
              'Thyssen-Krupp') |> 
  paste(collapse = '|')
Transportation <- c('treno', 'aeroporto', 'Trenitalia',
              'ciclabile', 'mobilità', 'BRT', 'trasporti') |> 
  paste(collapse = '|')

### Keeping only matching articles ----
UmbriaPressInd <- UmbriaPress |> # industry
  filter(str_detect(text, regex(Industry, ignore_case = T))) |> 
  mutate(keyword = 'Industry')

UmbriaPressTrans <- UmbriaPress |> # transportation
  filter(str_detect(text, regex(Transportation, ignore_case = T))) |> 
  mutate(keyword = 'Transportation')

### Merge dataframes and handle overlapping articles ----
UmbriaPressFilt <- bind_rows(UmbriaPressInd, UmbriaPressTrans) |>
  group_by(doc_id) |>
  mutate(keyword = if_else(n() > 1, 'both', first(keyword))) |>
  ungroup() |>
  distinct(doc_id, .keep_all = TRUE) # Remove duplicate rows

# NLP dataprep -----------------------------------------------------------------
stopwords_vec <- stopwords(language = 'it')
UPTok <- UmbriaPressFilt |> 
  mutate(text = str_replace_all(text, "[\'’](?!\\s)", "' ")) |> # adjust tokeniser for Italian
  unnest_tokens(token, text) |> # tokenisation
  filter(!token %in% stopwords_vec) |> # deleting stopwords
  filter(token != 'n' & token != 'tm') |> # deleting artifacts
  filter(!str_detect(token, regex('\\d'))) |> # deleting numbers
  filter(!str_detect(token, regex('[[:punct:][:digit:]\\p{S}]'))) |> # deleting punctuation
  filter(str_length(token) <= 15) |> # deleting impossibly long words
  filter(str_length(token) > 4) # deleting impossibly short words

# Save dataset -----------------------------------------------------------------
saveRDS(UPTok, 'Data/UPTok_retrieval.RDS')
