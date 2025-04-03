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
Environment <- c('emissioni', 'PM10', 'inquinamento', 'ecolog', 'riuso',
               'ecosistem', 'rifiuti', 'inceneritor') |> 
  paste(collapse = '|')

### Matching dictionaries, flagging topics ----
UmbriaPressDet <- UmbriaPress |>
  mutate(Industry = ifelse(str_detect(text, regex(Industry, ignore_case = T)), 1, 0),
         Transportation = ifelse(str_detect(text, regex(Transportation, ignore_case = T)), 1, 0),
         Environment = ifelse(str_detect(text, regex(Environment, ignore_case = T)), 1, 0))

### Computing ratios ----
UmbriaPressSal <- UmbriaPressDet |> 
  mutate(semester = floor_date(date, unit = 'halfyears'),
         city = case_match(city,
                           'PG' ~ 'Perugia',
                           'TR' ~ 'Terni')) |> 
  filter(semester > as_date('1 January 2011', format = "%d %B %Y")) |> 
  group_by(semester, city) |> 
  count(Environment) |> 
  pivot_wider(names_from = Environment, values_from = n) |> 
  mutate(Environment = `1`/(`0`+`1`)) |> 
  select(Environment, semester, city)

UmbriaPressSal <- UmbriaPressDet |> 
  mutate(semester = floor_date(date, unit = 'halfyears'),
         city = case_match(city,
                           'PG' ~ 'Perugia',
                           'TR' ~ 'Terni')) |> 
  filter(semester > as_date('1 January 2011', format = "%d %B %Y")) |> 
  group_by(semester, city) |> 
  count(Transportation) |> 
  pivot_wider(names_from = Transportation, values_from = n) |> 
  mutate(Transportation = `1`/(`0`+`1`)) |> 
  select(Transportation, semester, city) |> 
  full_join(UmbriaPressSal)

UmbriaPressSal <- UmbriaPressDet |> 
  mutate(semester = floor_date(date, unit = 'halfyears'),
         city = case_match(city,
                           'PG' ~ 'Perugia',
                           'TR' ~ 'Terni')) |> 
  filter(semester > as_date('1 January 2011', format = "%d %B %Y")) |> 
  group_by(semester, city) |> 
  count(Industry) |> 
  pivot_wider(names_from = Industry, values_from = n) |> 
  mutate(Industry = `1`/(`0`+`1`)) |> 
  select(Industry, semester, city) |> 
  full_join(UmbriaPressSal) |> 
  pivot_longer(cols = !semester:city, names_to = 'topic', values_to = 'ratio')

# NLP dataprep -----------------------------------------------------------------
stopwords_vec <- stopwords(language = 'it')
## tokenisation ----
UPTok <- UmbriaPressDet |> 
  filter(Environment == 1 | Transportation == 1 | Industry == 1) |> 
  mutate(text = str_replace_all(text, "[\'’](?!\\s)", "' ")) |> # adjust tokeniser for Italian
  unnest_tokens(token, text) |> # tokenisation
  filter(!token %in% stopwords_vec, # deleting stopwords
        token != 'n' & token != 'tm', # deleting artifacts
        !str_detect(token, regex('\\d')), # deleting numbers
        !str_detect(token, regex('[[:punct:][:digit:]\\p{S}]')), # deleting punctuation
        str_length(token) <= 15, # deleting impossibly long words
        str_length(token) > 4) # deleting impossibly short words
  
UPngrams <- UmbriaPressDet |>
  filter(Environment == 1 | Transportation == 1 | Industry == 1) |> 
  mutate(text = str_replace_all(text, "[\'’](?!\\s)", "' "), # adjust tokeniser for Italian
         text = str_remove_all(text, regex('\\d'))) |> # deleting numbers
  unnest_tokens(token, text, token = 'ngrams', n = 2) |> 
  separate_wider_delim(cols = token, delim = ' ', names = c('token1', 'token2')) |> 
  filter(!token1 %in% stopwords_vec, # removing stopwords
         !token2 %in% stopwords_vec,
         !str_detect(token1, regex('[[:punct:][:digit:]\\p{S}]')), # removing punctuation/artifacts
         !str_detect(token2, regex('[[:punct:][:digit:]\\p{S}]')))


# Save dataset -----------------------------------------------------------------
saveRDS(UPTok, 'Data/UPTok_retrieval.RDS')
saveRDS(UPngrams, 'Data/UPngrams_retrieval.RDS')
saveRDS(UmbriaPressSal, 'Data/UmbriaPressSal.RDS')
saveRDS(UmbriaPressDet, 'Data/UmbriaPressDet.RDS')


