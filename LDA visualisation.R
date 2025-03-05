if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, scales, ggrepel)

# Import data ------------------------------------------------------------------
UmbriaPressFeat <- read_rds('Data/UmbriaPressFeat.RDS')

## convert to dtm
UP_dfm <- UmbriaPressFeat |> 
  cast_dfm(document = doc_id, term = lemma, value = n)

# Import model data ------------------------------------------------------------
seed_LDA <-    read_rds('Models/seed_LDA_k25.RDS')
determine_k <- read_rds('Models/lda_tuning.RDS')
docs_LDA <-    read_rds('Models/docs_LDA_k25.RDS')
dict <-        read_rds('Models/dict_LDA_k25.RDS')

# Visualisation settings -------------------------------------------------------
### palette ----
pal <- c(
  "#FDA638",
  "#459395",
  "#EB7C69",
  '#2BE19E',
  '#972F5A',
  '#121333'
)
na_col <- "gray85"

### theming ----
theme_set(theme(panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.title = element_blank(),
                panel.grid.major = element_line(linetype = 'solid',
                                                colour = 'gray97',
                                                linewidth = .3),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(colour = 'gray25'),
                axis.line.y = element_line(colour = 'gray25'),
                strip.background = element_blank()
))


# Hyperparameter tuning --------------------------------------------------------

determine_k <- read_rds('Models/lda_tuning.RDS')

determine_k |> 
  mutate(across(c(Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014), ~rescale(., to = c(0, 1)))) |> 
  pivot_longer(cols = c(Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014),
               names_to = 'index') |> 
  mutate(maxmin = ifelse(index %in% c('Griffiths2004', 'Deveaud2014'), 'Maximise', 'Minimise')) |> 
  ggplot(aes(topics, value, shape = index)) +
  geom_point(data = ~. |> filter(maxmin == 'Maximise'), colour = pal[6]) +
  geom_point(data = ~. |> filter(maxmin != 'Maximise'), colour = pal[5]) +
  geom_line(data = ~. |> filter(maxmin == 'Maximise'), colour = pal[6]) +
  geom_line(data = ~. |> filter(maxmin != 'Maximise'), colour = pal[5]) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~factor(maxmin, levels = c('Minimise', 'Maximise')),
             scales = 'free', dir = 'v') +
  theme(axis.title = element_blank(),
        legend.position = 'bottom')


# Visualise words --------------------------------------------------------------
seed_LDA <- read_rds('Models/seed_LDA_k25.RDS')
## get word phi ----
topic_words <- seed_LDA |> 
  pluck("phi") |> 
  t() |> 
  as_tibble(rownames = NA) |> 
  rownames_to_column("term") |> 
  pivot_longer(-term) |> 
  group_by(name) |> 
  slice_max(value, n = 10) 

## visualise ----
topic_words_gg <- topic_words |> 
  mutate(name = factor(name,
                       levels = ((append(names(dict), paste0('other', 1:20))))
                       )) |>
  ggplot(aes(value, reorder(term, value))) +
  geom_col(data = ~. |> filter(name %in% names(dict)),
           aes(fill = name)) +
  geom_col(data = ~. |> filter(!name %in% names(dict)),
           fill = na_col) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = pal) +
  facet_wrap(~name, scales = 'free') +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.line.x = element_blank())
## save
ggsave('Plots/topic_words_gg.png', topic_words_gg, width = 12, height = 9)

# Document topics --------------------------------------------------------------
docs_LDA <- readRDS('Models/docs_LDA_K25.RDS')

docs_topics_gg <- docs_LDA |> 
  mutate(month = floor_date(date, unit = 'quarter'),
         city = case_match(city,
                           'PG' ~ 'Perugia',
                           'TR' ~ 'Terni')) |> 
  group_by(month, city) |> 
  summarise(across(Industry:other20, mean)) |> 
  pivot_longer(cols = Industry:other20, names_to = 'Topic', values_to = 'Strength') |> 
  # filter(Strength <=.35) |> 
  ggplot(aes(month, Strength, colour = Topic)) +
  geom_jitter(data = ~. |> filter(!Topic %in% names(dict)),
             colour = na_col, alpha = .7) +
  geom_line(data = ~. |> filter(Topic %in% names(dict)),
              aes(colour = Topic)) +
  geom_point(data = ~. |> filter(Topic %in% names(dict)),
            aes(colour = Topic), size = 2.2) +
  geom_point(data = ~. |> filter(Topic %in% names(dict)),
             colour = 'white', size = 1.2) +
  geom_text_repel(data = ~. |> filter(Topic %in% names(dict) & month == as_date('2025-01-01')),
            aes(label = Topic), hjust = -.05, direction = 'y') +
  facet_wrap(~city, ncol = 1, scales = 'free') +
  scale_x_date(limits = c(as_date('1 January 2011', format = "%d %B %Y"),
                          as_date('1 January 2025', format = "%d %B %Y")),
               expand = expansion(mult = c(0, 0.12)),
               date_breaks = '2 years',
               date_labels = '%Y') +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(0, .12),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        strip.text = element_text(size = 12))

## save
ggsave('Plots/docs_topics_gg.png', docs_topics_gg, width = 12, height = 8)
