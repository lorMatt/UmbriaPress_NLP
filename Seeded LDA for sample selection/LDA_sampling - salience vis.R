if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, scales, ggrepel, ggpattern)

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
cont_topics <- c('Weather', 'Narcotics')
target_topics <- c('Industry', 'Pollution', 'Transportation')

### palette ----
pal <- c(
  "#FDA638",
  "#459395",
  "#EB7C69",
  '#2BE19E',
  '#972F5A',
  '#121333'
)
na_col <- "gray75"

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

lda_tuning_gg <- determine_k |> 
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
## save
ggsave('Plots/lda_K25_tuning_gg.pdf', lda_tuning_gg, width = 7, height = 5)

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
  geom_col(data = ~. |> filter(name %in% target_topics),
           aes(fill = name)) +
  geom_col(data = ~. |> filter(name %in% cont_topics),
           fill = 'gray55') +
  geom_col(data = ~. |> filter(!name %in% names(dict)),
           fill = na_col) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = pal) +
  facet_wrap(~name, scales = 'free') +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.line.x = element_blank())
## save
ggsave('Plots/topic_words_gg.pdf', topic_words_gg, width = 12, height = 9)

# Document topics --------------------------------------------------------------
docs_LDA <- readRDS('Models/docs_LDA_K25.RDS')

docs_topics_gg <- docs_LDA |>
  mutate(month = floor_date(date, unit = 'quarter'),
         city = case_match(city,
                           'PG' ~ 'Perugia',
                           'TR' ~ 'Terni')) |> 
  filter(month > as_date('1 January 2011', format = "%d %B %Y")) |> 
  group_by(month, city) |> 
  summarise(across(Industry:other20, mean)) |> 
  pivot_longer(cols = Industry:other20, names_to = 'Topic', values_to = 'Strength') |> 
  ggplot(aes(month, Strength, colour = Topic)) +
  geom_jitter(data = ~. |> filter(!Topic %in% target_topics),
             colour = na_col, alpha = .3, size = .8) +
  geom_line(data = ~. |> filter(Topic %in% cont_topics),
            aes(group = Topic), colour = 'gray55', linewidth = .2) +
  geom_point(data = ~. |> filter(Topic %in% cont_topics),
             aes(group = Topic), colour = 'gray55', size = .8) +
  geom_line(data = ~. |> filter(Topic %in% target_topics),
              aes(colour = Topic)) +
  geom_area(data = ~. |> filter(Topic %in% target_topics),
            aes(fill = Topic),
            position = position_identity(),
            alpha = .1
            ) +
  geom_point(data = ~. |> filter(Topic %in% target_topics),
            aes(colour = Topic), size = 1.2) +
  geom_point(data = ~. |> filter(Topic %in% target_topics),
             colour = 'white', size = .2) +
  geom_text_repel(data = ~. |> filter(Topic %in% target_topics & month == as_date('2025-01-01')),
            aes(label = Topic), hjust = -.05, direction = 'y', force = 3.5, force_pull = 15) +
  # geom_text_repel(data = ~. |> filter(Topic %in% cont_topics & month == as_date('2025-01-01')),
  #                 aes(label = Topic), colour = na_col, hjust = -.05, direction = 'y', force = 2.5, force_pull = 3.5) +
  facet_wrap(~city, ncol = 1) +
  scale_x_date(expand = expansion(mult = c(0, 0.12)),
               date_breaks = '2 years',
               date_labels = '%Y') +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(0, .12),
                     expand = c(0,0)) +
  labs(title = 'Average topic strength',
       subtitle = 'Monthly data by city',
       caption = '"Control" topics are in gray') +
  theme(axis.title.x = element_blank(),
        # axis.line.y = element_blank(),
        legend.position = 'none',
        strip.text = element_text(size = 12, vjust = 1.1))

## save
ggsave('Plots/docs_topics_gg.pdf', docs_topics_gg, width = 12, height = 8)

# Document topic count ---------------------------------------------------------
## Data wrangling
# docs_LDA_class <- docs_LDA |> 
#   select(!lemma:tf_idf) |> 
#   unique() |> 
#   rowwise() |> 
#   mutate(max_theta = names(docs_LDA[1 + which.max(c_across(Industry:other20))]))
# write_rds(docs_LDA_class, 'Models/docs_LDA_K25_class.RDS')

docs_LDA_class <- read_rds('Models/docs_LDA_K25_class.RDS')

docs_LDA_month <- docs_LDA_class |>
  mutate(month = floor_date(date, unit = 'quarter'),
         city = case_match(city,
                           'PG' ~ 'Perugia',
                           'TR' ~ 'Terni')) |> 
  filter(month > as_date('1 January 2011', format = "%d %B %Y")) |> 
  group_by(month, city) |> 
  count(max_theta) |> 
  group_by(month, city) |> 
  mutate(ratio = n/sum(n))

## Visualisation ----
docs_class_ts_gg <-  docs_LDA_month |> 
  rename('Topic' = max_theta) |> 
  ggplot(aes(month, ratio, colour = Topic)) +
  geom_jitter(data = ~. |> filter(!Topic %in% target_topics),
              colour = na_col, alpha = .3, size = .8) +
  geom_line(data = ~. |> filter(Topic %in% cont_topics),
            aes(group = Topic), colour = 'gray55', linewidth = .2) +
  geom_point(data = ~. |> filter(Topic %in% cont_topics),
             aes(group = Topic), colour = 'gray55', size = .8) +
  geom_line(data = ~. |> filter(Topic %in% target_topics),
            aes(colour = Topic)) +
  geom_area(data = ~. |> filter(Topic %in% target_topics),
            aes(fill = Topic),
            position = position_identity(),
            alpha = .1
  ) +
  geom_point(data = ~. |> filter(Topic %in% target_topics),
             aes(colour = Topic), size = 1.2) +
  geom_point(data = ~. |> filter(Topic %in% target_topics),
             colour = 'white', size = .2) +
  geom_text(data = ~. |> filter(Topic %in% target_topics & month == as_date('2025-01-01')),
                  aes(label = Topic), hjust = -.05) +
  # geom_text_repel(data = ~. |> filter(Topic %in% cont_topics & month == as_date('2025-01-01')),
  #                 aes(label = Topic), colour = na_col, hjust = -.05, direction = 'y') +
  facet_wrap(~city, ncol = 1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand = expansion(mult = c(0, 0.12)),
               date_breaks = '2 years',
               date_labels = '%Y') +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(title = 'Number of articles per topic',
       subtitle = 'Ratio of article/monthly total by city',
       caption = '"Control" topics in gray') +
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        strip.text = element_text(size = 12, vjust = 1.1))

## save
ggsave('Plots/docs_class_ts_gg.pdf', docs_class_ts_gg, width = 12, height = 8)

