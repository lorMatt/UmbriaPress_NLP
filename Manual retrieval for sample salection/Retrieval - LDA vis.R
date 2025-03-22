if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, scales)

# Import data ------------------------------------------------------------------
determine_k <- read_rds('Models/ret_lda_tuning.RDS')
ret_lda_K10_tokens <- read_rds('Models/ret_lda_K10_tokens.RDS')
ret_lda_K10_docs <- read_rds('Models/ret_lda_K10_docs.RDS')


# Visualisation ----------------------------------------------------------------
## settings ----
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


## LDA tuning ----
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
  guides(shape = guide_legend(override.aes = list(size = 4))) +
  theme(axis.title = element_blank(),
        legend.position = 'bottom')
## save
ggsave('Plots/ret_lda_tuning_gg.pdf', lda_tuning_gg, width = 8, height = 5)

## Top terms ----

ret_lda_K10_tokens |> 
  group_by(topic) |> 
  slice_max(beta, n = 5) |> 
  ungroup() |> 
  arrange(topic, -beta) |> 
  mutate(term = reorder_within(term, beta, topic)) |> 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

ret_lda_K10_docs |>
  pivot_wider(names_from = topic, values_from = gamma) |> 
  rowwise() |>
  mutate(max_gamma = which.max(c_across(`1`:`10`))) |> 
  # filter(max_gamma == 2) |> 
  View()
  group_by(month, city) |> 
  count(max_theta) |> 
  group_by(month, city) |> 
  mutate(ratio = n/sum(n))