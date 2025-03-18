if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load()

# Import -----------------------------------------------------------------------
docs_LDA_art_sent <- read_rds('Models/docs_LDA_art_sent.RDS')
docs_LDA_target_sent <- read_rds('Models/docs_LDA_target_sent.RDS')

## Visualisation ---------------------------------------------------------------
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

## Pos-neg per topic by city ----
### option 1 ----
ratio_byCity_gg1 <- docs_LDA_art_sent |> 
  filter(max_theta == 'Transportation' | max_theta == 'Industry') |> 
  ggplot(aes(x = ratio, fill = city)) +
  geom_density(alpha = .3) +
  geom_boxplot(aes(y = -.5)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  facet_wrap(~max_theta, scales = 'free') +
  scale_x_continuous(limits = c(-1,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(-1,2), expand = c(0,0)) +
  scale_fill_manual(values = pal) +
  labs(title = 'Ratio between positive and negative word count',
       subtitle = 'Visualised by topic and city') +
  theme(legend.position = 'bottom',
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        strip.text = element_text(face = 'bold', vjust = 1))

ggsave('Plots/ratio_byCity_gg1.pdf', ratio_byCity_gg1, width = 10)

### option 2 ----
ratio_byCity_gg2 <- docs_LDA_art_sent |>
  filter(max_theta == 'Transportation' | max_theta == 'Industry') |> 
  select(!tag) |> 
  drop_na() |> 
  ggplot(aes(ratio, city, fill = city)) +
  geom_violin(alpha = .3) +
  geom_boxplot(width = .2) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  scale_fill_manual(values = pal) +
  facet_wrap(~max_theta, ncol = 1, strip.position = 'right') +
  labs(title = 'Ratio between positive and negative word count',
       subtitle = 'Visualised by topic and city') +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        axis.line.x = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave('Plots/ratio_byCity_gg2.pdf', ratio_byCity_gg2)

## Pos/neg ratio by newspaper --------------------------------------------------
ratio_byPaper_gg <- docs_LDA_art_sent |>
  filter(max_theta == 'Transportation' | max_theta == 'Industry') |> 
  select(!tag) |> 
  drop_na() |> 
  ggplot(aes(ratio, newspaper, fill = newspaper)) +
  geom_violin(alpha = .3) +
  geom_boxplot(width = .2) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  scale_fill_manual(values = pal) +
  facet_wrap(~max_theta, ncol = 1, strip.position = 'right') +
  labs(title = 'Ratio between positive and negative word count',
       subtitle = 'Visualised by topic and newspaper') +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        axis.line.x = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave('Plots/ratio_byPaper_gg.pdf', ratio_byPaper_gg)

## Most relevant stems in pos and neg ----
most_rel_stems_gg <- docs_LDA_target_sent |>
  group_by(polarity) |> 
  count(word, sort = T) |> 
  slice(1:20) |> 
  mutate(polarity = factor(polarity, levels = c('pos', 'neg'))) |> 
  ggplot(aes(reorder(word, n, decreasing = T), n, fill = polarity)) +
  geom_col() +
  # scale_y_continuous(limits = c(0, 850)) +
  facet_wrap(~polarity, scales = 'free') +
  labs(title = 'Most relevant stems in pos and neg') +
  ylab('Frequency') +
  scale_fill_manual(values = pal[2:6]) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .1),
        legend.position = 'null')

ggsave('Plots/most_rel_stems_gg.pdf', most_rel_stems_gg)
