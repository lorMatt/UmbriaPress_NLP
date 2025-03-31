if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, tidygraph, ggraph, ggh4x, patchwork)

# Import -----------------------------------------------------------------------
UPTok_sent <- readRDS('Models/Retrieval_UPTok_sent.RDS')
UPngrams_sent <- read_rds('Models/Retrieval_UPngrams_sent.RDS')

# Settings ---------------------------------------------------------------
## palette ----
pal <- c(
  "#FDA638",
  "#459395",
  "#EB7C69",
  '#2BE19E',
  '#972F5A',
  '#121333'
)
na_col <- "gray75"

## theming ----
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

# Document-level ---------------------------------------------------------------
## Pos-neg per topic by city ----
ret_ratioCity_gg <- UPTok_sent |> 
  filter(Industry + Transportation + Environment < 2) |> # eliminating ambiguous matches
  pivot_longer(cols = Industry:Environment,
               names_to = 'topic',
               values_to = 'flag') |> 
  filter(flag != 0) |> 
  ggplot(aes(ratio, city, fill = city)) +
  geom_violin(alpha = .3) +
  geom_boxplot(width = .2) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  scale_fill_manual(values = pal) +
  facet_wrap(~topic, ncol = 1, strip.position = 'right') +
  labs(title = 'Ratio between positive and negative word count',
       subtitle = 'Visualised by topic and city') +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        axis.line.x = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave('Plots/ret_ratioCity_gg.pdf', ret_ratioCity_gg)
### restricted to corriere
ret_ratioCorrCity_gg <- UPTok_sent |> 
  filter(Industry + Transportation + Environment < 2 & newspaper == 'Corriere dell\'Umbria') |> # eliminating ambiguous matches
  pivot_longer(cols = Industry:Environment,
               names_to = 'topic',
               values_to = 'flag') |> 
  filter(flag != 0) |> 
  ggplot(aes(ratio, city, fill = city)) +
  geom_violin(alpha = .3) +
  geom_boxplot(width = .2) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  scale_fill_manual(values = pal) +
  facet_wrap(~topic, ncol = 1, strip.position = 'right') +
  labs(title = 'Ratio between positive and negative word count',
       subtitle = 'Visualised by topic and city, restricted to Corriere dell\'Umbria') +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        axis.line.x = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave('Plots/ret_ratioCorrCity_gg.pdf', ret_ratioCorrCity_gg)
## Pos/neg ratio by newspaper --------------------------------------------------
ret_ratio_byPaper_gg <- UPTok_sent |> 
  filter(Industry + Transportation + Environment < 2) |> # eliminating ambiguous matches
  pivot_longer(cols = Industry:Environment,
               names_to = 'topic',
               values_to = 'flag') |> 
  filter(flag != 0) |> 
  ggplot(aes(ratio, newspaper, fill = newspaper)) +
  geom_violin(alpha = .3) +
  geom_boxplot(width = .2) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  scale_fill_manual(values = pal[3:6]) +
  facet_wrap(~topic, ncol = 1, strip.position = 'right') +
  labs(title = 'Ratio between positive and negative word count',
       subtitle = 'Visualised by topic and newspaper') +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        axis.line.x = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave('Plots/ret_ratio_byPaper_gg.pdf', ret_ratio_byPaper_gg)

## Most relevant stems in pos and neg ----
ret_topTok_gg <- UPTok_sent |>
  group_by(polarity, city) |> 
  count(token, sort = T) |> 
  slice(1:20) |> 
  mutate(polarity = factor(polarity, levels = c('pos', 'neg'))) |> 
  ggplot(aes(n, reorder_within(token, n, city, decreasing = F), fill = polarity)) +
  geom_col() +
  facet_grid2(polarity~city, scales = 'free', independent = 'y') +
  labs(title = 'Most relevant stems in pos and neg') +
  xlab('Frequency') +
  scale_fill_manual(values = pal[2:6]) +
  scale_y_reordered() +
  # theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1, vjust = .1),
        legend.position = 'null')

ggsave('Plots/ret_topTok_gg.pdf', ret_topTok_gg)

# N-grams ----------------------------------------------------------------------
## basic graph
bigram_graph_TR <- UPngrams |> 
  filter(city == 'TR') |> 
  filter(str_detect(token1, regex("emissioni|PM10|inquinamento|ecolog|riuso|ecosistem|rifiuti|inceneritor", ignore_case = T)),
         !token1 == 'ginecologia') |> 
  count(token1, token2) |> 
  filter(n > 10) |> 
  as_tbl_graph() |> 
  ggraph(layout = 'fr') +
  geom_edge_fan(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(size = 3, colour = pal[2]) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0, size = 3) +
  labs(tag = 'Terni') +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank())

bigram_graph_PG <- UPngrams |> 
  filter(city == 'PG') |> 
  filter(str_detect(token1, regex("emissioni|PM10|inquinamento|ecolog|riuso|ecosistem|rifiuti|inceneritor", ignore_case = T)),
         !token1 == 'ginecologia') |> 
  count(token1, token2) |> 
  filter(n > 10) |> 
  as_tbl_graph() |> 
  ggraph(layout = 'fr') +
  geom_edge_fan(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(size = 3, colour = pal[1]) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0, size = 3) +
  labs(tag = 'Perugia') +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank())

bigram_graph <- bigram_graph_PG + bigram_graph_TR &
  plot_annotation(title = 'Co-occurrences graph',
                  subtitle = 'Bigrams filtered by manual query') &
  theme(plot.tag.position = 'bottom',
        plot.tag = element_text(size = 10, face = 'bold'))

ggsave('Plots/bigram_graph.pdf', bigram_graph, width = 10)



## sentiment
bigram_sent_gg <- UPngrams_sent |> 
  ggplot(aes(ratio, query, fill = city)) +
  geom_col(position = position_dodge()) +
  geom_vline(xintercept = 0, colour = 'gray25') +
  scale_fill_manual(values = pal) +
  scale_x_continuous(limits = c(-1,1)) +
  labs(title = 'Average sentiment by query',
       subtitle = 'Positive/negative wordcount ratio computed on bigrams, normalised') +
  guides(color = guide_legend(override.aes = list(shape = 1))) +
  theme(legend.position = 'bottom',
        axis.title = element_blank())

ggsave('Plots/bigram_sent_gg.pdf', bigram_sent_gg, width = 8, height = 3)
