if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, ggrepel, ggh4x)

# Import dataset ---------------------------------------------------------------
UmbriaPressSal <- readRDS('Data/UmbriaPressSal.RDS')
  
# Visualisation
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
                plot.title = element_text(face = 'bold'),
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




## Topic salience by city ----

ret_salByCity <- UmbriaPressSal |> 
  mutate(ratio = case_match(ratio, NA ~ 0, .default = ratio)) |> 
  ggplot(aes(semester, ratio, col = topic)) +
  # geom_area(aes(fill = topic), alpha = .1, position = 'identity') +
  geom_line() +
  geom_point(size = 1.2) +
  geom_point(colour = 'white', size = .2) +
  facet_grid2(city~., independent = 'x', scales = 'free') +
  scale_colour_manual(values = pal) +
  scale_y_continuous(limits = c(0,.1), expand = c(0,0)) +
  scale_x_date(limits = c(dmy('01/01/2016'), dmy('01/01/2025')),
               expand = expansion(c(0,.01)),
               date_breaks = '2 years',
               date_labels = '%Y') +
  labs(title = 'Topic salience by city',
       subtitle = 'Percentage of matching articles by semester') +
  guides(color = guide_legend(override.aes = list(size = 5,
                                                  linetype = 0,
                                                  fill = NA))) +
  theme(axis.title = element_blank(),
        # axis.line.y = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text(size = 12, vjust = 1.1, face = 'bold'))

ggsave('Plots/ret_salByCity.pdf', ret_salByCity, width = 10)
