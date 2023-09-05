library(tidyverse)
library(camcorder)
library(tidytuesdayR)
library(paletteer)
library(ggalluvial)
library(ggsankey)
library(scales)
library(gganimate)
gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 12, units = "in", dpi = 320)


df <- tt_load(2023, week=34)
df$population$coa_name <- str_replace(df$population$coa_name,
                             "United Kingdom of Great Britain and Northern Ireland",
                             "UK")

df$population$coa_name <- str_replace(df$population$coa_name,
                                      "Iran \\(Islamic Rep. of\\)",
                                      "Iran")

df$population$coa_name <- str_replace(df$population$coa_name,
                                      'Netherlands \\(Kingdom of the\\)',
                                      'Netherlands')

get_most_frequent <- function(data, n){
  data <- mutate(data, 
         coa_lump = fct_lump(coa_name, n=n, w = refugees),
         coa_iso_lump = fct_lump(coa_iso, n=n, w = refugees)) %>% 
  reframe(.by = c(coa_lump, coa_iso_lump),
          refugees = sum(refugees),
          coo = unique(coo)) %>% 
  select(coo, coa_lump, refugees) 
  return(data)
}

mf <- df$population %>% 
  filter(coo %in% c("AFG", "SYR", "PAK")) %>% 
  # lump into "other"
  nest(data = -year) %>% 
  mutate(mf = map(data, function(.x) get_most_frequent(.x, 3))) 
  
mf <- mf %>%
    mutate(set_data = map(mf,
                          function(.x) ggforce::gather_set_data(.x, x = 1:2)),
           set_data = map(set_data,
                          function(.x) mutate(.x, 
                                              coa_lump = fct_reorder(coa_lump, desc(refugees)),
                                              y = fct_reorder(y, desc(refugees))
                                              )
                          ))


custom_colors <- paletteer::paletteer_d("awtools::a_palette")[1:6]
names(custom_colors) <- c('Other', 'Germany', 'Iran', 'Pakistan', 'Lebanon', 'Türkiye')

  
plot_list <- map(
  c(2010, 2015, 2020),
  function(.x) 
    ggplot(mf %>% filter(year %in% .x) %>%
             unnest(set_data), 
           aes(x, id = id, split = y, value = refugees)) +
    ggforce::geom_parallel_sets(aes(fill = coa_lump),
                                alpha = 0.6, axis.width = 0.4) +
    ggforce::geom_parallel_sets_labels(
      aes(label = after_stat(paste(label, 
                                   number(value,
                                          accuracy = 0.1,
                                          scale_cut = cut_short_scale()),
                                   sep = ' '))), 
      colour = 'black', 
      nudge_x = 0, angle = 0) +
    scale_x_continuous(expand = expansion(0, 0.1)) +
    theme_void(base_family = "Ubuntu", base_size = 16) +
    theme(legend.position = "none",
          text = element_text(family = "Ubuntu")) +
    facet_wrap(~year) +
    scale_fill_manual(values = custom_colors)
)

library(patchwork)
plot_title <- ggplot() +
  labs(title = "Pakistan No Longer Absorbs The Most Refugees",
       subtitle = "Refugee flow from 3 selected Countries of Origin (Afghanistan, Pakistan, and Syria) into Countries of Asylum.\nData from United Nations High Commissioner for Refugees (UNHCR) Refugee Data Finder") +
  theme_void(base_size = 16, base_family = "Ubuntu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

plots <- wrap_plots(plot_list) + 
  plot_layout(nrow = 3)
plot_title / plots + plot_layout(heights = c(0.05 , 1))
