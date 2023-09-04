library(tidyverse)
library(camcorder)
library(tidytuesdayR)
library(paletteer)
library(ggalluvial)
library(ggsankey)
library(scales)
gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)


df <- tt_load(2023, week=34)
df$population$coa_name <- str_replace(df$population$coa_name,
                             "United Kingdom of Great Britain and Northern Ireland",
                             "UK")

df$population$coa_name <- str_replace(df$population$coa_name,
                                      "Iran \\(Islamic Rep. of\\)",
                                      "Iran")


df$population %>% 
  filter(coo %in% "SYR") %>% # c("AFG", "SYR", "PAK")) %>% 
  # lump into "other"
  mutate(
    .by = year,
    coa_lump = fct_lump(coa_name, n=5, w = refugees),
    coa_iso_lump = fct_lump(coa_iso, n=5, w = refugees)) %>% 
  reframe(.by = c(year, coa_lump, coa_iso_lump),
            refugees = sum(refugees),
            coo = unique(coo)) %>% 
  select(year, coo, coa_lump, refugees) %>% 
  mutate(year = as.character(year)) %>% 
  ggforce::gather_set_data(x = 1:3) %>% 
  mutate(coa_lump = fct_reorder(coa_lump, desc(refugees)),
         y = fct_reorder(y, desc(refugees))) %>% 
  ggplot(aes(x, id = id, split = y, value = refugees)) +
  ggforce::geom_parallel_sets(aes(fill = coa_lump),
                              alpha = 0.3, axis.width = 0.4) +
  #ggforce::geom_parallel_sets_axes() +
  ggforce::geom_parallel_sets_labels(
    aes(label = after_stat(label)), 
    colour = 'black', 
    nudge_x = 0, angle = 0) +
  scale_x_continuous(expand = expansion(0, 0.1)) +
  theme_void() +
  theme(legend.position = "none") 
