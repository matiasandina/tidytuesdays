library(tidyverse)
library(camcorder)
library(tidytuesdayR)
library(paletteer)
library(showtext)
showtext_auto()
#showtext_opts(dpi = 320)
gg_record(dir = "tidytuesday-temp", device = "png", 
          width = 14, height = 6, 
          units = "in", dpi = 320)

source("../../assets/utils.R")
font_add("fa-brands", regular = "../../assets/fonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../assets/fonts/fa-solid-900.ttf")

df <- tt_load(2023, week=35)

caption <- make_caption("black", "white", "US Copyright Office")


df$fair_use_cases %>% 
  filter(year > 1950) %>% 
  mutate(year_bin = cut(year, seq(1945, 2025, 5))) %>% 
  count(year_bin, fair_use_found) %>% 
  mutate(fair_use_found = ifelse(fair_use_found, "Fair", "Not Fair")) %>%
  separate(year_bin, into = c('low', "high"), sep = ",") %>% 
  mutate(low = str_remove(low, "\\(")) %>% 
  ggplot(aes(low, y = n, fill = fair_use_found)) + 
  geom_col() +
  labs(x = element_blank(), 
       y = "Number of Use Cases",
       title = "Is Fair Use Increasing?",
       subtitle = "Data comes from the U.S. Copyright Office Fair Use Index",
       fill = "Use",
       caption = caption) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size = 24, lineheight = 1, colour = "gray50"),
    legend.position = "bottom",
    plot.caption = ggtext::element_markdown(colour = "gray50", 
                                            hjust = 1,
                                            margin = margin(t=20, b=20), 
                                            size = 14))
  
