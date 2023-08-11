library(tidyverse)
library(camcorder)
library(tidytuesdayR)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)


df <- tt_load(2023, week=31)

mutate(df$states, 
       q = case_when(
         str_detect(demonym, "ian$") ~ "ian",
         str_detect(demonym, "an$") ~ "an",
         str_detect(demonym, "er$") ~ "er",
         TRUE ~ "other")
) %>% 
  count(q)


