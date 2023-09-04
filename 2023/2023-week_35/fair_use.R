library(tidyverse)
library(camcorder)
library(tidytuesdayR)
library(paletteer)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)


df <- tt_load(2023, week=35)
