# Load packages -----------------------------------------------------------
library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(paletteer)

#💾  Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(date_chr)


# Load fonts --------------------------------------------------------------

source("../../assets/utils.R")
font_add("fa-brands", regular = "../../assets/fonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../assets/fonts/fa-solid-900.ttf")
font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 320)

# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------



# Start recording ---------------------------------------------------------

gg_record(
  dir = "tidytuesday-temp",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 320
)


# Define plot text -------------------------------------------------------------

title <- ""
st <- ""
# Make Caption
caption <- make_caption("black", "white", "")


# Plot --------------------------------------------------------------------



# Save gif ----------------------------------------------------------------

gg_playback(
  name = ,
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
