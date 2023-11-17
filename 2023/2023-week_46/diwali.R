# Load packages -----------------------------------------------------------
library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(paletteer)

#💾  Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 46)


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

house <- tuesdata$diwali_sales_data

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

title <- "Diwali Customer Base Distribution"
st <- "Banking, Aviation, Healthcare, and IT take the lion's share of sales"
# Make Caption
caption <- make_caption(accent = "gray70", "black", "Diwali Dataset")


# Plot --------------------------------------------------------------------

house %>% 
  count(Age, Occupation) %>% 
  filter(n > 5) %>% 
  ggplot(aes(Age, Occupation, size = n)) +
  geom_point()
  

house %>% 
  count(Age, Occupation) %>% 
  filter(n > 5) %>% 
  ggplot(aes(Age, Occupation, size = n, fill=n)) +
  geom_point(pch = 22)

sum_data <- house %>% 
  count(Occupation) %>%
  rename(total = n) %>% 
  mutate(Occupation = fct_reorder(Occupation, total))

house %>% 
  count(Age, Occupation) %>% 
  filter(n > 5) %>% 
  left_join(sum_data) %>% 
  mutate(Occupation = fct_reorder(Occupation, desc(total))) %>% 
  ggplot(aes(Age, Occupation, fill=n)) +
  geom_tile(color = "white") +
  geom_text(data = sum_data,
            aes(x = 60, y = Occupation, label = total),
            inherit.aes = FALSE) +
  scale_color_viridis_c("inferno") +
  ggdark::dark_theme_minimal() +
  labs(title = title,
       subtitle = st,
       caption = caption,
       fill = "Number of Purchases") +
  theme(plot.caption = element_markdown(hjust = 0.5),
        legend.position = "bottom",
        legend.spacing.y = unit(-0.25, 'cm')) + 
  scale_x_continuous(breaks = seq(10, 55, 5))
# Save gif ----------------------------------------------------------------

gg_playback(
  name = "diwali_gif",
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "black"
)
