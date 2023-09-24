# Load packages -----------------------------------------------------------
library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(paletteer)

#💾  Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 37)


# Load fonts --------------------------------------------------------------

source("../../assets/utils.R")
font_add("fa-brands", regular = "../../assets/fonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../assets/fonts/fa-solid-900.ttf")
font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 320)

# Define colours ----------------------------------------------------------

bg_col <- "black"
text_col <- "gray90"
highlight_col <- "gray90"


# Data wrangling ----------------------------------------------------------

filter(tuesdata$all_countries, 
       Category %in% c("Nonfood provision","Organization", "Technosphere modification", "Experience oriented")) %>%
  mutate(cat = ifelse(Category == "Experience oriented", "Experiences", "Economic")) %>% 
  group_by(cat, region_code, country_iso3) %>% 
  summarise(hours_day = sum(hoursPerDayCombined)) %>% 
  pivot_wider(names_from = cat, values_from = hours_day) %>% 
  ggplot(aes(Experiences, Economic)) +
  geom_point(alpha = 0.5, mapping = aes(color = region_code), size = 5)


filter(tuesdata$all_countries, 
       Subcategory %in% c("Meals")) %>%
  #group_by(cat, region_code, country_iso3) %>% 
  #summarise(hours_day = sum(hoursPerDayCombined)) %>% 
  #pivot_wider(names_from = cat, values_from = hours_day) %>% 
  separate(region_code, into = c("continent", "portion"), remove = F, sep = "_") %>% 
  ggplot(aes(hoursPerDayCombined)) +
  geom_density(aes(fill = continent)) +
  #facet_wrap(~continent, scales = "free_y")+
  NULL


filter(tuesdata$all_countries, 
       Subcategory %in% c("Meals")) %>%
  #group_by(cat, region_code, country_iso3) %>% 
  #summarise(hours_day = sum(hoursPerDayCombined)) %>% 
  #pivot_wider(names_from = cat, values_from = hours_day) %>% 
  separate(region_code, into = c("continent", "portion"), remove = F, sep = "_") %>% 
  ggplot() +
  geom_point(aes(hoursPerDayCombined, continent), 
             pch="|", alpha = 0.5, position = position_jitter(0.01, height = 0)) +
  #facet_wrap(~continent, scales = "free_y")+
  NULL

to_plot  <- filter(tuesdata$all_countries, 
                   Subcategory %in% c("Food preparation", "Meals")) %>%
  group_by(region_code, country_iso3) %>% 
  summarise(hours_day = sum(hoursPerDayCombined)) %>% 
  #pivot_wider(names_from = cat, values_from = hours_day) %>% 
  separate(region_code, into = c("regions", "portion"), remove = F, sep = "_") %>% 
  mutate(long_regions = 
           case_when(regions == "EU" ~ "European Union",
                     regions == "AS" ~ "Asia",
                     regions == "ANZ" ~ "Australia\nNew Zealand",
                     regions == "AM" ~ "America",
                     regions == "AF" ~ "Africa")) %>% 
  ungroup() %>% 
  rename(hours = hours_day)#hoursPerDayCombined)

minmax_per_region <- to_plot %>%
  summarise(.by = long_regions, 
            minh = min(hours),
            maxh = max(hours),
            country_max = country_iso3[which.max(hours)],
            country_min = country_iso3[which.min(hours)]) %>% 
  mutate(name_max = countrycode::countrycode(country_max, 
                                             origin = "iso3c",
                                             destination = "cldr.short.en"),
         name_min = countrycode::countrycode(country_min, 
                                             origin = "iso3c",
                                             destination = "cldr.short.en"))


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

title <- "Europeans Dedicate the Longest Time to Food"
st <- "Daily time spent in meal prep and eating meals (hours)."
# Make Caption
caption <- make_caption(accent = highlight_col, bg = "black", "doi.org/10.1073/pnas.2219564120")


# Plot --------------------------------------------------------------------

  ggplot(data = to_plot,
         aes(hours, long_regions)) +
  #geom_point(pch="|", size = 5, 
  #           alpha = 0.5, 
  #           position = position_jitter(0.01, height = 0)) +
  #facet_wrap(~continent, scales = "free_y")+
  #ggridges::geom_density_ridges2(alpha=0.8) +
  ggridges::geom_density_ridges(
    jittered_points = TRUE,
    position = ggridges::position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
  )
  
  NULL


  
iqr = function(z, lower = 0.25, upper = 0.75) {
    data.frame(
      y = median(z),
      ymin = quantile(z, lower),
      ymax = quantile(z, upper)
    )
  }  
  
ggplot(data = to_plot,
       aes(hours, long_regions)) +
    geom_point(pch="|", size = 5, 
               alpha = 0.5, 
               position = position_jitter(0.01, height = 0))+
    stat_summary(color = "darkred", fun.data = iqr,
                 position = position_nudge(y=0.4))+
    scale_x_continuous(limits = c(1.5, 4)) + 
  geom_text(data = minmax_per_region,
            aes(x = minh,
                y = long_regions,
                label = paste(name_min, minh, sep = "\n")), 
            nudge_x = -0.2,
            nudge_y = 0.2, size = 2) +
  geom_curve(data = minmax_per_region,
             aes(x = minh - 0.2, xend = minh - 0.05,
                 y = long_regions, yend = long_regions), 
             arrow = arrow(length = (length = unit(0.01, "npc"))),
             linewidth = 0.2
             ) + 
  geom_text(data = minmax_per_region,
            aes(x = maxh,
                y = long_regions,
                label = paste(name_max, maxh, sep = "\n")), 
            nudge_x = 0.2,
            nudge_y = 0.2, size = 2) +
  geom_curve(data = minmax_per_region,
             aes(x = maxh + 0.2, xend = maxh + 0.05,
                 y = long_regions, yend = long_regions), 
             curvature = -0.5, 
             arrow = arrow(length = (length = unit(0.01, "npc"))),
             linewidth = 0.2
  ) +
    ggdark::dark_theme_light(base_family = "roboto", base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "darkred", face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.caption = element_markdown(hjust = 0.5, size = 6)) +
    labs(y = element_blank(),
         x = element_blank(),
         title = title,
         subtitle = st,
         caption = caption)
    

ggplot(to_plot, aes(x = hours, )) +
  geom_histogram(binwidth = 0.1, aes(group = 1)) +
  geom_histogram(aes(fill = long_regions))+
  facet_wrap(~ long_regions)



ggplot(to_plot, aes(hours, fill = long_regions)) +
  geom_histogram(binwidth = 0.1, color="black") +
  scale_fill_viridis_d() +
  gghighlight::gghighlight() +
  ggdark::dark_theme_minimal()+
  facet_wrap(~ long_regions) +
  theme(strip.text = element_text(lineheight = 2)) +
  labs(y = element_blank(),
       x = st)
  
ggsave("histograms.png",
       width = 7,
       height = 5,
       units = "in",
       dpi = 320
)  

# Save gif ----------------------------------------------------------------

gg_playback(
  name = "meal_prep_rug.gif",
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
