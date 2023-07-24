# major credits to
# https://github.com/gkaramanis/tidytuesday/blob/master/2023/2023-week_28/global_temps.R

library(tidyverse)
library(camcorder)
library(tidytuesdayR)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

df <- tt_load(2023, week=28)

global_temps <- df$global_temps %>% 
  pivot_longer(Jan:Dec, names_to = "month") %>% 
  mutate(month = fct_inorder(month))


mean_temps <- global_temps %>% 
  group_by(Year) %>% 
  summarise(min = min(value, na.rm = T), 
            max = max(value, na.rm = T),
            mean = mean(value, na.rm = T)) 

annot <- filter(mean_temps, Year %in% c(1966, 1991, 2023)) %>% 
  select(Year, mean) %>% 
  mutate(Year = ifelse(Year == 2023, 2024, Year),
         label = c("Dad's Birth",
                   "My Birth",
                   "Son's Birth"),
         id = c("Dad", "Self", "Son"))

mean_temps %>%
  ggplot(aes(Year)) + 
  geom_ribbon(aes(ymin=min, ymax=max), 
              color = "gray75", 
              fill="gray90", 
              alpha = 0.8) + 
  geom_line(aes(y = mean), 
            linewidth = 1, 
            color="red")  + 
  geom_point(data = annot,
             aes(Year, mean),
             color="red", size=5) +
  geom_curve(data = annot,
             aes(x = Year - 20, 
                 xend = Year,
                 y = mean + 0.7,
                 yend = mean),
             color="gray25", 
             arrow = arrow(length = unit(0.03, "npc"), 
                           type = "closed"),
             curvature = -0.3) +
  geom_text(data = annot,
            aes(x = Year - 22,
                y = mean + 0.7,
                label = label), 
            hjust = 1,
            color="gray25", 
            size = 9) +
  ggthemes::theme_fivethirtyeight(base_size = 16) +
  labs(y = "Mean Global Temperature (C)",
       x = element_blank(),
       title = "Generational Warming",
       subtitle = "Yearly Global Surface Temperature. Shown as deviation from the 1951-1980 means",
       caption = "Source GISTEMP v4\nViz: @NeuroMLA")


mean_temps %>%
  ggplot(aes(Year)) + 
  geom_ribbon(aes(ymin=min, ymax=max), 
              color = "gray75", 
              fill="gray90", 
              alpha = 0.8) + 
  geom_line(aes(y = mean), 
            linewidth = 1, 
            color="gray25")  + 
  geom_point(data = annot,
             aes(Year, mean, 
                 color = id), size=5) +
  paletteer::scale_color_paletteer_d("basetheme::clean") +
  ggthemes::theme_fivethirtyeight(base_size = 16) +
  labs(y = "Mean Global Temperature (C)",
       x = element_blank(),
       title = "Generational Warming",
       subtitle = "Yearly Global Surface Temperature. Shown as deviation from the 1951-1980 means.\nPoints show the year of birth of 3 generations in a family.",
       caption = "Source GISTEMP v4\nViz: @NeuroMLA",
       color = element_blank()) +
  theme(legend.position = c(0.85, 0.1))
