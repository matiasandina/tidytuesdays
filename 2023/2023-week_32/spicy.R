library(tidyverse)
library(camcorder)
library(tidytuesdayR)

gg_record(dir = "tidytuesday-temp", 
          device = "png", width = 10, height = 8, units = "in", dpi = 320)


df <- tt_load(2023, week=32)

df$episodes %>% 
  group_by(guest) %>% 
  summarise(times = max(guest_appearance_number)) %>% 
  arrange(desc(times)) %>% 
  head(n=20)

unique_guests <- df$episodes %>% 
  distinct(guest) %>% 
  select(guest) %>% 
  tidyr::separate(guest, into=c("first", "last"), sep = " ")

predicted_genders <- gender::gender(unique_guests$first)

predicted_genders %>% 
  count(gender)

# it missed a few women, but mostly all men here too
filter(unique_guests, first %in% setdiff(unique_guests$first, predicted_genders$name))

df$sauces %>% 
  ggplot(aes(sauce_number, log10(scoville))) +
  geom_line(aes(group=season, color=factor(season)))+
  geom_point() +
  scale_color_viridis_d()

df$sauces %>% 
  ggplot(aes(factor(sauce_number), season,fill=log10(scoville))) +
  geom_tile() +
  scale_y_continuous(expand = expansion(0, 0)) +
  scale_x_discrete(expand = expansion(0, 0)) +
  paletteer::scale_fill_paletteer_c("pals::kovesi.linear_kryw_5_100_c67", 
                                    direction = -1) +
  coord_fixed(ratio = 1/5) +
  theme(
    text = element_text(size = 16, family = "Ubuntu"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_blank(),
    axis.ticks = element_blank())+
  labs(y = "Season", 
       x = "Sauce Number",
       fill = "Scoville Rating (log)",
       title = "🌶 Bring on the Heat",
       subtitle = "Scoville ratings for the sauces on each season of `Hot Ones`")


df$sauces %>%
  summarise(.by = season, max_sco = max(scoville)) %>% 
  ggplot(aes(season, max_sco)) +
  geom_point()

df$episodes %>% 
  group_by(season) %>% 
  count(finished) %>% 
  mutate(total = sum(n),
         prop = n / total) %>% 
  filter(finished) %>% 
  ggplot(aes(season, prop)) +
  geom_point()
  
