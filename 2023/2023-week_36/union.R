library(tidyverse)
library(camcorder)
library(tidytuesdayR)
library(paletteer)
library(showtext)
library(ggtext)

gg_record(dir = "tidytuesday-temp", 
          device = "png", width = 10, 
          height = 8, 
          units = "in", dpi = 320)


# for some reason this is failing to parse
# df <- tt_load(2023, week=36)

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')


source("../../assets/utils.R")

font_add_google("Indie Flower", 'flower')
font_add_google("Righteous", "right")
font_add("fa-brands", regular = "../../assets/fonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../assets/fonts/fa-solid-900.ttf")


showtext_auto()
showtext_opts(dpi = 320)

state_difs <- states %>% 
  filter(sector %in% c("Private", "Public")) %>% 
  summarise(.by = c(state, state_abbreviation, sector),
            p_covered = mean(p_covered))


to_highlight <- 
  bind_rows(state_difs %>% slice_max(p_covered, by = sector),
            state_difs %>% slice_min(p_covered, by = sector))
to_highlight <- mutate(to_highlight,
       label = c("Hawaii has the highest %\nof unionized workers\nin the private sector",
                 "New York has the highest %\nof unionized workers\nin the public sector",
                 "The Carolinas rank lowest\nin both sectors",
                 "The Carolinas rank lowest\nin both sectors"),
       x = c(2000, 2000, 2003, 2000),
       y = c(0.4, 0.9, -0.05, -0.05),
       y1 = c(0.4, 0.85, -0.05, 0),
       x1 = c(1990, 2010, 1991, 2016),
       x2 = c(1985, 2015, 1985, 2020),
       yend = c(0.3, 0.75, 0.04, 0.05),
       curv = c(0.8, -0.2, -0.5, 0.5)
)


title <- "Decadent State of The Unions"
subtitle <- "Workers Covered By Collective Collective Bargaining Agreement.\nEach line represents one US state."
caption <- make_caption("black", "white", "unionstats.com")



ggplot(states %>% filter(sector %in% c("Private", "Public"))) +
  geom_line(aes(year,
                p_covered, 
                group=state_abbreviation),
            color = "gray70") +
  geom_line(data = states %>% 
              filter(sector %in% c("Private", "Public"),
                     state %in% to_highlight$state),
            aes(x=year, y=p_covered, color = state), linewidth = 2,
            show.legend = FALSE) +
  geom_text(data = to_highlight,
            aes(x = x, y = y, label = label), family = 'flower',
            size = 6) +
  lapply(split(to_highlight, 1:nrow(to_highlight)),
         function(dat) {
           geom_curve(data = dat,
                      mapping = aes(x = x1, xend = x2, y=y1, yend=yend),
                      curvature = dat['curv'],
                      lineend = 'round', 
                      arrow = arrow(length = unit(0.03, "npc"), type = "closed"))}
  ) +
  facet_wrap(~sector)+
  scale_y_continuous(labels = scales::label_percent(),
                     limits = c(-0.1, 1.1)) +
  labs(y = element_blank(),
       x = element_blank(),
       title = title,
       subtitle = subtitle,
       caption = caption)+
  ggthemes::theme_hc(base_family = "right") +
  theme(text = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_markdown()) +
  scale_color_paletteer_d("calecopal::fire")



states_map <- map_data('state')
states_map <- usmap::us_map() %>% 
  mutate(region = tolower(full))


filter()

data_anim <- states %>% filter(sector %in% c("Public")) %>%
  select(state, year, p_covered) %>% 
  mutate(region = tolower(state)) 

states_map_augmented <- states_map %>%
  crossing(year = unique(data_anim$year)) %>%
  left_join(data_anim, by = c("region", "year")) %>% 
  arrange(year, order)


gg_stop_recording()

# Create animated plot
p1 <- ggplot(data = states_map_augmented, 
       aes(x = x, y = y, fill = p_covered * 100, group = group)) + 
  geom_polygon(color = "white") + 
  scale_fill_viridis_c()+
  theme_void(base_family = "right") +
  theme(
    text = element_text(color = "gray80"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "gray5"),
    panel.background = element_rect(fill = "gray5"),
    plot.caption = element_markdown(colour = "gray80"),
    ) + 
  coord_fixed(1.3) + 
  gganimate::transition_states(year) + 
  labs(
    title = "Workers Covered by Agreement (%)",
    subtitle = "Public sector. Year: {closest_state}",
    fill = "Workers Covered (%)",
    caption = make_caption(accent = "gray80", bg="gray5", "unionstats.com"))


gganimate::animate(p1, 
                   height = 10,
                   width = 10, 
                   units = "in", res = 320)

gganimate::anim_save("gapminder_example.gif")

state_difs  %>% 
  pivot_wider(values_from = p_covered, names_from = sector) %>% 
  ggplot(aes(Public, Private)) +
  geom_abline(intercept = 0, slope = 1, lty = 3) +
  geom_point()

state_difs  %>% 
  pivot_wider(values_from = p_covered, names_from = sector) %>%
  ggplot(aes(Public, Public - Private)) +
  geom_abline(intercept = 0, slope = 1, lty = 3) +
  geom_point()
