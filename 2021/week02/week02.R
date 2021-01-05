tuesdata <- tidytuesdayR::tt_load(2021, week = 2)
transit_cost <- tuesdata$transit_cost
library(tidyverse)


# clean -------------------------------------------------------------------

transit_cost <- 
  transit_cost %>% 
  # remove length outliers that are NA
  filter(length < 5000) %>% 
  # duration and cleaning of years
  # NAs by coercion
  mutate(duration = ifelse(str_detect(start_year, "years"),
                           parse_number(start_year),
                           as.numeric(end_year) - as.numeric(start_year)),
         start_year = as.numeric(start_year),
         end_year = as.numeric(end_year))

ggplot(transit_cost,
       aes(length, cost_km_millions)) +
  geom_point() +
  ggrepel::geom_text_repel(data = filter(transit_cost, cost_km_millions > 1000), 
            aes(length, cost_km_millions, label=city),
            direction="x", 
            segment.color = "black") +
  ggrepel::geom_text_repel(data = filter(transit_cost, length > 100), 
                           aes(length, cost_km_millions, label=city),
                           direction="y", 
                           segment.color = "black")

med_adj_cost <- 
transit_cost %>% 
  arrange(desc(cost_km_millions)) %>% 
  mutate(p_order = 1:nrow(transit_cost), 
         adj_cost = cost_km_millions - median(cost_km_millions, na.rm=T)) 
med_adj_cost %>% 
  ggplot(aes(p_order, adj_cost)) + 
  geom_point() +
  ggrepel::geom_text_repel(data = filter(med_adj_cost, p_order < 8), 
                           aes(p_order, adj_cost, label=city),
                           direction="x", 
                           segment.color = "black") +
  ggrepel::geom_text_repel(data = tail(med_adj_cost), 
                           aes(p_order, adj_cost, label=city),
                           segment.color = "black")+
  scale_x_discrete(expand=c(0, 10))

ggplot(transit_cost, aes(stations)) +
  geom_histogram()

ggplot(transit_cost, aes(stations/length)) +
  geom_histogram() +
  labs(
    title = "Subway stations per km",
    subtitle=glue::glue("Average of {round(mean(transit_cost$stations/transit_cost$length, na.rm=T), 2)} stations per km"))

ggplot(transit_cost, aes(stations/length, length)) +
  geom_point()

transit_cost %>% 
  mutate(cutt = cut(length, seq(0, 200, 40))) %>% 
  filter(!is.na(duration)) %>%
  group_by(duration, cutt) %>% 
  count() %>% 
  ggplot(aes(duration, y=n, fill=cutt)) +
  geom_col(color="black") +
  labs(title="It takes roughly 5 years to complete a subway",
       fill="Tunnel length (Km)",
       x="Years", y="Count") +
  ggthemes::theme_clean()+ 
  theme(legend.position="bottom")

transit_cost %>% 
  mutate(duration = end_year - start_year) %>% 
ggplot(transit_cost) +
  geom_segment(aes(x=start_year, xend=end_year, y = city, yend=city))