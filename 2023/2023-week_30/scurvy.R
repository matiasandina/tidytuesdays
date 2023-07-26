library(tidyverse)
library(camcorder)
library(tidytuesdayR)
library(paletteer) 


# A LOT of inspiration from
# https://github.com/efranke22/tidytuesday/blob/main/2023/week30/tidyScurvy.Rmd

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)


df <- tt_load(2023, week=30)

df$scurvy %>%
  pivot_longer(cols = c(gum_rot_d6:lassitude_d6), 
               names_to = "Symptom", 
               values_to = "Severity") %>%
  mutate(Severity = str_to_title(str_extract(Severity, "[a-z]+")),
         Severity = factor(Severity,
                           levels = c("None", "Mild", "Moderate", "Severe")),
         treatment = str_to_title(str_replace_all(treatment, "_", " ")),
         Symptom = case_when(Symptom == "gum_rot_d6" ~ "Gum rot", 
                             Symptom == "skin_sores_d6" ~ "Skin sores", 
                             Symptom == "weakness_of_the_knees_d6" ~ "Weakness of knees", 
                             Symptom == "lassitude_d6" ~ "Lassitude"), 
         treat_label = case_when(study_id %% 2 == 0 ~ treatment, 
                                 TRUE ~ ""),
         study_id = factor(study_id, 
                           levels = c("9", "10", "1", "2", "3", "4", "5", "6", "7", "8", "11", "12"))) %>%
  ggplot(aes(y=study_id, x=Symptom, color=Severity))+
  geom_point(shape = 15, size = 8)+
  geom_text(aes(x = 4.8, 
                y = study_id, 
                label = treat_label),
            nudge_y = -0.5, size =3.5, 
            family = "Ubuntu", 
            fontface = "bold",
            color = "gray40")+
  geom_text(aes(x = Symptom, 
                y = 13, 
                label = Symptom),
            size =3.5, 
            family = "Ubuntu", 
            fontface="bold", 
            color = "gray40") +
  expand_limits(x=5.5, y=17) +
  scale_color_paletteer_d("futurevisions::titan", direction = -1) +
  labs(x = element_blank(), 
       title = "Citrus Treatment Improves Scurvy Symptoms", 
       subtitle = "Each square is one patient measured for 4 common scurvy symptoms in 1757 by James Lind.", 
       caption = "Data: medicaldata R package | Viz: @NeuroMLA")+
  geom_hline(yintercept = c(10.5, 8.5, 6.5, 4.5, 2.5), 
             color = "gray90", linewidth = 0.5, linetype = "dashed")+
  theme(text = element_text(size = 16),
        aspect.ratio = 1/1.18, 
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size =9),
        panel.background = element_rect(fill = "gray6"),
        panel.grid = element_blank(),
        legend.position = c(0.5, 0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill=NA, color="gray90"),,
        legend.text = element_text(color = "gray90"),
        legend.title = element_text(color = "gray90", face = "bold"))
