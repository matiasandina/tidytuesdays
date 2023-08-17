library(tidyverse)
library(camcorder)
library(tidytuesdayR)
library(paletteer)
library(FactoMineR)
library(factoextra)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)


df <- tt_load(2023, week=33)

# This is the recommended method to classify
# spam.rpart <- rpart::rpart(formula = yesno ~ crl.tot + dollar + bang +
#                             money + n000 + make, data=df$spam)
# plot(spam.rpart)
# text(spam.rpart)

df_transformed <- select(df$spam, where(is.numeric)) %>% 
  mutate_all(.funs = function(x) predict(bestNormalize::yeojohnson(x), newdata = x))

pc <- prcomp(df_transformed)
pc_data <- pc$x[, 1:2] %>% as_tibble()
pc_data$yesno <- df$spam$yesno
set.seed(1234)
ggplot(mapping = aes(PC1, PC2)) + 
  geom_point(data = filter(pc_data, yesno=="n"),
                    alpha = 0.4, size = 8,
             pch = "📧") +
  geom_point(data = filter(pc_data, yesno=="y"),
             alpha = 0.4, size = 4,
             pch = "💀") +
  coord_equal() +
  theme_void() +
  theme(
    text = element_text(size = 16), 
    title = element_text(family = "Ubuntu"),
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#05CFE1", color = NA),
    plot.background = element_rect(fill = "#05CFE1")) +
  annotate("segment", x = -6, xend = 2, 
           y = 3, yend = 3) + 
  labs(title = "Finding Spam In The Email Space",
       subtitle = "2D representation of spam (💀) or emails (📧).\nSpace constructed using a PCA of the transformed spam dataset.")