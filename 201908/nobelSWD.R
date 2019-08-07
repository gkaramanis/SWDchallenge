library(tidyverse)
library(here)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

decadesLabels <- function(string) {
  return(as.numeric(string)+1900)
}

filterCategory = "Medicine"

nobel_winners %>%
  filter(category == filterCategory) %>%
  mutate(nume = as.numeric(str_sub(prize_share, 1, 1)),
         deno = as.numeric(str_sub(prize_share, -1)),
         share = nume/deno,
         year = prize_year %% 10,
         decade = prize_year - 1900 - year) %>%
  group_by(prize_year) %>% 
  distinct(full_name, .keep_all = TRUE) %>% 
  mutate(n = row_number()) %>%
  # Big parts of plot code from https://github.com/spren9er/tidytuesday/blob/master/tidytuesday_201916_new_economist.r
  ggplot() + 
  geom_bar(aes(x = "", y = share, fill = as.factor(n)),
    stat = "identity", show.legend = FALSE
  ) +
  scale_fill_brewer(palette = "Purples") +
  coord_polar("y") +
  facet_wrap(~ prize_year, ncol = 10,
             labeller = labeller(decade = decadesLabels)) +
  # labs(title = paste("Shared Nobel Prizes in ", filterCategory, sep = ""),
  #      subtitle = "by decade and year, 1901-2016",
  #      caption = "Source: Kaggle | Graphic: Georgios Karamanis") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey80")
    ) 


ggsave(here("201908", "nobelSWD.png"))
