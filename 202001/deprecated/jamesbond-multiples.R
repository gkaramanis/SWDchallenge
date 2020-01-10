library(tidyverse)
library(here)
library(janitor)
library(lemon)
library(gghighlight)

# Chrck notes!

jb <- readRDS(here::here("202001", "data", "jamesbond-multiples.RDS")) %>% 
  clean_names() %>% 
  rename(consensual_sex = conquests) %>% 
  pivot_longer(c("kills_others", "kills_bond"), names_to = "what", values_to = "value") %>%
    select(year, movie, what, value) %>%
  mutate(movie = fct_reorder(movie, year))
  
ggplot(jb, aes(what, -value)) +
  geom_col(width = 0.6, fill = "darkred") +
  facet_wrap(vars(movie), ncol = 6) +
  # theme_minimal() +
  ggsave(
    here::here("202001", "plots", "jamesbond-multiples.png"), dpi = 320, width = 6, height = 10
  )
