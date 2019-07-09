library(here)
library(tidyverse)

df <- read.csv(here::here("201907", "health.csv"))
df_t <- df %>%
  mutate(date = as.Date(date)) %>% 
  filter(date > "2019-06-02") %>% 
  select(date, steps_perc, sleep_perc, exercise_perc) %>% 
  gather("type", "perc", steps_perc:exercise_perc) %>% 
  mutate(wkd = weekdays(as.Date(date,'%Y-%m-%d')),
         wk = format(date, "%V"),
         d = format(date, "%d"))

df_t$wkdf <- factor(df_t$wkd, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(df_t) +
  geom_rect(aes(xmin = 0, xmax = perc,
                ymin = type, ymax = type,
                color = type),
            size = 2.6, key_glyph = "smooth") +
  coord_polar() +
  labs(
    title = "Activity and sleep data from June 3 to June 30",
    subtitle = "Goals: 10k steps, 1h exercise and 7h sleep per day",
    caption = "Graphic by Georgios Karamanis"
    ) +
  ylim(c("", "", "sleep_perc", "exercise_perc", "steps_perc")) +
  scale_color_manual(values = c("#FF006D", "#01BEFE", "#ADFF02"),
    labels = c("Exercise", "Sleep", "Steps")) +
  geom_text(aes(0, 0, label = d),
    family = "IBM Plex Mono",
    color = "grey50") +
  facet_grid(wk ~ wkdf) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F7E9D4"),
    plot.margin = margin(0, 20, 0, 20),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 10, 0),
    strip.text.x = element_text(family = "IBM Plex Mono Medium",
                              size = 11, color = "grey50",
                              margin = margin(20, 0, 10, 0)),
    strip.text.y = element_blank(),
    plot.title = element_text(family = "IBM Plex Serif Bold",
                              hjust = 0.5,
                              margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(family = "IBM Plex Sans",
                                 hjust = 0.5),
    plot.caption = element_text(family = "IBM Plex Sans",
                                hjust = 0.5, size = 6,
                                margin = margin(0, 0, 10, 0))
  ) +
  ggsave(here("201907", "health.png"), dpi = 300)




