library(tidyverse)
library(lubridate)
library(here)
library(tidybayes)
  
heartrate <- read_csv(here::here("201909", "heartrate-data.csv")) %>% 
  select(start_date = startDate, value) %>% 
  mutate(
    start_date = as_datetime(start_date),
    start_wday = wday(start_date, label = TRUE, abbr = FALSE, week_start = 1),
    start_time = as.numeric(format(start_date, "%H"))
  ) 


heartrate %>% 
ggplot(aes(x = value, y = start_time)) +
  annotate("rect", xmin = 40, ymin = -0.5, xmax = 210, ymax = 5.5, fill = "grey70", alpha = 0.2) +
  annotate("rect", xmin = 40, ymin = 17.5, xmax = 210, ymax = 23.5, fill = "grey70", alpha = 0.2) +
  stat_intervalh(.width = c(.1, .25, .5, .75, .9, 1), size = 7) +
  # geom_segment(aes(x = value, y = start_time - 0.35,
  #                  xend = value, yend = start_time + 0.35),
  #              color = "purple", size = 0.5, alpha = 0.1) +
  geom_point(aes(value, y = start_time), color = "purple", size = 0.1, alpha = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(40, 210), breaks = seq(40, 200, 20),
                     labels = c(
                       seq(40, 60, 20),
                       "♥ 80 bpm",
                       seq(100, 200, 20)
                       ),
                     sec.axis = dup_axis()) +
  scale_y_reverse(expand = c(0, 0), breaks = 0:23,
                  labels = c(
                    paste0(sprintf("%02d", 0:5), ":", "00"),
                  "☀ 06:00",
                  paste0(sprintf("%02d", 7:17), ":", "00"),
                  "☾ 18:00",
                  paste0(sprintf("%02d", 19:23), ":", "00")
                  )
                  ) +
  scale_color_brewer(palette = "OrRd") +
  labs(
    title = "50 000 personal heart rate values from 2016 to 2019",
    subtitle = "The purple dots correspond to the actual values, while the bars show the distribution\nat 10%, 25%, 50%, 75%, 90% and 100% of total values by hour of day",
    caption = "Data and Graphic by Georgios Karamanis"
  ) +
  theme_minimal(base_family = "IBM Plex Sans", base_size = 14) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(family = "Menlo", size = 11, color = "grey60"),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  ) +
  ggsave(here::here("201909", "plot-heartrate.png"), width = 12, height = 9, dpi = 320)
