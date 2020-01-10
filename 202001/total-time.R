library(tidyverse)
library(readxl)
library(here)
library(geofacet)

total_time <- read_xlsx(here::here("202001", "data", "total-time.xlsx"), skip = 1) %>% 
  head(., -5) %>% 
  select(region = "Region/landsting/akutmottagning", sex = Kön, age = Ålder, "2018") %>% 
  # mutate_at(c("2016", "2017"), as.numeric) %>% 
  pivot_longer("2018", names_to = "year", values_to = "time")

ggplot(total_time) +
  geom_line(aes(age, time, group = interaction(sex, year), color = sex), size = 0.5) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  scale_y_continuous(limits = c(0, 700)) +
  scale_color_manual(values = c("mediumseagreen", "purple"), name = "", labels = c("Women", "Men")) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  labs(
    title = "Older patients and female patients spend more time at the\nemergency departments of most Swedish Regions in 2018",
    subtitle = "Showing 90th percentile of length of stay in minutes, by age group.",
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) | Graphic: Georgios Karamanis"
  ) +
  facet_wrap(vars(region), nrow = 3) +
  # facet_geo(~region, grid = "se_counties_grid1") +
  theme_linedraw(base_family = "IBM Plex Sans") +
  theme(
    legend.position = c(0.5, 1.14),
    legend.text = element_text(family = "IBM Plex Sans Medium", size = 15),
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "#004B87", color = NA),
    strip.text = element_text(family = "IBM Plex Sans Medium", color = "white", size = 12),
    panel.spacing = unit(1, "lines"),
    axis.title = element_blank(),
    plot.title = element_text(family = "IBM Plex Sans Bold", size = 18),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0), size = 14),
    plot.caption = element_text(margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here::here("202001", "plots", paste0("total-time-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 18, height = 10)
