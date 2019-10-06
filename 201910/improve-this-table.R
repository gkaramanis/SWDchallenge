library(tidyverse)
library(here)
library(janitor)
library(treemapify)
library(cowplot)
library(wesanderson)

clients <- readxl::read_xlsx(here("201910", "data", "2.1 EXERCISE.xlsx"), range = "B7:F12") %>% 
  clean_names() %>% 
  mutate(rev_per_acc = revenue_m/number_of_accounts)

pal <- wes_palette("Moonrise3", 5)

# common layers and theme
gglist <- list(
  geom_treemap(color = "grey40"),
    geom_treemap_text(
      family = "IBM Plex Mono Medium",
      color = "grey99", 
      place = "centre",
      grow = TRUE),
  scale_fill_manual(values = pal),
  theme_void(),
    theme(
      legend.position = "none",
      plot.margin = margin(10, 20, 20, 20),
      plot.title = element_text(family = "IBM Plex Sans", size = 19, color = "grey40", margin = margin(0, 0, 10, 0)),
      plot.caption = element_text(family = "IBM Plex Sans", size = 8, color = "grey50", margin = margin(5, 0, 10, 0))
    )
)

# title
title <- ggplot() +
  labs(title = "Comparison of tiers by") +
  gglist +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, color = "grey30", margin = margin(10, 0, 0, 0))
  )

# accounts treemap
p_acc <- ggplot(clients, aes(area = number_of_accounts, fill = tier, label = tier)) +
  gglist +
  labs(title = "Number of accounts")
  
# revenue treemap
p_rev <- ggplot(clients, aes(area = revenue_m, fill = tier, label = tier)) +
  gglist +
  labs(title = "Total revenue") +
  theme(plot.margin = margin(10, 5, 20, 5))

# revenue/account treemap
p_rpc <- ggplot(clients, aes(area = rev_per_acc, fill = tier, label = tier)) +
  labs(title = "Revenue per account", caption = "Data: #SWDchallenge | Graphic: Georgios Karamanis") +
  gglist +
  theme(plot.margin = margin(10, 20, 0, 15))

row <- plot_grid(p_acc, p_rev, p_rpc, nrow = 1)

plot_grid(title, row, ncol = 1, rel_heights = c(0.1, 1)) +
  ggsave(here("201910", "figures", "improved.png"),
         width = 15, height = 6, dpi = 320)

