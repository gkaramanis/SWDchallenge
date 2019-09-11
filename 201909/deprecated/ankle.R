library(tidyverse)
library(here)
library(cowplot)
library(grid)
library(ggtext)

tbl2 <- read_csv(here("201909", "data", "table2.csv"))
tbl4 <- read_csv(here("201909", "data", "table4.csv"))

# text --------------------------------------------------------------------
p1 <- ggplot() +
  geom_textbox(aes(label = "<span style='color:black'>Abstract<br>
Objectives—To determine the rate of ankle injury and examine risk factors of ankle injuries in mainly recreational bas- ketball players. Methods—Injury observers sat courtside to determine the occurrence of ankle inju- ries in basketball. Ankle injured players and a group of non-injured basketball players completed a questionnaire. Results—A total of 10 393 basketball par- ticipations were observed and 40 ankle injuries documented. A group of non- injured players formed the control group (n = 360). The rate of ankle injury was 3.85 per 1000 participations, with almost half (45.9%) missing one week or more of com- petition and the most common mech- anism being landing (45%). Over half (56.8%) of the ankle injured basketball players did not seek professional treat- ment. Three risk factors for ankle injury were identified: (1) players with a history of ankle injury were almost five times more likely to sustain an ankle injury (odds ratio (OR) 4.94, 95% confidence interval (CI) 1.95 to 12.48); (2) players wearing shoes with air cells in the heel were 4.3 times more likely to injure an ankle than those wearing shoes without air cells (OR 4.34, 95% CI 1.51 to 12.40); (3) players who did not stretch before the game were 2.6 times more likely to injure an ankle than players who did (OR 2.62, 95% CI 1.01 to 6.34). There was also a trend toward ankle tape decreasing the risk of ankle injury in players with a history of ankle injury (p = 0.06). Conclusions—Ankle injuries occurred at a rate of 3.85 per 1000 participations. The three identified risk factors, and landing, should all be considered when preventive strategies for ankle injuries in basketball are being formulated.</span>", x = 0, y = Inf), height = 0.9, width = 0.9, hjust = 0, fill = NA, color = "#e5e8ef")  +
  lims(y = c(0, 3)) +
  # coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "#e5e8ef"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )
 
# table 2 -----------------------------------------------------------------
p2 <- ggplot(tbl2) +
  geom_segment(aes(x = ci1, y = variable, 
                   xend = ci2, yend = variable), size = 1, color = "#d4826d") +
  geom_richtext(aes(x = mean, y = variable, label = mean),
            family = "Helvetica Neue", color = "black", size = 10, fill = "#e5e8ef", label.color = NA) +
  # geom_richtext(aes(x = ci1, y = variable, label = ci1),
  #          family = "Helvetica Neue", color = "black", size = 6, fill = "#e5e8ef", label.color = NA) +
  # geom_richtext(aes(x = ci2, y = variable, label = ci2),
  #          family = "Helvetica Neue", color = "black", size = 6, fill = "#e5e8ef", label.color = NA) +
  geom_richtext(aes(x = -Inf, y = variable, label = label),
            family = "Helvetica Neue Thin", color = "black", size = 6, hjust = 0, fill = NA, label.color = NA) +
  scale_x_continuous(expand = expand_scale(add = c(20, 5))) +
  facet_wrap(vars(variable_group), ncol = 1, scales = "free") +
  labs(
    title = "Characteristics of ankle injured and control basketball players (Mean values with 95% CI)",
    caption = "*Significant difference from control at p<0.05"
    ) +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "#e5e8ef"),
    strip.background = element_rect(fill = "white"),
    strip.text.x = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(family = "Helvetica Neue Thin", color ="black", hjust = 0.5, size = 20, margin = margin(10, 0, 20, 0)),
    plot.caption = element_text(family = "Helvetica Neue Thin", color ="black", hjust = 0.01, size = 8, margin = margin(10, 0, 10, 0)),
    plot.margin = margin(20, 20, 20, 20)
  )

# table 4 -----------------------------------------------------------------
p4 <- ggplot(tbl4) +
  geom_richtext(aes(x = -Inf, y = variable, label = label),
            family = "Helvetica Neue Thin", color = "black", size = 6, hjust = 0, fill = NA, label.color = NA) +
  geom_segment(aes(x = ci1, y = variable, 
                   xend = ci2, yend = variable), size = 1, color = "#d4826d") +
  geom_richtext(aes(x = odds_ratio, y = variable, label = odds_ratio),
            family = "Helvetica Neue", color = "black", size = 10, fill = "#e5e8ef", label.color = NA) +
  # geom_richtext(aes(x = ci1, y = variable, label = ci1),
  #          family = "Helvetica Neue", color = "black", size = 4, fill = "#e5e8ef", label.color = NA) +
  # geom_richtext(aes(x = ci2, y = variable, label = ci2),
  #          family = "Helvetica Neue", color = "black", size = 4, fill = "#e5e8ef", label.color = NA) +          
  labs(
    title = "Multivariate logistic regression: assessing the relation of outcome (ankle injury or no ankle injury) to selected independent variables"
  ) +
  scale_x_continuous(expand = expand_scale(add = c(5, 1))) +
  facet_wrap(vars(variable_group), ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = NA, fill = "#e5e8ef"),
    strip.background = element_rect(fill = "white"),
    strip.text.x = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(family = "Helvetica Neue Thin", color ="black", hjust = 0.5, size = 20, margin = margin(10, 0, 20, 0)),
    plot.caption = element_text(family = "Helvetica Neue Thin", color ="black", hjust = 0.01, size = 8, margin = margin(10, 0, 10, 0)),
    plot.margin = margin(20, 20, 20, 20)
  )

# round my corners! ------------------------------------------------------------
g1 <- ggplotGrob(p1)
bg1 <- g1$grobs[[1]]
round_bg1 <- roundrectGrob(
  x = bg1$x, y = bg1$y,
  width = bg1$width,
  height = bg1$height,
  r = unit(0.05, "snpc"),
  just = bg1$just, name = bg1$name, gp = bg1$gp, vp = bg1$vp)
g1$grobs[[1]] <- round_bg1

g2 <- ggplotGrob(p2)
bg2 <- g2$grobs[[1]]
round_bg2 <- roundrectGrob(
  x = bg2$x, y = bg2$y,
  width = bg2$width,
  height = bg2$height,
  r = unit(0.05, "snpc"),
  just = bg2$just, name = bg2$name, gp = bg2$gp, vp = bg2$vp)
g2$grobs[[1]] <- round_bg2

g4 <- ggplotGrob(p4)
bg4 <- g4$grobs[[1]]
round_bg4 <- roundrectGrob(
  x = bg4$x, y = bg4$y,
  width = bg4$width,
  height = bg4$height,
  r = unit(0.05, "snpc"),
  just = bg4$just, name = bg4$name, gp = bg4$gp, vp = bg4$vp)
g4$grobs[[1]] <- round_bg4

# plot me -----------------------------------------------------------------
row1 <- plot_grid(g1, g2, ncol = 2, nrow = 1, scale = 0.97, rel_widths = c(0.5, 1))

plot_grid(g1, g2, g4, ncol = 1, nrow = 3, scale = 0.97, rel_heights = c(0.4, 1, 1), labels = c("", "", "Results")) +
  theme(plot.background = element_rect(fill = "#a0a5b4", color = NA)) +

  ggsave(here("201909", "ankle-plot.png"), width = 16, height = 16, dpi = 320)

      
#e5e8ef 74.0 % Solitude (Grey)
#a0a5b4 11.5 % Santas Grey (Blue)
#1d78ba 10.5 % Havelock Blue (Blue)
#66a4d1 4.1 % Picton Blue (Blue)