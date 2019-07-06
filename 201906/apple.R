library(tidyverse)
library(ggimage)


df <- read_csv("./201906/SWDchallenge_June19.csv")
# ! changed last actual data point to forecast (because of geom_path)!

df <- df %>% 
  mutate(yearmonth = paste(year, month, sep = ""))

ggplot(df, aes(yearmonth, sales, group = 1)) +
  geom_path(aes(color = type)) +
  # logo
  geom_image(aes(x = 13, y = 3.7,
             image = "./201906/logo.png"), asp = 0.9) +
  # title
  geom_text(aes(x = 13, y = 4.4, label = "Market size over time"),
            family = "SF Pro Display Heavy", 
            size = 4.5, color = "#999999") +
  geom_text(aes(x = 13, y = 4.2, label = "sales in billion US dollars"),
            family = "SF Pro Display", 
            size = 3.5, color = "#999999") +
  # labels - years
  geom_text(aes(x = "201801", y = 1.31, label = "2018"), 
            family = "SF Pro Display", color = "#999999", size = 2.5) +
  geom_segment(aes(x = "201801", y = 1.41,
                   xend = "201801", yend = 1.55),
               color = "#bababa", size = 0.1) +
  geom_text(aes(x = "201901", y = 1.28, label = "2019"), 
            family = "SF Pro Display", color = "#999999", size = 2.5) +
  geom_segment(aes(x = "201901", y = 1.38,
                   xend = "201901", yend = 1.53),
               color = "#bababa", size = 0.1) +
  # labels - sales
  geom_text(aes(x = "201905", y = 2.22, label = "$1.97B"),
            family = "SF Pro Display", 
            size = 3, color = "grey40") +
  # labels - events
  geom_text(aes(x = "201807", y = 0.8, label = "X recalled"), 
            family = "SF Pro Display", color = "#999999", size = 3) +
  geom_segment(aes(x = "201807", y = 1,
                   xend = "201807", yend = 1.49),
               color = "#bababa", size = 0.1) +
  geom_text(aes(x = "201902", y = 0.8, label = "New study"),
            family = "SF Pro Display", color = "#999999", size = 3) + 
  geom_segment(aes(x = "201902", y = 1,
                   xend = "201902", yend = 1.75),
               color = "#bababa", size = 0.1) +
  # caption
  geom_text(aes(x = 13, y = -0.5,
                label = "The 2019 forecast is provided by ABC consultants\nand is based on market data through June. The forecast\nassumes no major market changes.\nGraphic: Georgios Karamanis"),
            family = "SF Pro Display", color = "#999999", size = 2) +
  # stuff
  scale_color_grey(guide = FALSE) +
  scale_y_continuous(limits = c(-1, 4.5)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin=unit(c(1, 1, 0, 1),"cm")
  )

ggsave("./201906/apple.png", dpi = 600, height = 5, width = 5)
# add disclaimer fake data