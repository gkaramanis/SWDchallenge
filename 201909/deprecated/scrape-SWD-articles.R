library(tidyverse)
library(readr)
library(here)

article <- read_file(here("201909", "SWD-201909.txt"))
# tokens <- tibble(text = article) %>% unnest_tokens(word, text)

library(rvest)
url <- "http://www.storytellingwithdata.com/blog/"
webpage <-  read_html(url)

url_ <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>% 
  as.data.frame() %>% 
  filter(str_starts(., "/blog")) %>%
  filter(str_detect(., "#comments", negate = TRUE)) %>%
  distinct()

