# Advent of Code 2024
# December 1

library(tidyverse)

input <- read_lines("2024/input/dec01.txt") %>%
  str_split("   ") %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame()

list1 <- input %>% pull(V1) %>% sort() %>% as.numeric()
list2 <- input %>% pull(V2) %>% sort() %>% as.numeric()

difference <- abs(list1 - list2)
sum(difference)


# Pt 2

# Similarity Score = For Each item in List 1, score is value * number of appearances in List 2

similarity <- list2[list2 %in% list1] %>% data.frame() 
colnames(similarity) <- "value"

similarity %>% count(value) %>% 
  mutate(sim = value * n) %>%
  pull(sim) %>%
  sum()