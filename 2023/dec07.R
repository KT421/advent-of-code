# Advent of Code 2023
# Day 07

library(tidyverse)

input <- read_lines("32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

ranks <- ('A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2') %>% str_split_1(", ")

hands <- input %>%
  tibble %>%
  separate_wider_delim(cols = everything(), delim = " ", names = c("cards","bid")) %>%
  sep

hands2 <- hands %>%
  mutate()
  
  
  
  mutate(type = case_when(
    str_detect()
  ))