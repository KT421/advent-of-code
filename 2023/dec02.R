# Advent of Code 2023
# December 2

library(tidyverse)

input <- read_lines("2023/input/dec02.txt") %>%
  str_split(":|;")

# make it a rectangular dataframe
input <- input  %>%
  lapply(`length<-`, max(lengths(input))) %>%
  data.frame() %>%
  data.table::transpose()

colnames(input) <- c("game",paste0("round_",1:6)) 

games <- input %>%
  mutate(game = row_number()) %>%
  pivot_longer(round_1:round_6) %>%
  separate_longer_delim(cols = everything(), delim = ",") %>%
  separate_wider_delim(cols = value, delim = " ", names = c("x", "value", "color"))

# PT1
# find which games are valid, sum up game ids

games <- games %>%
  mutate(value = as.numeric(value),
    valid = case_when(
    color == "red" & value > 12 ~ "invalid",
    color == "green" & value > 13 ~ "invalid",
    color == "blue" & value > 14 ~ "invalid",
    TRUE ~ "valid"
  )) %>%
  group_by(game) %>%
  mutate(valid = ifelse(any(valid == "invalid"), "invalid", "valid"))

games %>% filter(valid == "valid") %>%
  pull(game) %>% unique() %>% as.numeric() %>% sum()

# PT2
# using the minimum possible dice per game, find game power (multiply colors) and sum

games_mins <- games %>%
  group_by(game, color) %>%
  summarise(min = max(value)) %>%
  pivot_wider(names_from = color, values_from = min) %>%
  mutate(power = red * green * blue)

sum(games_mins$power)
