# AOC 2022 
# Day 01

library(tidyverse)

input <- read_file("2022/input/dec01.txt") %>% str_split("\\n\\n") %>% unlist()

# PT1

calories_df <- data.frame(elf_id = 1:260, calories = input) 

calories_df <- separate_rows(calories_df, calories, sep = "\\n")

calories_df$calories <- as.numeric(calories_df$calories)

calories_by_elf <- calories_df %>%
  na.omit() %>%
  group_by(elf_id) %>%
  summarise(total_cal = sum(calories)) 

max(calories_by_elf$total_cal)

# PT2

sort(calories_by_elf$total_cal, decreasing = T) %>%
  head(3) %>%
  sum()
