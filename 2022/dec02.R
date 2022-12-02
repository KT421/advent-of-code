# AOC 2022 
# Day 02

# Solution V2 (using a lookup table - see commit history for clunky if statements)
# it could be even shorter if I didn't convert ABC or XYZ to human words, but 
# then I would get too confused. I'm already too confused.

library(tidyverse)

input <- readLines("2022/input/dec02.txt") %>% 
  as.data.frame() %>%
  separate(1, c("a","b"), sep = " ")

# all possible results
result_lookup <- data.frame(
  p1 = c("rock","rock","rock","paper","paper","paper","scissors","scissors","scissors"),
  p2 = c("rock","paper","scissors","rock","paper","scissors","rock","paper","scissors"),
  winner = c("draw","win","lose","lose","draw","win","win","lose","draw"),
  score = c(4,8,3,1,5,9,7,2,6)
)

# PT1

# interpret data
rps1 <- input %>%
  transmute(p1 = case_when(
    a == "A" ~ "rock",
    a == "B" ~ "paper",
    a == "C" ~ "scissors"
  ),
  p2 = case_when(
    b == "X" ~ "rock",
    b == "Y" ~ "paper",
    b == "Z" ~ "scissors"
  ))

rps1 %>%
  left_join(result_lookup, by = c("p1", "p2")) %>%
  pull(score) %>%
  sum()

# PT2

# interpret data
rps2 <- input %>%
  transmute(p1 = case_when(
    a == "A" ~ "rock",
    a == "B" ~ "paper",
    a == "C" ~ "scissors"
  ),
  winner = case_when(
    b == "X" ~ "lose",
    b == "Y" ~ "draw",
    b == "Z" ~ "win"
  ))

# determine winning move
rps2 %>%
  left_join(result_lookup, by = c("p1", "winner")) %>%
  pull(score) %>%
  sum()
