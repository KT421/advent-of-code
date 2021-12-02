# Advent of Code 2021
# Dec 2
# Puzzle #1

# calculate sub location starting from 0,0 given directions

library(tidyverse)

steps <- read_csv("input/dec2.txt", col_names = FALSE)

steps <- separate(steps, X1, c("direction", "value"), sep = " ")

steps$value <- as.numeric(steps$value)

x_loc <- sum(steps$value[steps$direction == "forward"])

z_loc <- sum(steps$value[steps$direction == "down"]) - sum(steps$value[steps$direction == "up"])

x_loc * z_loc


# Part 2

#calculate new location based on third "aim" parameter

x_loc2 <- NULL
z_loc2 <- NULL

for (step in steps) {
  
}