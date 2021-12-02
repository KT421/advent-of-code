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

x_loc2 <- 0
z_loc2 <- 0
aim <- 0

for (i in 1:nrow(steps)) {

  if (steps$direction[i] == "forward") {
    x_loc2 <- x_loc2 + steps$value[i]
    z_loc2 <- z_loc2 + (aim*steps$value[i])
  } else if (steps$direction[i] == "up") {
    aim <- aim - steps$value[i]
  } else if (steps$direction[i] == "down") {
    aim <- aim + steps$value[i]
  }
}

x_loc2 * z_loc2
