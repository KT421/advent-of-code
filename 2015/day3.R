# aoc 2015
# day 3

library(tidyverse)

input <- read_lines("2015/input/day3.txt") %>% str_split("") %>% unlist()

# traverse grid up-down-left-right, count unique cells visited
# start counts as a house

current_coords <- c(x = 0, y = 0)
visited_coords <- tibble(x = current_coords[1], y = current_coords[2])

for (i in 1:length(input)) {
  direction <- input[i]
  
  if (direction == ">") {
    current_coords[1] <- current_coords[1] + 1
  } else if (direction == "<") {
    current_coords[1] <- current_coords[1] - 1
  } else if (direction == "^") {
    current_coords[2] <- current_coords[2] + 1
  } else if (direction == "v") {
    current_coords[2] <- current_coords[2] - 1
  }
  
  visited_coords <- rbind(visited_coords,current_coords)
}

nrow(unique(visited_coords))

# pt 2
# now santa and robo-santa TAKE TURNS reading directions

current_coords_santa <- c(x = 0, y = 0)
current_coords_robo_santa <- c(x = 0, y = 0)
visited_coords <- tibble(x = current_coords_santa[1], y = current_coords_santa[2])

santa_move <- function(coords, direction) {
  if (direction == ">") {
    coords[1] <- coords[1] + 1
  } else if (direction == "<") {
    coords[1] <- coords[1] - 1
  } else if (direction == "^") {
    coords[2] <- coords[2] + 1
  } else if (direction == "v") {
    coords[2] <- coords[2] - 1
  }
  coords
}

for (i in 1:length(input)) {
  direction <- input[i]
  
  if (i %% 2 == 0) {
    current_coords_robo_santa <- santa_move(current_coords_robo_santa, direction)
    visited_coords <- rbind(visited_coords,current_coords_robo_santa)
  } else {
    current_coords_santa <- santa_move(current_coords_santa,direction)
    visited_coords <- rbind(visited_coords,current_coords_santa)
  }
  
  
}

nrow(unique(visited_coords))
