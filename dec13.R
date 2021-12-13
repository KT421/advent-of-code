# Advent of Code 2021
# Day 13

library(tidyverse)

input <- read_lines("input/dec13.txt")

input_grid <- input[1:1017]

input_folds <- input[1019:1030]

# test input 

input <- read_lines("6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

input_grid <- input[1:18]

input_folds <- input[20:21]

# prepare grid

input_grid <- input_grid %>% strsplit(",") %>% tibble() %>% unnest_wider(col = c(.))

# add ones because R is 1 indexed
input_grid$x <- as.numeric(input_grid$...1) + 1
input_grid$y <- as.numeric(input_grid$...2) + 1

grid <- matrix(0,ncol=max(input_grid$x),nrow=max(input_grid$y))

for (i in 1:nrow(input_grid)) {
  x = input_grid[i,3][[1]]
  y = input_grid[i,4][[1]]
  
  grid[y,x] <- 1
}

# prepare inputs

folds <- str_extract(input_folds,"[x-y]=\\d+") %>% strsplit("=") %>% tibble() %>% unnest_wider(col = c(.))
folds$direction <- folds$...1
folds$value <- as.numeric(folds$...2) + 1

# prepare fold function

fold_grid <- function(instruction) {
  direction <- instruction$direction
  value <- instruction$value
  
  # vertical fold
  if (direction == "x") {
    grid_a <- grid[,1:(value-1)]
    grid_b <- grid[,c(ncol(grid):(value+1)),drop = FALSE]
    
  }
  
  # horizontal fold
  if (direction == "y") {
    
  }
}

# part 1: number of dots after first fold

# part 2: number of dots after all folds