# Advent of Code 2021
# Dec 9

library(tidyverse)

input <- read_lines("2199943210
3987894921
9856789892
8767896789
9899965678") %>% str_split("") %>% unlist() %>% as.numeric() %>% matrix(ncol=10)

input <- read_lines("input/dec9.txt") %>% str_split("") %>% unlist() %>% as.numeric() %>% matrix(ncol=100)

# part 1 - sum all risk levels: local minima (adjacent only - no diags) + 1



# locate local minia 
# use 10 for test and 100 for real input

minima <- NULL

check_adjacent <- function(cell, direction) {
  if (is.na(direction)) 
    return(NA)
  if (cell < input[direction]) 
    return(T)
  else
    return(F)
}

for (i in 1:length(input)) {
  
  up <- i - 100
  down <- i + 100
  left <- i - 1
  right <- i + 1
  
  if (up <= 0) up <- NA
  if (down > length(input)) down <- NA
  if (left %% 100 == 0) left <- NA
  if ((right-1) %% 100 == 0) right <- NA
  
  up <- check_adjacent(input[i],up)
  down <- check_adjacent(input[i],down)
  left <- check_adjacent(input[i],left)
  right <- check_adjacent(input[i],right)
  
  minimum <- sum(up,down,left,right, na.rm = T) == (4 - sum(is.na(up),is.na(down),is.na(left),is.na(right)))
  
  minima <- append(minima,minimum)
}

sum(input[minima] + 1)

# part 2 

# find the three largest basins, multiply number of cells within each
# a basin surrounded on all sides by 9

# define a matrix of cells I have checked. Because 9s are not in basins, don't need to check them
visited <- ifelse(input == 9, T, F)


#store results in here
basins <- tibble()


#function to check adjacent cells to see if they are in the basin

neighbors_in_bounds <- function(cell) {
  up <- cell - 100
  down <- cell + 100
  left <- cell - 1
  right <- cell + 1
  
  if (up <= 0) up <- NA
  if (down > length(input)) down <- NA
  if (left %% 100 == 0) left <- NA
  if ((right-1) %% 100 == 0) right <- NA
  
  cells_in_bounds <- c(up,down,left,right)
  na.omit(cells_in_bounds)
}

for (i in 1:length(input)) {
  
  print(i)
  
  # check to see if in an existing basin or is not in a basin
  if (visited[i] == TRUE) next
  
  basin_id <- length(basins) + 1  
  
  basin_size <- 1
  basin_contents <- i
  
  j <- T
  up <- i
  left <- i
  right <- i
  down <- i
  
  while (TRUE) {
    while (up > 0) {
      up <- up - 100
      if (any(up <= 0,input[up] == 9)) {
        up <- F 
        break
        }
      basin_contents <- append(basin_contents,up)
      basin_size <- basin_size + 1
    }
    while (down < length(input)) {
      down <- down + 100
      if (any(down > length(input), input[down] == 9)) {
        down <- F 
        break
        }
      basin_contents <- append(basin_contents,down)
      basin_size <- basin_size + 1
    }
    while (left > 0) {
      left <- left - 1
      if (any(left %% 100 == 0, input[left] == 9,left<=0)) {
        left <- F 
        break
        }
      basin_contents <- append(basin_contents,left)
      basin_size <- basin_size + 1
    }
    while (right < length(input) ) {
      right <- right + 1
      if (any((right-1) %% 100 == 0, right > length(input), input[right] == 9)) {
        right <- F 
        break
        }
      basin_contents <- append(basin_contents,right)
      basin_size <- basin_size + 1
    }
  }
    basin <- tibble(basin_id, basin_contents, basin_size)
    
    basins <- bind_rows(basins, basin)
}



# testing while loops
testing = 1
while (testing != F) {
  print(testing)
  if (testing == 5) break
  testing <- testing + 1
}
