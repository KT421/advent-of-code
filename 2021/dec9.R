# Advent of Code 2021
# Dec 9

library(tidyverse)

input <- read_lines("2199943210
3987894921
9856789892
8767896789
9899965678") %>% str_split("") %>% unlist() %>% as.numeric() %>% matrix(ncol=10)

input <- read_lines("2021/input/dec9.txt") %>% str_split("") %>% unlist() %>% as.numeric() %>%  matrix(ncol=100, byrow = TRUE)

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
  
  if (up <= 0 ) up <- NA
  if (down > length(input) ) down <- NA
  if ((left %% 100 == 0) ) left <- NA
  if ((right-1) %% 100 == 0 ) right <- NA
  
  cells_in_bounds <- na.omit(c(up,down,left,right))
  cells_in_bounds <- cells_in_bounds[visited[cells_in_bounds] == FALSE]
  cells_in_bounds
}


for (i in 1:length(input)) {
  
  print(i)
  
  # check to see if in an existing basin or is not in a basin
  if (visited[i] == TRUE) next
  
  # create a new basin id
  basin_size <- 0
  basin_contents <- NULL
  
  # starting from i, check neighbors
  # add to checklist
  # if valid, add to basin
  # if list to be checked is length zero, break
 
  cells_to_check <- i
 
   while (length(cells_to_check) > 0) {
  
     current_cell <- cells_to_check[1]
     
     # check if visited
     if (visited[current_cell] == FALSE) {
       # add to visited list 
       visited[current_cell] <- TRUE
     } 
     
       #check value
       if (input[current_cell] != 9) {
         #add neighbors to checklist
         cells_to_check <- append(cells_to_check,neighbors_in_bounds(current_cell))
         #add to basin
         basin_contents <- append(basin_contents,current_cell)
         #increment basin size
         basin_size <- basin_size + 1
       } 

     
     # remove this from checklist
     cells_to_check <- cells_to_check[cells_to_check != current_cell]
      
     if (length(cells_to_check) == 0) break
  }
  
  # add basin results to results table
  basin_contents <- paste(basin_contents,collapse = ",")
  
    basin <- tibble(paste(basin_contents), basin_size)
    
    basins <- bind_rows(basins, basin)
}


# top 3 basins

top_basins <- basins %>% arrange(basin_size) %>% top_n(3)

top_basins$basin_size[1] * top_basins$basin_size[2] * top_basins$basin_size[3]
