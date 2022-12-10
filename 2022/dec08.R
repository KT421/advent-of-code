# AOC 2022
# Dec 08

library(tidyverse)

input <- readLines("2022/input/dec08.txt") %>% str_split("") %>% unlist() %>% as.numeric() %>% matrix(nrow=99, byrow=T)

# PT1

is_visible <- function(x,y) {
  
  this_tree <- input[x,y]
  
  #if it's on the grid edge, it's visible
  if (any(x == 1, x == 99, y == 1, y == 99))   return(T)

  # is it visible?
  if (all(
        any(input[1:(x-1),y] >= this_tree), #up
        any(input[(x+1):99,y] >= this_tree),#down
        any(input[x,1:(y-1)] >= this_tree), #left
        any(input[x,(y+1):99] >= this_tree) #right
  )) {
    return(F)
  } else {
    return(T)
  }
  
}

visible_trees <- matrix(nrow=99, ncol=99)

for (x in 1:99) {
  for (y in 1:99)
    visible_trees[x,y] <- is_visible(x,y)
}

sum(visible_trees)

# PT2

scenic_score <- function(x,y) {
  
  this_tree <- input[x,y]
  
  if (any(x == 1, x == 99, y == 1, y == 99))   return(0)
  
  scores <- c(min(which(input[(x-1):1,y] >= this_tree)[1],length(input[(x-1):1]),na.rm = T),    #up 
              min(which(input[(x+1):99,y] >= this_tree)[1],length(input[(x+1):99]),na.rm = T),   #down
              min(which(input[x,(y-1):1] >= this_tree)[1],length(input[(y-1):1]),na.rm = T),    #left
              min(which(input[x,(y+1):99] >= this_tree)[1],length(input[(y+1):99]),na.rm = T))
    
    score <- scores[1]*scores[2]*scores[3]*scores[4]
    
    return(score)

}

scenic_trees <- matrix(nrow=99, ncol=99)

for (x in 1:99) {
  for (y in 1:99)
    scenic_trees[x,y] <- scenic_score(x,y)
}

max(scenic_trees)
