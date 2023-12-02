# AOC 2022
# Dec 12

# Pathfinding day

# WIP

library(tidyverse)

input <- readLines("2022/input/dec12.txt") %>% strsplit("") %>% unlist() %>%
  matrix(ncol = 173, nrow = 41, byrow = T)

# note start and end and replace them with their actual values
start <- which(input == "S", arr.ind = T)
end <- which(input == "E", arr.ind = T)

input[start] <- "a"
input[end] <- "z"

valid_step <- function(letter_a,letter_b) {
  if ((which(letters == letter_a)+1) >= which(letters == letter_b)) {
    return(TRUE)
    }  else {
    return(FALSE)
    }
  }

check_adj <- function(row,column) {
  
  current_letter <- input[row,column]
  
  up <- ifelse(row != 1, input[row-1,column])
  down <- ifelse(row != 41) input[row+1,column])
  left <- ifelse(column != 1) input[row,column-1])
  right <- ifelse(column != 173) input[row,column+1])
  
}

# PT1 



