# AOC 2022
# Dec 09

# WIP

library(tidyverse)

input <- readLines("2022/input/dec09.txt") %>% strsplit("\n") %>% unlist() %>% tibble(data = .) 

input <- separate(input, data,c("direction","value"),sep = " ")

test_input <- "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2" %>% strsplit("\n") %>% unlist() %>% tibble(data = .) 

test_input <- separate(test_input, data,c("direction","value"),sep = " ")

tail_needs_to_move <- function(head_pos, tail_pos) {
  ifelse(abs(head_pos[1]-tail_pos[1]) > 1 || abs(head_pos[2]-tail_pos[2]) > 1, T, F) 
}

# PT 1
head_pos <- c(0,0)
tail_pos <- c(0,0)
hist <- data_frame(head_pos[1], head_pos[2], tail_pos[1], tail_pos[2])

for (i in 1:2000) {
  
  direction <- input[[i,1]]
  value <- input[[i,2]]
  
    for (j in 1:as.numeric(value)) {
  # move head pos
    if (direction == "U") head_pos[2] <- head_pos[2] + 1
    if (direction == "D") head_pos[2] <- head_pos[2] - 1
    if (direction == "R") head_pos[1] <- head_pos[1] + 1
    if (direction == "L") head_pos[1] <- head_pos[1] - 1
  
    # if tail needs to move, move it too
    if (tail_needs_to_move(head_pos, tail_pos)) {
      
      
      # if 2 steps off inline, move one step to match

    if (head_pos[1] == tail_pos[1] | head_pos[2] == tail_pos[2]) {
    
      if ((head_pos[1]-tail_pos[1]) > 1) tail_pos[1] <- tail_pos[1] + 1
      if ((tail_pos[1]-head_pos[1]) > 1) tail_pos[1] <- tail_pos[1] - 1
      if ((head_pos[2]-tail_pos[2]) > 1) tail_pos[2] <- tail_pos[2] + 1
      if ((tail_pos[2]-head_pos[2]) > 1) tail_pos[2] <- tail_pos[2] - 1
      
    } else { 
      # if not in same column, move one step diagonal
      if (direction %in% c("U","D")) {
        tail_pos[1] <- head_pos[1]
        if ((head_pos[2]-tail_pos[2]) > 1) tail_pos[2] <- tail_pos[2] + 1
        if ((tail_pos[2]-head_pos[2]) > 1) tail_pos[2] <- tail_pos[2] - 1
      }
      if (direction %in% c("R","L")) {
        tail_pos[2] <- head_pos[2]
        if ((head_pos[1]-tail_pos[1]) > 1) tail_pos[1] <- tail_pos[1] + 1
        if ((tail_pos[1]-head_pos[1]) > 1) tail_pos[1] <- tail_pos[1] - 1
      }
    }
    }
      
    hist <- rbind(hist, data_frame(head_pos[1], head_pos[2], tail_pos[1], tail_pos[2])) 
  }


}


# How many positions visited once
all_tail_pos <- hist[,3:4] %>%
  distinct() %>%
  nrow()
