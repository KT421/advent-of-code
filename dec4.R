# Advent of Code 2021
# Dec 4

library(tidyverse)

# find first winning bingo board
# test

input <- read_lines("input/dec4.txt", n_max = 1)

bingo_numbers <- str_split(input[1], ",")[[1]]

#create a list containing each board as a dataframe

boards <- read_lines("input/dec4.txt", skip = 1, skip_empty_rows = TRUE)

boards <- split(boards, rep(seq_along(boards), each = 5)[seq_along(boards)])

prepped_boards <- list()

for (i in 1:100) {
  board <- str_extract_all(boards[[i]], "[\\b(\\d+)\\b]+")
  board <- data.frame(board)
  board[] <- lapply(board, as.numeric)
  colnames(board) <- c("A","B","C","D","E")
  prepped_boards <- append(prepped_boards,list(board))
}

# now to play bingo

drawn_numbers <- NULL
winning_board <- NULL
winners <- data.frame(matrix(ncol=3,nrow=0))
colnames(winners) <- c("draw", "num","board")

for (i in 1:length(bingo_numbers)) {
  drawn_numbers <- bingo_numbers[1:i]
  
  # check each board for winning
  
  for (j in 1:length(prepped_boards)) {
    
    #skip boards that have already won
    if (j %in% winners$board) next
      
    board <- prepped_boards[[j]]
    
    # check each column and row
    for (k in 1:5) {
      winner <- F
      
      if (all(board[,k] %in% drawn_numbers) | all(board[k,] %in% drawn_numbers))
      { winner <- T }
      
      if (winner == T) {
        print(paste("Winner winner chicken dinner! Board: ",j))
        win <- c(draw = i, num = as.numeric(bingo_numbers[i]), board = j)
        winners <- bind_rows(winners, win)
        break
        
      }

    }
    
  }
  if (length(winners$num) == length(prepped_boards)) break
}

# Part 1

# which board wins first

winning_board <- head(winners,1)

unmarked_nums_win <- unlist(prepped_boards[[winning_board$board]]) 
unmarked_nums_win <- unmarked_nums_win[!unmarked_nums_win %in% drawn_numbers[1:winning_board$draw]]

as.numeric(winning_board$num) * sum(unmarked_nums_win)

# Part 2

# let the squid win
# find the LAST board to win

losing_board <- tail(winners,1)

unmarked_nums_lose <- unlist(prepped_boards[[losing_board$board]]) 
unmarked_nums_lose <- unmarked_nums_lose[!unmarked_nums_lose %in% drawn_numbers]

as.numeric(losing_board$num) * sum(unmarked_nums_lose)

