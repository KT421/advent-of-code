# Advent of Code 2021
# Dec 4
# Puzzle #1

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
winner <- FALSE

for (i in 1:length(bingo_numbers)) {
  drawn_numbers <- bingo_numbers[1:i]
  
  # check each board for winning

  for (j in 1:length(prepped_boards)) {
    board <- prepped_boards[[j]]
    
    # check each column and row
    for (k in 1:5) {
      
    if (all(board[,k] %in% drawn_numbers) | all(board[k,] %in% drawn_numbers))
        { print(paste("Winner winner chicken dinner! Board#: ",j))
          winner = T }
    
    if (winner == T) {
      winning_board <- j
      break
      }

    }

    if (winner == T) break
  }
    if (winner == T) break
}

# calculate winning score

# last number called * sum(all uncalled values on board)

unmarked_nums <- unlist(prepped_boards[[winning_board]]) 
unmarked_nums <- unmarked_nums[!unmarked_nums %in% drawn_numbers]

as.numeric(tail(drawn_numbers,1)) * sum(unmarked_nums)


# Part 2

# let the squid win
# find the LAST board to win

# modify the loop to return an index of winner num

drawn_numbers <- NULL
winning_board <- NULL
winners <- data.frame(matrix(ncol=3,nrow=0))
colnames(winners) <- c("draw", "num","board")

for (i in 1:length(bingo_numbers)) {
  drawn_numbers <- bingo_numbers[1:i]
  
  # check each board for winning
  
  for (j in 1:length(prepped_boards)) {
    board <- prepped_boards[[j]]
    
    # check each column and row
    for (k in 1:5) {
      winner = F
      
      if (all(board[,k] %in% drawn_numbers) | all(board[k,] %in% drawn_numbers))
      { winner = T }
      
      if (winner == T & !j %in% winners[,3]) {
        win <- c(draw = i, num = as.numeric(bingo_numbers[i]), board = j)
        winners <- bind_rows(winners, win)
        break
        
      }

    }
    
    
  }
  if (length(winners$num) == length(prepped_boards)) break
}

# calculate score

losing_board <- tail(winners,1)

unmarked_nums <- unlist(prepped_boards[[losing_board$board]]) 
unmarked_nums <- unmarked_nums[!unmarked_nums %in% drawn_numbers]

as.numeric(losing_board$num) * sum(unmarked_nums)
