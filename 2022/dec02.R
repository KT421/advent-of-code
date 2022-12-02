# AOC 2022 
# Day 02

library(tidyverse)

input <- readLines("2022/input/dec02.txt") %>% 
  as.data.frame() %>%
  separate(1, c("a","b"), sep = " ")

# scoring function
score_round <- function(winner, p2) {
  score <- NULL
  if (winner == "win") {
    score <- 6
  } else if (winner == "draw") {
    score <- 3
  } else {
    score <- 0
  }
  
  if (p2 == "rock") {
    score <- score + 1
  } else if (p2 == "paper") {
    score <- score + 2
  } else {
    score <- score + 3
  }
  
  return(score)
}

# determine winner

play_rps <- function(p1, p2) {
  
  winner <- NULL 
  
  if (any(p1 == "rock" & p2 == "paper", 
          p1 == "paper" & p2 == "scissors",
          p1 == "scissors" & p2 == "rock")) {
    winner <- "win"
  } else if (any(p1 == "rock" & p2 == "scissors", 
                 p1 == "paper" & p2 == "rock",
                 p1 == "scissors" & p2 == "paper")) {
    winner <- "lose"
  } else {
    winner <- "draw"
  }
  
  #return result
  return(winner)
}

# PT1

# interpret data
rps1 <- input %>%
  transmute(p1 = case_when(
    a == "A" ~ "rock",
    a == "B" ~ "paper",
    a == "C" ~ "scissors"
  ),
  p2 = case_when(
    b == "X" ~ "rock",
    b == "Y" ~ "paper",
    b == "Z" ~ "scissors"
  ))

rps1 <- rps1 %>%
  mutate(winner = mapply(play_rps, p1, p2),
         score = mapply(score_round, winner, p2))

sum(rps1$score)

# PT2

rps2 <- input %>%
  transmute(p1 = case_when(
    a == "A" ~ "rock",
    a == "B" ~ "paper",
    a == "C" ~ "scissors"
  ),
  winner = case_when(
    b == "X" ~ "lose",
    b == "Y" ~ "draw",
    b == "Z" ~ "win"
  ))

determine_winning_move <- function(p1, winner) {
  
  # determine winner
  p2 <- NULL 
  
  if (any(p1 == "scissors" & winner == "win", 
          p1 == "paper" & winner == "lose",
          p1 == "rock" & winner == "draw")) {
    p2 <- "rock"
  } else if (any(p1 == "rock" & winner == "win", 
                 p1 == "scissors" & winner == "lose",
                 p1 == "paper" & winner == "draw")) {
    p2 <- "paper"
  } else if (any(p1 == "paper" & winner == "win", 
         p1 == "rock" & winner == "lose",
         p1 == "scissors" & winner == "draw")) {
      p2 <- "scissors"
    }
    
  return(p2)
}

rps2 <- rps2 %>%
  mutate(p2 = mapply(determine_winning_move, p1, winner),
         score = mapply(score_round, winner, p2))

sum(rps2$score)
