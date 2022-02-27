# # Advent of Code 2021
# # Dec 18
# 
# WIP

library(jsonlite)
library(tidyverse)

input <- read_lines("2021/input/dec18.txt") 

input <- sapply(input, fromJSON)

snailfish_add <- function(snailfish_num1,snailfish_num2) {
  snailfish_num <- list(snailfish_num1,snailfish_num2)
  snailfish_num
}

snailfish_num <- snailfish_add(input[[1]],input[[2]])

explode <- function(snailfish_num) {
  
  
  
  snailfish_num
}

split_n <- function(snailfish_num) {
  
  snailfish_num
}

reduce_n <- function(snailfish_num) {
  while (vec_depth(snailfish_num) >= 5) {
    snailfish_num <- explode(snailfish_num)
  }
  if (any num >= 10) {
    snailfish_num <- split(snailfish_num)
  }
  snailfish_num
}

test <- map_depth(snailfish_num,4,`[`,.ragged = T)

test <- imap(snailfish_num, ~ paste0(.y,":",.x))
