# aoc 2015
# day 2

library(tidyverse)

input <- read_lines("2015/input/day2.txt")

# find wrapping paper dimensions
# find surface area of each, plus smallest side
# return total

paper_per_present <- function(dimensions) {
  dims <- str_split(dimensions, "x") %>% unlist() %>% as.numeric()
  a <- dims[1] * dims[2]
  b <- dims[1] * dims[3]
  c <- dims[2] * dims[3]
  total <- (a+b+c)*2 + min(a,b,c)
  total
}

map(input,paper_per_present) %>% unlist() %>% sum()

# pt 2
# find ribbon length needed
# perimeter of smallest side + volume

ribbon_per_present <- function(dimensions) {
  dims <- str_split(dimensions, "x") %>% unlist() %>% as.numeric()
  a <- dims[1] + dims[2]
  b <- dims[1] + dims[3]
  c <- dims[2] + dims[3]
  
  min(a,b,c)*2 + (dims[1]*dims[2]*dims[3])
}

map(input,ribbon_per_present) %>% unlist() %>% sum()
