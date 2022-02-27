# aoc 2015
# day 1

library(tidyverse)

input <- read_lines("2015/input/day1.txt") %>% str_split("") %>% unlist()

# open paren means up, close paren means down

sum(grepl("\\(",input)) - sum(str_detect(input,"\\)"))

# first basement level (negative number)

counter <- 0

for (i in 1:length(input)) {
  if (grepl("\\(",input[i])) {
    counter <- counter + 1
  } else {
    counter <- counter - 1
  }
    
    if (counter < 0) {
      print(i)
      break
    }
}
