# aoc 2015
# day 10

library(tidyverse)

input <- 3113322113

look_and_say <- function(input) {
  
  runs <- str_split(input, "") %>% 
    unlist() %>% 
    as.numeric() %>% 
    rle()
  
  output <- rbind(runs$lengths,runs$values)
  
  return(paste0(output, collapse = ""))
  
}

turns <- as.vector(input)

for (i in 1:40) {
  turns[i+1] <- look_and_say(turns[i])
}

nchar(turns[41])

# pt2

for (i in 1:50) {
  turns[i+1] <- look_and_say(turns[i])
}

nchar(turns[51])
