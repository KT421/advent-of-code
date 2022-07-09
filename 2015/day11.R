# aoc 2015
# day 11

library(tidyverse)

input <- "vzbxkghb"

# define Santa's bad password rules

straights <- rbind(lag(letters,2), lag(letters,1), letters) %>% 
  t() %>%
  na.omit() %>%
  as.data.frame() %>%
  unite("straights", 1:3, remove = T, sep = "") %>%
  pull(straights)

is_valid <- function(santa_password) {
  all(ifelse(grepl(paste(straights, collapse="|"),santa_password),T,F),
      ifelse(grepl("i|o|l",santa_password),F,T),
      ifelse(grepl("(.)\\1",santa_password),T,F)) # this is insufficient <- must contain TWO DIFFERENT sequences
}

# increment santa's password

#### HOLD UP

# this is easy to solve with meat brain

# pt 1 

# solve attending to only last 5 characters with next valid sequence

# vzbxxyzz

# pt 2 

# cause at the end with z, wrap around char 3 and do aabcc

#vzcaabcc