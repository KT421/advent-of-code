# Advent of Code 2021
# Dec 3
# Puzzle #1

library(tidyverse)

diagnostics <- read_lines("2021/input/dec3.txt") 

diagnostics <- data.frame(str_split_fixed(diagnostics,"",12))

comparison <- apply(diagnostics, 2, table)

gamma_rate <- strtoi(paste0(apply(comparison, 2, which.max) - 1,collapse = ""), base = 2L)

epsilon_rate <- strtoi(paste0(apply(comparison, 2, which.min) - 1, collapse = ""), base = 2L)

gamma_rate * epsilon_rate

# Puzzle #2

# o2 rating = most common value in current bit position. 1 wins ties
# co2 rating = least common value in current bit position, 0 wins ties
# filter to keep only numbers with that value in that bitposition, 
# then move to next bit

# o2

o2 <- diagnostics

for (i in 1:12){
 current_bit <- o2[,i]
 
 bit <- ifelse(length(current_bit[current_bit == "1"]) >= length(current_bit)/2, "1", "0")
 
 o2 <- o2 %>%
   filter(.[[i]] == bit)
 
 if (nrow(o2) == 1) {
   o2 <- strtoi(paste0(o2[1,], collapse=""), base = 2L)
   break
 }
 }

# co2

co2 <- diagnostics

for (i in 1:12){
  current_bit <- co2[,i]
  
  bit <- ifelse(length(current_bit[current_bit == "0"]) <= length(current_bit)/2, "0", "1")
  
  co2 <- co2 %>%
    filter(.[[i]] == bit)
  
  if (nrow(co2) == 1) {
    co2 <- strtoi(paste0(co2[1,], collapse=""), base = 2L)
    break
  }
}

# total

o2 * co2
