# Advent of Code 2023
# December 3

# NOT DONE

library(tidyverse)

input <- read_lines("2023/input/dec03.txt")

input <- read_lines('467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..')

schematic <- input %>% 
   str_replace_all("\\."," ") %>% str_split("") %>% unlist() %>% matrix(ncol=10, byrow = T) 

index(str_detect(t(schematic),'[:digit:]'))

