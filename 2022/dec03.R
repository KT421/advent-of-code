# AOC 2022 
# Day 03
# v2 - cleaned up

library(tidyverse)

input <- readLines("2022/input/dec03.txt") 

rucksacks <- data.frame(a = substr(input, 1, nchar(input)/2),
                        b = substr(input, nchar(input)/2+1, nchar(input))) %>%
  mutate(a = strsplit(a,""),
         b = strsplit(b,""))

priorities <- data.frame(items = c(letters,LETTERS),
                         priority = 1:52)
  
# PT 1

# find doubled items

find_doubles <- function(a,b) {
    intersect(unlist(a),unlist(b))
}

rucksacks %>% 
  mutate(items = mapply(find_doubles, a, b)) %>%
  left_join(priorities, by = c("items")) %>%
  pull(priority) %>%
  sum()

# PT 2

rucksacks$elf_group <- rep(1:100, each = 3)

rucksacks <- rucksacks %>%
  mutate(rucksack = strsplit(paste0(a,b),"")) 

badges <- data.frame()

for (i in 1:100) {
  group_bags <- rucksacks$rucksack[rucksacks$elf_group == i]
  
  badge <- intersect(group_bags[[1]], intersect(group_bags[[2]], group_bags[[3]]))
  
  badges <- rbind(badges, badge)
  
}

badges %>%
  left_join(priorities, by = c(`X.j.` = "items")) %>%
  pull(priority) %>%
  sum()
