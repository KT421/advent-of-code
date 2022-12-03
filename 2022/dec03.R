# AOC 2022 
# Day 03

library(tidyverse)

input <- readLines("2022/input/dec03.txt") 

rucksacks <- data.frame(a = substr(input, 1, nchar(input)/2),
                        b = substr(input, nchar(input)/2+1, nchar(input)))

  
# PT 1

# find doubled items

doubles <- data.frame(item = NULL)
for (i in 1:nrow(rucksacks)) {
  doubled <- unlist(strsplit(rucksacks$a[i],""))[(unlist(strsplit(rucksacks$a[i],"")) %in% unlist(strsplit(rucksacks$b[i],"")))]
  doubles <- rbind(doubles,doubled)
}


priorities <- data.frame(items = c(letters,LETTERS),
                     priority = 1:52)

doubles %>% 
  left_join(priorities, by = c(`X.s.` = "items")) %>%
  pull(priority) %>%
  sum()

# PT 2

rucksacks$elf_group <- rep(1:100, each = 3)

rucksacks <- rucksacks %>%
  mutate(rucksack = strsplit(paste0(a,b),"")) 

badges <- data.frame()

for (i in 1:100) {
  group_bags <- rucksacks$rucksack[rucksacks$elf_group == i]
  
  set1 <- group_bags[[1]][group_bags[[1]] %in% group_bags[[2]]]
  set2 <- group_bags[[2]][group_bags[[2]] %in% group_bags[[3]]]
  badge <- set1[set1 %in% set2]
  
  badges <- rbind(badges, badge)
  
}

badges %>%
  left_join(priorities, by = c(`X.j.` = "items")) %>%
  pull(priority) %>%
  sum()
