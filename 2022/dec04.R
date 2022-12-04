# AOC 2022
# Day 04
# v2 - much less repetitive 

library(tidyverse)

input <- readLines("2022/input/dec04.txt") %>% 
  str_replace_all("-",":") %>%
  strsplit(",") 

find_overlap <- function(elf_assignments) {
  elf1 <- eval(parse(text = elf_assignments[[1]][1]))
  elf2 <- eval(parse(text = elf_assignments[[1]][2]))
  
  subset <- ifelse(identical(intersect(elf1, elf2), elf1) || identical(intersect(elf1, elf2), elf2),T,F) 
  overlap <- ifelse(is_empty(intersect(elf1,elf2)),F,T)
  
  return(c(subset,overlap))
}

results <- NULL

for (i in 1:1000) {
  results <- rbind(results,find_overlap(input[i]))
}

# PT1

sum(results[,1])

# PT2

sum(results[,2])
