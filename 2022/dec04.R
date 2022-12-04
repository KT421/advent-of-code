# AOC 2022
# Day 04

library(tidyverse)

input <- readLines("2022/input/dec04.txt") %>% 
  str_replace_all("-",":") %>%
  strsplit(",") 


# PT1

find_overlap <- function(elf_assignments) {
  elf1 <- eval(parse(text = elf_assignments[[1]][1]))
  elf2 <- eval(parse(text = elf_assignments[[1]][2]))
  
  ifelse(identical(intersect(elf1, elf2), elf1) || identical(intersect(elf1, elf2), elf2),T,F) 
}

overlaps <- NULL
for (i in 1:1000) {
  overlaps <- append(overlaps,find_overlap(input[i]))
}

sum(overlaps)

# PT2

which_overlap <- function(elf_assignments) {
  elf1 <- eval(parse(text = elf_assignments[[1]][1]))
  elf2 <- eval(parse(text = elf_assignments[[1]][2]))
  
  ifelse(is_empty(intersect(elf1,elf2)),F,T)
}

overlaps <- NULL
for (i in 1:1000) {
  overlapping_sections <- which_overlap(input[i])
  overlaps <- append(overlaps,overlapping_sections)
}

sum(overlaps)
