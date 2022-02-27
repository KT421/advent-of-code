# Advent of Code 2021
# Dec 4

# Find exponentially growing num of lanternfish on day 80

library(tidyverse)

input <- read_lines("2021/input/dec6.txt") %>% 
  strsplit(",") %>% 
  unlist() %>%
  table() 

lanternfish_tracker <- tibble(days_to_spawn = c(0:8), num_lanternfish = 0)
lanternfish_tracker$num_lanternfish[2:6] <- as.numeric(input)


# function to iterate lanternfish tracker

spawn_lanternfish <- function(lanternfish_tracker) {
  
  new_table <- lanternfish_tracker
  
  new_table$num_lanternfish <- lead(new_table$num_lanternfish, default = 0)
  
  new_table$num_lanternfish[new_table$days_to_spawn == 6] <- new_table$num_lanternfish[new_table$days_to_spawn == 6] + lanternfish_tracker$num_lanternfish[lanternfish_tracker$days_to_spawn == 0]
  
  new_table$num_lanternfish[new_table$days_to_spawn == 8] <- lanternfish_tracker$num_lanternfish[lanternfish_tracker$days_to_spawn == 0]
  
  new_table
    
}

# part 1

for (day in 1:80) {
  lanternfish_tracker <- spawn_lanternfish(lanternfish_tracker)
}
  
sum(lanternfish_tracker$num_lanternfish)

# part 2

for (day in 1:256) {
  lanternfish_tracker <- spawn_lanternfish(lanternfish_tracker)
}

sum(lanternfish_tracker$num_lanternfish) %>% format(scientific = F)
