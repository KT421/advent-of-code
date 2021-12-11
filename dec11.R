# Advent of Code 2021
# Dec 11

# if octopus > 9, increments neighbors
# then all > 9 flash and reset to 0

library(tidyverse)

input <- read_lines("input/dec11.txt") %>% str_split("") %>% unlist() %>% as.numeric() %>% matrix(nrow = 10, ncol = 10, byrow=T)

test_input <- read_lines("5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526") %>% str_split("") %>% unlist() %>% as.numeric() %>% matrix(nrow = 10, ncol = 10, byrow=T)

octopi <- input

increment_octopi <- function(octopi) {
  octopi <- octopi + 1
  octopi
}

neighbors_in_bounds <- function(octopus) {
  adjacent_octopi <- c(octopus - 10, octopus + 10)
  
  if ((octopus-1) %% 10 != 0) adjacent_octopi <- append(adjacent_octopi,c(octopus -11, octopus -1, octopus + 9))
  if ((octopus) %% 10 != 0) adjacent_octopi <- append(adjacent_octopi,c(octopus -9, octopus +1, octopus +11))
  
  adjacent_octopi <- adjacent_octopi[adjacent_octopi %in% 1:100]

  adjacent_octopi
  }

flash_counter <- NULL

for (steps in 1:100) {
  octopi <- increment_octopi(octopi)
  
  flashes <- ifelse(octopi > 9, "to flash", "waiting")

  while (TRUE) {
    
    for (octopus in 1:length(flashes)) {
      if (flashes[octopus] != "to flash") next
      
      adjacent_octopi <- neighbors_in_bounds(octopus)
      
      octopi[adjacent_octopi] <- octopi[adjacent_octopi] + 1
      
      # set octopi > 9 to "to flash" without popping any "flashed" to flash twice
      
      for (adjacent in adjacent_octopi) {
        if (flashes[adjacent] != "flashed") {
          if (octopi[adjacent] > 9) {
            flashes[adjacent] <- "to flash"
          }
        }
      }
      
      flashes[octopus] <- "flashed"
    }
    
    if (sum(flashes == "to flash") == 0) break
  }
  flash_counter <- append(flash_counter,sum(flashes == "flashed"))
  octopi[octopi > 9] <- 0
}

sum(flash_counter)

# part 2

# find step when all are in sync

# reset input
octopi <- input

step_counter <- 0

while (TRUE) {

  octopi <- increment_octopi(octopi)
  
  flashes <- ifelse(octopi > 9, "to flash", "waiting")
  
  while (TRUE) {
    
    for (octopus in 1:length(flashes)) {
      if (flashes[octopus] != "to flash") next
      
      adjacent_octopi <- neighbors_in_bounds(octopus)
      
      octopi[adjacent_octopi] <- octopi[adjacent_octopi] + 1
      
      # set octopi > 9 to "to flash" without popping any "flashed" to flash twice
      
      for (adjacent in adjacent_octopi) {
        if (flashes[adjacent] != "flashed") {
          if (octopi[adjacent] > 9) {
            flashes[adjacent] <- "to flash"
          }
        }
      }
      
      flashes[octopus] <- "flashed"
    }
    
    if (sum(flashes == "to flash") == 0) break
  }
  
  octopi[octopi > 9] <- 0
  
  step_counter <- step_counter + 1
  print(step_counter)
  if (sum(flashes == "flashed") == 100) break
}
