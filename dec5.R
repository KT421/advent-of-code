# Advent of Code 2021
# Dec 4

# given x,y endpoints, find number points where lines intersect at least once
# puzzle 1 says consider only horizontal and vertical. Going to bet that puzzle 2 says "lol now with diagonals"

library(tidyverse)

input <- read_lines("input/dec5.txt")

input <- read_lines("input/dec5_test.txt")

endpoints <- t(data.frame(strsplit(input, " -> ")))

#function that takes in endpoints data and returns a dataframe of x,y points
locate_vents <- function(vents, diagonals) {
  start_coord <- unlist(strsplit(vents[1],","))
  end_coord <- unlist(strsplit(vents[2],","))
  
  #skip diags
  run <- T
  if (diagonals == FALSE) {
    if (any((start_coord[1] == end_coord[1]),(start_coord[2] == end_coord[2]))) {
      run <= T 
      } else {
        run <- F
        } 
  }
  
  if (run == T)
         {
    x <- c(start_coord[1]:end_coord[1])
    y <- c(start_coord[2]:end_coord[2])
    
    vent_locs <- data.frame(x, y)
    
    } else {
      vent_locs <- NULL
      }

  vent_locs
}

#tests
vents_straight <- endpoints[1,]
vents_diag <- endpoints[6,]

locate_vents(vents_straight,T)
locate_vents(vents_straight,F)
locate_vents(vents_diag,T)
locate_vents(vents_diag,F)

# find all vent locations

vent_locations <- NULL

for (i in 1:nrow(endpoints)) {
  vents <- locate_vents(endpoints[i,], diagonals = FALSE)
  vent_locations <- rbind(vents,vent_locations)
}

# find total number of spots with > 1

test <- vent_locations %>%
  mutate(coords = paste(x,y)) 

  table(coords)
