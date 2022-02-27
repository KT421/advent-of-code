# # Advent of Code 2021
# # Dec 19
# 
# WIP


library(tidyverse)

input <- read_lines("2021/input/dec19.txt") %>% str_split("\n")

# format data into workable structure

breaks <- which(input == "") + 1

breaks <- prepend(breaks,1)
breaks <- append(breaks,1057)

beacons <- NULL
scanner <- 0

for (i in 1:38) {
  
  scanner <- scanner + 1
  
  span <- (breaks[i]+1):(breaks[i + 1]-2)
  
  this_beacon <- input[span] %>%
    tibble() %>%
    separate(c(.), into = c("x","y","z"), sep = ",") %>%
    mutate(scanner_id = scanner,
           x = as.numeric(x),
           y = as.numeric(y),
           z = as.numeric(z))
  
  beacons <- bind_rows(beacons,this_beacon)
  
}

#distance <- function(coords) {
#  sqrt(coords[1]^2 + coords[2]^2 + coords[3]^2)[[1]]
#}

#beacons$distance <- apply(beacons[,1:3],1,distance)

scanners <- tibble(scanner_id = 1:38, scanner_x = NA, scanner_y = NA, scanner_z = NA)
scanners[1,2:4] <- list(0,0,0)

beacons %>% 
  group_by(scanner_id) %>%
  coords
  summarise(rel_dist = dist(x,y,z))
