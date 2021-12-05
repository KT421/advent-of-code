# Advent of Code 2021
# Dec 4

# given x,y endpoints, find number points where lines intersect at least once
# puzzle 1 says consider only horizontal and vertical. Going to bet that puzzle 2 says "lol now with diagonals"

library(tidyverse)

input <- read_lines("input/dec5.txt")

endpoints <- t(data.frame(strsplit(input, " -> ")))

#### function for finding vent locations from endpoints ####

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

#### locate_vents() tests ####

vents_straight <- endpoints[1,]
vents_diag <- endpoints[6,]

locate_vents(vents_straight,T)
locate_vents(vents_straight,F)
locate_vents(vents_diag,T)
locate_vents(vents_diag,F)

#### function for finding danger spots ####

count_danger_spots <- function(vent_locations) {
  vent_locations %>%
    group_by(x,y) %>%
    summarise(count = n()) %>%
    filter(count > 1) %>%
    nrow()
}

#### solutions ####

# find all vent locations - straight lines

vent_locations_straight <- NULL

for (i in 1:nrow(endpoints)) {
  vents <- locate_vents(endpoints[i,], diagonals = FALSE)
  vent_locations_straight <- rbind(vents,vent_locations_straight)
}

# find total number of spots with > 1

count_danger_spots(vent_locations_straight)

# find all vent locations - with diagonal lines

vent_locations_diag <- NULL

for (i in 1:nrow(endpoints)) {
  vents <- locate_vents(endpoints[i,], diagonals = TRUE)
  vent_locations_diag <- rbind(vents,vent_locations_diag)
}

# find total number of spots with > 1

count_danger_spots(vent_locations_diag)


#### shits and giggles ####

library(viridis)
library(ggthemes)

vent_locations_diag %>%
  group_by(x,y) %>%
  summarise(count = n()) %>%
ggplot(aes(x,y, color = fct_rev(as.factor(count)))) +
  geom_point(size = .1, shape = 17) +
  theme_solarized_2() +
  scale_color_viridis_d(option = "inferno") +
  theme(legend.position = "none")
