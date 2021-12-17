# Advent of Code 2021
# dec 17

library(tidyverse)

#input_example <- "target area: x=20..30, y=-10..-5" %>%
#  str_extract("\\w=\\w{+}")

## do string extraction later

target_x <- c(88,125)
target_y <- c(-157,-103)

# test input
target_x <- c(20, 30)
target_y <- c(-10,-5)

# pt 1
# let's brute force it. What could possibly go wrong?

step_probe <- function(probe) {

  probe$position$x <- probe$position$x + probe$velocity$x
  
  probe$position$y <- probe$position$y + probe$velocity$y
  
  if (probe$velocity$x > 0) {
    probe$velocity$x <- probe$velocity$x - 1
  } 
  
  if (probe$velocity$x < 0) {
    probe$velocity$x <- probe$velocity$x + 1 
  } 
  
  probe$velocity$y <- probe$velocity$y - 1

  if (probe$position$y > probe$max_y) probe$max_y <- probe$position$y
  
  probe
}

fire_probe <- function(vel_x, vel_y) {
  
  position <- list(x = 0, y = 0)
  velocity <- list(x = vel_x, y = vel_y)
  probe <- list("position" = position, "velocity" = velocity, max_y = 0)
  hit <- 
  repeat {
    
  probe <- step_probe(probe)
  
  if (probe$position$y < target_y[1]) {
    print(paste("MISS!",vel_x, vel_y))
    break
  } 
  
  if (probe$position$x >= target_x[1] &&
      probe$position$x <= target_x[2] &&
      probe$position$y >= target_y[1] &&
      probe$position$y <= target_y[2]) {
  
    print(paste("HIT!",vel_x, vel_y))
    
    max_y <- probe$max_y
    
    hit <- tibble(vel_x, vel_y, max_y)
    
    return(hit)
    break  
  }
  
  }
  
}

shots <- tibble()


for (x in 0:125) {
  for (y in -157:157) {
    shots <- bind_rows(shots,fire_probe(x,y))
  }
}

# HA! PT2 solved as a side effect of the way I did pt 1
# BRUTE FORCE WINS!

shots %>%
  nrow()

