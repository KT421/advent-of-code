#aoc 2015
# day 6

library(tidyverse)

input <- read_lines("2015/input/day6.txt")

# need to detect from string start x, start y, end x, end y, and action
# if only the prior day's puzzle had been a refresher on regex capture groups...

instruction_pattern <- "(off|on|toggle) (\\d{1,3}),(\\d{1,3}) through (\\d{1,3}),(\\d{1,3})"

instructions <- str_match_all(input,instruction_pattern)

format_instructions <- function(instruction) {
  instruction<-as.vector(instruction)[2:6]
  
  names(instruction) <- c("action","x1","x2","y1","y2")
  
  instruction[2:5] <- as.numeric(instruction[2:5]) + 1

  return(instruction)
  }

instructions <- map_dfr(instructions,format_instructions) %>%
  mutate(across(.cols = c(x1,x2,y1,y2),as.numeric))

lights <- matrix(data = 0, nrow = 1000, ncol = 1000)

for (i in 1:nrow(instructions)) {
  i <- instructions[i,]
  
  if (i$action == "on") {
    lights[i$x1:i$y1,i$x2:i$y2] <- T
  } else if (i$action == "off") {
    lights[i$x1:i$y1,i$x2:i$y2] <- F
  } else if (i$action == "toggle") {
    lights[i$x1:i$y1,i$x2:i$y2] <- !lights[i$x1:i$y1,i$x2:i$y2]
  }
}

sum(lights)

# pt 2
# now with ints instead of bools

lights <- matrix(data = 0, nrow = 1000, ncol = 1000)

for (i in 1:nrow(instructions)) {
  i <- instructions[i,]
  
  if (i$action == "on") {
    lights[i$x1:i$y1,i$x2:i$y2] <- lights[i$x1:i$y1,i$x2:i$y2] +1
  } else if (i$action == "off") {
    lights[i$x1:i$y1,i$x2:i$y2] <- pmax(lights[i$x1:i$y1,i$x2:i$y2] - 1,0)
    
  } else if (i$action == "toggle") {
    lights[i$x1:i$y1,i$x2:i$y2] <- lights[i$x1:i$y1,i$x2:i$y2] + 2
  }
}

sum(lights)
