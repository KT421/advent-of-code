# Advent of Code 2024
# December 2

library(tidyverse)

input <- read_lines("2024/input/dec02.txt") %>%
  str_split(" ") 

# all levels increasing
# all levels decreasing
# difference between each level is between 1 and 3
is_safe <- function(report) {
  r <- as.numeric(report)
  lags <- r - lead(r)
  lags <- na.omit(lags)
  
  ifelse((all(lags > 0 ) & all(lags < 4)) | (all(lags < 0 ) & all(lags > -4)), TRUE, FALSE)
}

safe <- sapply(input, is_safe)
sum(safe)

# Pt 2

# can remove up to one "bad" level per report

is_safe2 <- function(report) {
  if(is_safe(report)) 
    { return(TRUE) } else {
    dampener <- NULL
    for (i in 1:length(report)) {
      dampener <- append(dampener,is_safe(report[-i]))
    }
    return(any(dampener))
  }
}

safe2 <- sapply(input, is_safe2)
sum(safe2)
