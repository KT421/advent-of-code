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
  lags <- na.omit(r - lead(r))
  ((all(lags > 0 ) & all(lags < 4)) | (all(lags < 0 ) & all(lags > -4)))
}

sapply(input, is_safe) %>% sum

# Pt 2

# can remove up to one "bad" level per report

is_safe2 <- function(report) {
  if(is_safe(report)) { 
    T
    } else {
      dampener <- NULL
      for (i in 1:length(report)) dampener <- append(dampener,is_safe(report[-i]))
      any(dampener)
  }
}

sapply(input, is_safe2) %>% sum
