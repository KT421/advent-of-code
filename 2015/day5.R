# aoc 2015
# day 5

#regex day

library(tidyverse)

input <- read_lines("2015/input/day5.txt")

nice_regex <- c("[aieou].*[aieou].*[aieou]",
                "(.)\\1",
                "ab|cd|pq|xy")

nice_pattern <- c(T,T,F)

detect_nice <- function(string) {
  if (all(str_detect(string,nice_regex) == nice_pattern)) {
      return("nice")
    } else {
      return("naughty")
    }
}

sapply(input,detect_nice) %>% table()

# pt 2
# it's capture group time

nice_regex <- c("(.{2}).*\\1",
                 "(.).\\1")

nice_pattern <- c(T,T)

sapply(input,detect_nice) %>% table()

