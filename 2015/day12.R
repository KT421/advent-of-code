# aoc 2015
# day 12

library(tidyverse)

input <- jsonlite::fromJSON(readLines("2015/input/day12.txt"))

#extract and add all numbers

extract_numbers <- input %>%
  unlist() %>%
  as.numeric() %>%
  sum(na.rm = T) %>%
  print()

## PT 2

# exclude all {objects} with "red" and all children, then sum
# objects are named, eg {"a":"red"}
# object names are always a single letter

# doesn't work yet




lapply(input[["e"]][["a"]][["b"]][["d"]], test_if_red)

lapply(input[["e"]][["a"]][["b"]][["d"]], is.list)

lapply(input[["e"]][["a"]][["b"]][["d"]], is.named)

test_input <- '[1,{"c":"red","b":2},3]' %>% jsonlite::fromJSON()

