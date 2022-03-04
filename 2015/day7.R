# aoc 2015
# day 7

library(tidyverse)

input <- read_lines("2015/input/day7.txt")

input <- read_lines("123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i")

# define operators

bitwise_operations <- c("AND" = bitwAnd, 
  "OR"= bitwOr,
  "LSHIFT" = bitwShiftL,
  "RSHIFT" = bitwShiftR,
  "NOT" = bitwNot,
  "->" = assign)

# extract data