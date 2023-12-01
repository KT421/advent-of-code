# Advent of Code 2023
# December 1

library(tidyverse)

test_input <- read_lines('1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet')

input <- read_lines("2023/input/dec01.txt") 

#PT 1

input %>%
  str_extract_all('[:digit:]') %>%
  lapply(FUN = function(x) {paste0(head(x,1), tail(x,1))}) %>%
  as.numeric() %>%
  sum()

#PT 2

test_input <- read_lines('two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen')

# numbers that share a letter are still valid, e.g. eighthree should be eight AND three
# which was NOT in the test input
# trying to regex lookahead AND lookbehind is driving me nuts so I am going to cheese it

numbers <- c('one' = 1,
             'two' = 2,
             'three' = 3,
             'four' = 4, 
             'five' = 5,
             'six' = 6,
             'seven' = 7,
             'eight' = 8, 
             'nine' = 9)

replacements <- c('twone' = 'twoone',
  'oneight' = 'oneeight',
  'threeight' = 'threeeight',
  'fiveight' = 'fiveeight',
  'sevenine' = 'sevennine',
  'eightwo' = 'eighttwo',
  'eighthree' = 'eightthree')

regex_pattern <- "([:digit:]|one|two|three|four|five|six|seven|eight|nine)"

input %>%
  lapply(str_replace_all, replacements, names(replacements)) %>%
  str_extract_all(regex_pattern) %>%
  lapply(stringi::stri_replace_all_fixed, names(numbers), numbers, vectorize = F) %>%
  lapply(FUN = function(x) {paste0(head(x,1), tail(x,1))}) %>%
  as.numeric() %>%
  sum()
