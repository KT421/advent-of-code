# Advent of Code 2024
# December 4

library(tidyverse)

input <- "MMMSXXMASMMSAMXMSMSAAMXSXMAAMMMSAMASMSMXXMASAMXAMMXXAMMXXAMASMSMSASXSSSAXAMASAAAMAMMMXMMMMMXMXAXMASX" %>% 
  str_split("") %>% unlist() %>% matrix(nrow = 10) 

input <- read_lines("2024/input/dec04.txt") %>%
  str_split("") %>% unlist() %>% matrix(nrow = 140) %>% t()

# XMAS can appear horizontal, vertical, diagonal, and backwards

horizontals <- apply(input, 1, paste0, collapse = "")
verticals <- apply(input, 2, paste0, collapse = "")

d1 <- split(input, row(input) - col(input))
d1 <- sapply(d1, paste0, collapse="")
d2 <- split(input, row(input) + col(input))
d2 <- sapply(d2, paste0, collapse="")

all_rows <- c(horizontals, verticals, d1, d2)
all_rows <- append(all_rows, stringi::stri_reverse(all_rows))

xmas <- str_extract_all(all_rows,"XMAS")

xmas %>% unlist %>% length

# Pt 2

# Dear Krampus. I did not deserve this

# all the cross MAS must have A's in the center, and then two S and to M at diags
a <- which(input == "A")

valid_xmas <- function(anchor) {
  # get the four corners around the anchor A
  values <- c(input[anchor-nrow(input)-1], 
              input[anchor-nrow(input)+1], 
              input[anchor+nrow(input)-1], 
              input[anchor+nrow(input)+1])
  
  all(sum(str_count(values, "M")) == 2, # 2 Ms
      sum(str_count(values, "S")) == 2, # 2 Ss
      values[2] != values[3], # Makes MAS not SAS or MAM
      anchor > nrow(input), # not at edges of matrix
      anchor < length(input)-nrow(input), 
      anchor %% nrow(input) !=0, 
      (anchor-1) %% nrow(input) != 0) 
}

sapply(a, valid_xmas) %>% sum
