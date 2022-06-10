# aoc 2015
# day 8

# escaping characters with santa

# \\ -> \
# \" -> "
# \xAB -> ASCII hex code AB

# find total num of literals AND total num of characters in memory, find difference

library(tidyverse)

input <- read.table('2015/input/day8.txt',
sep = "\n",
quote = "") %>% unlist()

code_chars <- nchar(input)
lit_chars <- map(input,~nchar(eval(parse(text = .x)),type = "bytes")) %>% unlist() %>% sum()

code_chars - lit_chars

# pt 2

new_code_chars <- stringi::stri_escape_unicode(input) %>% nchar() + 2

sum(new_code_chars) - code_chars
