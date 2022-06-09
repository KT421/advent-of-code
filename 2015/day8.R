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
quote = "")

# ok to do this

# Remove wrapping quotes, add 2 to code_char
# Remove all \\ and \", add 2 to code_char, add 1 to lit_char
# Remove all \x.., add 4 to code_char, add 1 to lit_char
# nchar(remainder) and add to lit_char

string <- input[[1]][300]

parse_lines <- function(string) {
# wrapping quotes
string <- str_replace(string,'^\"',"")
string <- str_replace(string,'\"$',"")

# stupid escape characters are stupid
string <- str_replace_all(string,"\\\\","_")

# slashies
slashies <- str_extract_all(string,"_[^x\\\"]")[[1]] %>% length
string <- str_replace_all(string,"_[^x\\\"]","")

# ascii
ascii <- str_extract_all(string,'_x..')[[1]] %>% length
string <- str_replace_all(string,'_x..',"")

# quotes
quotes <- str_extract_all(string,'_\"')[[1]] %>% length
string <- str_replace_all(string,'_\"',"")

# normal characters
chars <- nchar(string)

# lit characters
lit_chars <- chars + ascii + slashies + quotes

# code characters
code_chars <- chars + 2 + ascii*4 + quotes*2 + slashies*2

return(c(lit_chars,code_chars))

}

result <- map(input[,1],parse_lines)

result <- do.call(rbind.data.frame,result)

lit_chars <- sum(result[,1])

code_chars <- sum(result[,2])

code_chars - lit_chars

#1371

# pt 2
