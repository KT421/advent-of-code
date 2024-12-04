# Advent of Code 2024
# December 3

library(tidyverse)

input <- read_lines("2024/input/dec03.txt") %>% unlist() %>% paste0(collapse="")

# extract strings in the form of mul(xxx,xxx) and then multiply as written

regex_str <- "mul\\(\\d{1,3},\\d{1,3}\\)"

instructions <- str_extract_all(input, regex_str) %>% unlist()

mul <- function(a,b) {a*b}

read <- function(x) eval(parse(text = x))

result <- sapply(instructions,read)
sum(result)

# Pt 2

regex_str <- "mul\\(\\d{1,3},\\d{1,3}\\)|don't|do"
instructions <- str_extract_all(input, regex_str) %>% unlist()

instructions_trimmed <- instructions[str_starts(instructions,"mul")] %>%
  as.data.frame()
colnames(instructions_trimmed)<-c("instruction")

dodont <- NULL 
do <- "do"
for (i in instructions) {
  if (str_starts(i,"don't()")) {
    do <- "don't"
  } else if (str_starts(i, "do()")) {
    do <- "do"
  } else {
    dodont <- append(dodont,do)
  }
}

instructions_trimmed$dodont <- dodont 

instructions_trimmed <- instructions_trimmed %>%
  filter(dodont == "do")

result2 <- sapply(instructions_trimmed$instruction,read)
sum(result2)
