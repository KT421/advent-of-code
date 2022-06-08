# aoc 2015
# day 7

library(tidyverse)

input <- read_lines("2015/input/day7.txt")

# define operators

bitwise_operations <- c("AND" = bitwAnd, 
  "OR"= bitwOr,
  "LSHIFT" = bitwShiftL,
  "RSHIFT" = bitwShiftR,
  "NOT" = bitwNot)

# rename problematic variables - when I `eval` these it tries to actually use them as if, in, etc

input <- gsub("as", "asa", input)
input <- gsub("if", "ifa", input)
input <- gsub("in", "ina", input)

evaluate_line <- function(line) {
  # separate into chunks
  chunks <- str_split(line, " ") %>% unlist()
  
  result <- tail(chunks,1)
  
  if (length(chunks) == 3) {
    # inputs
    chunks[1] <- eval(parse(text = chunks[1]))
    assign(result,as.integer(chunks[1]),envir = .GlobalEnv)
  } else if (length(chunks) == 4) {
    # NOT
    assign(result,bitwNot(eval(parse(text = chunks[2]))),envir = .GlobalEnv)
  } else  {
    # all other operators
      operator <- bitwise_operations[names(bitwise_operations) %in% chunks][[1]]
      operand1 <- eval(parse(text = chunks[1]))
      operand2 <- eval(parse(text = chunks[3]))
      assign(result,operator(operand1,operand2),envir = .GlobalEnv)
  }
}

calculated_value <- function(operand) {
  ifelse(exists(operand) && is.integer(eval(parse(text = operand))),TRUE,FALSE)
}

can_evaluate <- function(line) {
  chunks_test <- str_split(line, " ") %>% unlist()
  chunks_test <- chunks_test[str_detect(chunks_test,"[:lower:]")]
  test_result <- tail(chunks_test,1)
  test_operands <- head(chunks_test,-1)
  
  if (calculated_value(test_result)) {
    return(FALSE)
  } else if (all(unlist(map(test_operands,calculated_value)))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

repeat {
  for (this_line in input) {
    if (!can_evaluate(this_line)) next
    evaluate_line(this_line)
    
    print(this_line)
    
    if (exists("a")) break
    next
  }
  if (exists("a")) break
}

print(a)

# pt2

b <- a

# clear environment, except for b, the input, and my functions
rm(list=setdiff(ls(), c("b", "input", "can_evaluate","calculated_value","evaluate_line", "bitwise_operations")))

repeat {
  for (this_line in input) {
    if (!can_evaluate(this_line)) next
    evaluate_line(this_line)
    
    print(this_line)
    
    if (exists("a")) break
    next
  }
  if (exists("a")) break
}

print(a)
