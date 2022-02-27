# Advent of Code 2021
# Dec 8

library(tidyverse)

input <- read_lines("2021/input/dec8.txt") %>% str_split(" \\| | ") %>% tibble() %>% unnest_wider(col = c(.))

test_input <- "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf" %>% str_split(" \\| | ") %>% tibble() %>% unnest_wider(col = c(.))

input_signals <- input[,1:10]                                                     

output_signals <- input[,11:14]                                                     


# part 1

# find sum occurrences of 1, 4, 7, 8 in only the output
# part 2 is shaping up to be a doozy but I lack data at this point to write properly generalizable code
# so whack out pt 1 in non-generalizable fashion to get to pt 2 quickly

sum(apply(output_signals,c(1,2),nchar) %in% c(2,3,4,7))


# part 2 

# sum all output values

# as I thought, decode the whole damn thing. But there's additional guidance on how it's put together

compare_signals <- function(sig1, sig2) {
  sig1 <- str_split(sig1, "")[[1]]
  sum(sig1 %in% sig2)
}


decode_signal <- function(signal) {
  signal <- t(signal) %>%
    tibble() %>%
    mutate(decoded = NA)
  
  colnames(signal) <- c("encoded", "decoded")
  
  # easy decodes

  signal <- signal %>%
    mutate(decoded = case_when(
      nchar(encoded) == 2 ~ 1,
      nchar(encoded) == 3 ~ 7,
      nchar(encoded) == 4 ~ 4,
      nchar(encoded) == 7 ~ 8))
  
  one <- str_split(signal$encoded[signal$decoded == 1 & !is.na(signal$decoded)][1],"")[[1]]
  seven <- str_split(signal$encoded[signal$decoded == 7 & !is.na(signal$decoded)][1],"")[[1]]
  four <- str_split(signal$encoded[signal$decoded == 4 & !is.na(signal$decoded)][1],"")[[1]]
  eight <- str_split(signal$encoded[signal$decoded == 8 & !is.na(signal$decoded)][1],"")[[1]]
  
# I worked a lot to make case_when work and it's not working. Using ifelse statements because I'm frustrated
  
  for (i in 1:length(signal$encoded)) {
    if (nchar(signal$encoded[i]) == 5) {
    if (compare_signals(signal$encoded[i],one) == 2) {signal$decoded[i] <- 3}
      else if (compare_signals(signal$encoded[i],four) == 3) {signal$decoded[i] <- 5}
      else {signal$decoded[i] <- 2}
    }
    if (nchar(signal$encoded[i]) == 6) {
      if (compare_signals(signal$encoded[i],one) == 1) {signal$decoded[i] <- 6}
      else if (compare_signals(signal$encoded[i],four) == 4) {signal$decoded[i] <- 9}
      else {signal$decoded[i] <- 0}
    }
}
  
  signal$decoded
  
}


decode_signal(test_input)

output <- NULL

for (i in 1:nrow(input)) {
  this_output <- decode_signal(input[i,])
  this_output <- as.numeric(paste(as.character(this_output[11:14]), collapse = ""))
  output <- rbind(output,this_output)
}
  

# sum all output values

sum(output)
