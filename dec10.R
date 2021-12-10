# Advent of Code 2021
# Dec 10

input <- read_lines("input/dec10.txt") 

test_input <- read_lines("[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

# find CORRUPTED lines
# meaning a chunk closes with an incorrect bracket
# there are also INCOMPLETE lines, where a chunk does not close - leave those
# A line can be corrupted and incomplete

# characters selected for maximum evil and I don't want to escape all that shit so let's re-encode them

square_brackets <- "%5B%5D"
parens <- "%28%29"
curly_brackets <- "%7B%7D"
carets <- "%3C%3E"

close_characters <- c("%5D|%29|%7D|%3E")

lines_index <- tibble()
invalid_characters <- NULL

for (line in input) {
  
line_encoded <- URLencode(line, reserved = TRUE)

# remove closed bracket pairs and continue as long as it keeps getting shorter
while (TRUE) {

  line_shortened <- line_encoded

  line_shortened <- str_remove(line_shortened,square_brackets)
  line_shortened <- str_remove(line_shortened,parens)
  line_shortened <- str_remove(line_shortened,curly_brackets)
  line_shortened <- str_remove(line_shortened,carets)

  if (nchar(line_encoded) == nchar(line_shortened)) break

  line_encoded <- line_shortened
}

status <- NULL

if (nchar(line_encoded) == 0) {
  status <- "complete"
} else if (sum(str_detect(line_encoded,close_characters)) == 0) {
  status <- "incomplete"
} else {
  status <- "corrupted"
  invalid_char <- str_extract(line_encoded,close_characters)
  invalid_characters <- append(invalid_characters,invalid_char)
}

line_result <- c(line_encoded,status)

lines_index <- rbind(lines_index, line_result)

}

# scoring

invalid_characters[invalid_characters == "%29"] <- 3
invalid_characters[invalid_characters == "%5D"] <- 57
invalid_characters[invalid_characters == "%7D"] <- 1197
invalid_characters[invalid_characters == "%3E"] <- 25137

sum(as.numeric(invalid_characters))



# part 2

#filter out corrupted, finish incomplete lines

colnames(lines_index) <- c("line","status")

lines_index <- lines_index %>%
  filter(status != "corrupted")



# replace each value with its match

open_to_close <- function(line) {
  line <- str_replace_all(line, "%5B","%5D")
  line <- str_replace_all(line, "%28","%29")
  line <- str_replace_all(line, "%7B","%7D")
  line <- str_replace_all(line, "%3C","%3E")
}

lines_index$closing <- open_to_close(lines_index$line)


# reverse the order
lines_index$closing <- sapply(lines_index$closing,URLdecode)
lines_index$closing <- stringi::stri_reverse(lines_index$closing)

# scoring

score_line <- function(line) {
  score <- 0

  line <- str_split(line, "") %>% unlist()
  
  line[line == ")"] <- 1
  line[line == "]"] <- 2
  line[line == "}"] <- 3
  line[line == ">"] <- 4
  
  line <- as.numeric(line)
  
  while (TRUE) {
    score <- (score * 5) + line[1]
    line <- line[-1]
    if (length(line) == 0) break
  }
  score
}

lines_index$score <- sapply(lines_index$closing, score_line)


score_line(lines_index$closing[1])
