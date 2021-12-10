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

line_result <- c(URLdecode(line_encoded),status)

lines_index <- rbind(lines_index, line_result)

}

# scoring

invalid_characters[invalid_characters == "%29"] <- 3
invalid_characters[invalid_characters == "%5D"] <- 57
invalid_characters[invalid_characters == "%7D"] <- 1197
invalid_characters[invalid_characters == "%3E"] <- 25137

sum(as.numeric(invalid_characters))



# part 2

