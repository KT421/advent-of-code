# Advent of Code 2021
# Dec 7

input <- str_split("16,1,2,0,4,2,7,1,2,14", ",") %>% unlist() %>% as.numeric()

input <- str_split(read_lines("input/dec7.txt"),",") %>% unlist() %>% as.numeric()

# find horizontal position match that uses least number of steps ( wanna bet that part 2 adds vertical?)

# Median is ideal location

ideal_pos <- median(input)

fuel <- sum(abs(input - ideal_pos))

# pt 2

# lost the bet. fuel cost changes with steps
# Mean is ideal location

fuel_cost <- function (steps) { steps*(steps+1)/2 }

movement <- abs(input - as.integer(mean(input)))
# R rounds to even. This works for my input but may be off-by-one for another input

fuel_usage <- map(movement, fuel_cost) %>% unlist() %>% sum()

