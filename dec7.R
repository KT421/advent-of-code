# Advent of Code 2021
# Dec 7

input <- str_split("16,1,2,0,4,2,7,1,2,14", ",") %>% unlist() %>% as.numeric()

input <- str_split(read_lines("input/dec7.txt"),",") %>% unlist() %>% as.numeric()

# find horizontal position match that uses least number of steps ( wanna bet that part 2 adds vertical?)

# is the median the ideal location? It is for the test set

ideal_pos <- median(input)

fuel <- sum(abs(input - ideal_pos))

# pt 2

# lost the bet. fuel cost changes with steps
# fuel cost increases with step
# new ideal position is mean? 

fuel_cost <- function (steps) { steps*(steps+1)/2 }

movement <- abs(input - as.integer(mean(input)))

fuel_usage <- map(movement, fuel_cost) %>% unlist() %>% sum()

