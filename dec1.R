# Advent of Code 2021
# Dec 1
# Puzzle #1

# Count number of readings that are greater than the previous reading

depths <- read_lines("input/dec1.txt")

depths <- as.integer(depths)

differences <- diff(depths, lag = 1)

length(differences[differences > 0])


# Puzzle #2

# create a three-measurement sliding window of sums, then find those that increase

window_depths <- depths + dplyr::lag(depths) + dplyr::lag(depths, n = 2)

window_depths <- na.omit(window_depths)

window_diffs <- diff(window_depths, lag = 1)

length(window_diffs[window_diffs > 0])
