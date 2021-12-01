#Advent of Code 2021
#Puzzle #1

# Count number of readings that are greater than the previous reading

depths <- read_lines("input/dec1a.txt")

depths <- as.integer(depths)

differences <- diff(depths, lag = 1)

length(differences[differences > 0])
