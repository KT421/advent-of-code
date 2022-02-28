# aoc 2015
# day 4

input <- "yzbqklnj"

# part 1
# append integer to input, find lowest that returns an MD5 hash with
# five leading zeroes

which(substr(sapply(paste0(input,1:500000),openssl::md5), 1, 5) == "00000")


# part 2
# now 6 leading zeroes

which(substr(sapply(paste0(input,9000000:10000000),openssl::md5), 1, 6) == "000000")
# chunked into 1 million numbers at a time because apparently I am unlucky