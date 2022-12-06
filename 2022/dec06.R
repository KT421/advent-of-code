# AOC 2022
# Day 06

input <- readLines("2022/input/dec06.txt") %>% strsplit("") %>% unlist()

# PT1

parsed <- data.frame(string = paste0(input,lag(input,1),lag(input,2),lag(input,3)))

parsed$is_marker <- ifelse(str_detect(parsed$string,"(.).*\\1"),F,T)

# manually mark first 3 rows as false
parsed$is_marker[1:3] <- F

which(parsed$is_marker == T)[1]

# PT2

# there's probably a way to vectorize this but I don't feel like it right now
parsed_message <- data.frame(string = paste0(input,
                                             lag(input,1),
                                             lag(input,2),
                                             lag(input,3),
                                             lag(input,4),
                                             lag(input,5),
                                             lag(input,6),
                                             lag(input,7),
                                             lag(input,8),
                                             lag(input,9),
                                             lag(input,10),
                                             lag(input,11),
                                             lag(input,12),
                                             lag(input,13)))

parsed_message$is_marker <- ifelse(str_detect(parsed_message$string,"(.).*\\1"),F,T)

# manually mark first 13 rows as false
parsed_message$is_marker[1:13] <- F

which(parsed_message$is_marker == T)[1]
