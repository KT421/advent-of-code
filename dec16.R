# Advent of Code 2021
# Dec 16

library(tidyverse)

input <- read_lines("input/dec16.txt") %>% str_split("") %>% unlist()

input_example1 <- "D2FE28" %>% str_split("") %>% unlist()

input_example2 <- "38006F45291200" %>% str_split("") %>% unlist()

# bits to bytes lookup table

# extract packet version - first 3

# extract packet tyoe id - second 3
# 4 is a literal, others are operators and will include subpackets
# operators will then include a length value showing the length of the subpackets


# hex lookup table

hex_lookup_table <- tibble(hex = c("0","1","2","3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"),
                           binary = c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"))

hex_to_bin <- function(packet_hex) {
  packet_hex <- tibble(hex = packet_hex)
  
  packet_hex <- packet_hex %>% left_join(hex_lookup_table)
  
  paste0(packet_hex$binary, collapse = "")
}

literal_to_decimal <- function(binary_literal) {
  value <- str_extract_all(binary_literal, pattern = "\\d{5}")
  value <- lapply(value, substring, first = 2, last = 5)
  value <- value %>% unlist() %>% paste0(collapse = "")
  value <- strtoi(value, base = 2L)
  value
}

# parse packets out

parse_packet <- function(packet_hex) {
  
  packet_bin <- hex_to_bin(packet_hex)
  
  value <- NULL
  subpackets_length <- NULL
  subpackets_num <- NULL
  subpackets <- NULL
  version <- substring(packet_bin, 1, 3) %>% strtoi(2L)
  type <- substring(packet_bin, 4, 6) %>% strtoi(2L)
  contents <- substring(packet_bin, 7)
  
  if (type == 4L) {
    value <- literal_to_decimal(contents)
    
  } else {
    length_type <- substring(contents, 1, 1)
    if (length_type == 0) {
      subpackets_length <- substring(contents, 2, 16) %>%
        strtoi(base = 2L)
      subpackets <- parse_packet(substring(contents, 17, 16 + subpackets_length))
    } else {
      subpacket_num <- substring(contents, 2, 13) %>%
        strtoi(base = 2L) 
      subpackets <- parse_packet(substring(contents, first = 14))
    }
    
    
  }
  list("packet_bin" = packet_bin, 
     "version" = version, 
     "type" = type, 
     "contents" = contents, 
     "value" = value, 
     'subpackets_length' = subpackets_length, 
     'subpackets_num' = subpackets_num, 
     'subpackets' = subpackets)
}

parse_packet(input_example2)
