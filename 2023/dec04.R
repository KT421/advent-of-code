# Advent of Code 2023
# Day 04

input <- read_lines('Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11')

input <- read_lines('2023/input/dec04.txt') 

# PT 1

cards <- input %>%
  tibble() %>%
  separate_wider_delim(cols = everything(), delim = "|", names = c("play","winning")) %>%
  mutate(play = str_remove_all(play, "Card[:space:]*[:digit:]{1,3}: "),
         card_num = row_number(),
         play = str_extract_all(play, '[:digit:]{1,2}'),
         winning = str_extract_all(winning, '[:digit:]{1,2}')) %>% 
  rowwise() %>%
  mutate(num_win = sum(unlist(play) %in% unlist(winning))) %>%
  mutate(score = case_when(num_win == 0 ~ 0,
                           num_win == 1 ~ 1,
                           num_win > 1 ~ 2^(num_win-1)))

sum(cards$score)

# PT2

# recursive win more scratchcards
# for each win, instead of getting points, get new cards 
# evaluate cards until no new cards are found
# count totals of cards

cards$copies <- 1

draw_cards <- function(card_num, num_win) {
  if(num_win > 0) as.vector(seq(card_num+1, card_num+num_win))
}

current_hand <- cards

new_hand <- data.frame(1,1)


while(nrow(current_hand) > 0) {
  current_hand <- current_hand %>%
    mutate(new_cards = list(sapply(card_num, draw_cards, num_win = num_win))) 
  
  new_hand_all <- unlist(current_hand$new_cards) 
  new_hand <- new_hand_all %>% table() %>% as.data.frame()
  colnames(new_hand) <- c("card_num", "new_copies") 
  
  # exlcude cards off the table
  new_hand <- new_hand %>%
    mutate(card_num = as.numeric(as.character(card_num))) %>%
    filter(card_num < 205)
  
  # add cards to total copies
  cards <- left_join(cards, new_hand, by = "card_num") %>%
    mutate(new_copies = replace_na(new_copies, 0)) %>%
    mutate(copies = copies + new_copies) %>%
    select(-new_copies)
  
  current_hand <- cards[new_hand_all,]
}

# so it errors out but it has the right value when it's done?

sum(cards$copies)
