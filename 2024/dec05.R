# Advent of Code 2024
# December 5

library(tidyverse)

input <- read_lines("2024/input/dec05.txt")

page_orders <- input[1:1176]
page_lists <- input[1178:1374]

# Page Orders: a must print before b
# Page Lists: Pages needed in each update

# Find which list are in correct order, extract middle value, sum

# prep the reference Page Orders
page_orders_prepped <- tibble(orders = page_orders) %>%
  separate_wider_delim(orders, "|", names = c("start","end")) %>%
  mutate(across(everything(), as.numeric))

order_test <- function(page_list,test_value) {
  if(test_value %in% page_list) return(which(page_list %in% test_value)) else return(NA)
}

# function to test validity of an entire page list
validity_test <- function(page_list) {
  page_list <- str_split(page_list, ",") %>% unlist() %>% as.numeric()

page_orders_test <- page_orders_prepped %>%
  rowwise() %>%
  mutate(start_test = order_test(page_list,start),
         end_test = order_test(page_list,end),
         validity = end_test > start_test)

all(page_orders_test$validity, na.rm = T)
}

results <- sapply(page_lists, validity_test)

# make a dataframe, extract middle values, sum
results_df <- tibble(page_lists = page_lists, results = results) 

results_df %>%
  filter(results) %>%
  mutate(middle_value = as.numeric(
                        str_sub(page_lists, 
                                start = floor(nchar(page_lists)/2), 
                                end = floor(nchar(page_lists)/2)+1))) %>% 
  pull(middle_value) %>% sum

# Pt 2
# fix the invalid lists

incorrect_lists <- results_df %>%
  filter(!results) %>% pull(page_lists)

# function to fix the invalid page lists
validity_fix <- function(page_list) {
  page_list <- str_split(page_list, ",") %>% unlist() %>% as.numeric()
  
  this_page_orders <- page_orders_prepped %>%
    filter(start %in% page_list & end %in% page_list)
  
  page_orders_errors <- this_page_orders %>%
    rowwise() %>%
    mutate(start_test = order_test(page_list,start),
           end_test = order_test(page_list,end),
           validity = end_test > start_test) %>%
    filter(!is.na(validity),
           !validity)

  # for the first failed test, swap the two values that failed. Then test again
  # and keep going until no tests are failed
  while(T) {
    invalid_a <- page_orders_errors$start[[1]]
    invalid_a_loc <- which(page_list %in% invalid_a)
    invalid_b <- page_orders_errors$end[[1]]
    invalid_b_loc <- which(page_list %in% invalid_b)
    
    page_list[invalid_a_loc] <- invalid_b
    page_list[invalid_b_loc] <- invalid_a
    
    # retest
    page_orders_errors <- this_page_orders %>%
      rowwise() %>%
      mutate(start_test = order_test(page_list,start),
             end_test = order_test(page_list,end),
             validity = end_test > start_test)  %>%
      filter(!is.na(validity),
             !validity)
    
    if (nrow(page_orders_errors) == 0) {
      return(page_list)
      break
      }
  }
}

# apply the fixing function to every incorrect list
corrected_lists <- lapply(incorrect_lists,validity_fix)

# extract the middle values and sum
tibble(corrected = corrected_lists) %>%
  rowwise() %>%
  mutate(middle = corrected[(length(corrected)/2)+1]) %>% 
  pull(middle) %>% sum()
