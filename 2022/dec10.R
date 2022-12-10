# AOC 2022
# Dec 10

# WIP

library(tidyverse)

input <- readLines("2022/input/dec10.txt") 

X <- 1

cycle <- 1

tracker <- tibble(cycle = 1, X = 1)

# pt 1

for (i in input) {
  
  if (str_detect(i,"noop")) {
    cycle <- cycle + 1

  } else {
    value <- str_extract(i,"[:punct:]*[:digit:]+") %>% as.numeric()
    # bind in the processing cycle
    tracker <- rbind(tracker, tibble(cycle = cycle+1,X = X))
    X <- X + value
    cycle <- cycle + 2

  }
  
  tracker <- rbind(tracker, tibble(cycle = cycle,X = X))
  
}

tracker %>%
  head(240) %>%
  mutate(signal_strength = cycle * X) %>%
  filter(cycle %in% c(20,60,100,140,180,220)) %>%
  pull(signal_strength) %>%
  sum()

# pt 2

crt <- tracker %>%
  rename(sprite_center = X) %>%
  mutate(cycle = cycle - 1,
    Y_pos = case_when(
    cycle >= 201 ~ 6,
    cycle >= 161 ~ 5,
    cycle >= 121 ~ 4,
    cycle >=81 ~ 3,
    cycle > 40 ~ 2,
    TRUE ~ 1),
  X_pos = case_when(
    cycle >= 201 ~ cycle - 200,
    cycle >= 161 ~ cycle - 160,
    cycle >= 121 ~ cycle - 120,
    cycle >=81 ~ cycle - 80,
    cycle > 40 ~ cycle - 40,
    TRUE ~ cycle),
  lit = ifelse(abs(X_pos - sprite_center) <= 1,"#"," ")) %>%
  tail(240)

crt_display <- matrix(crt$lit,nrow = 6, ncol=40, byrow=T)
