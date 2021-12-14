# Advent of Code
# Dec 14

library(tidyverse)

starting_polymer <- read_lines("input/dec14.txt",n_max = 1)
polymer_chains <- read_lines("input/dec14.txt", skip = 2)

starting_polymer_example <- ("NNCB")
polymer_chains_example <- read_lines("CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C") 

# format polymer chains

chains <- polymer_chains %>%
  str_split(" -> ") %>%
  tibble() %>%
  unnest_wider(col = c(.)) %>%
  mutate(starts = str_split(`...1`,"")) %>%
  unnest_wider(starts, names_repair = "unique") %>%
  mutate(result1 = paste0(`...3`,`...2`),
         result2 = paste0(`...2`,`...4`)) %>%
  rename(start = `...1`) %>%
  select(c(start, result1, result2)) %>%
  pivot_longer(result1:result2) %>%
  select(-name) %>%
  rename(result = value)

# create initial pairlist

pairlist <- NULL

polymer <- str_split(starting_polymer,"") %>% unlist()

for (i in 1:(nchar(starting_polymer)-1)) {
  poly <- paste0(polymer[i],polymer[i+1],collapse = "")
  pairlist <- append(pairlist, poly)
}

pairlist <- tibble(start = pairlist, "value" = 1) %>%
  group_by(start) %>%
  summarise(value = sum(value))


# next start adding bits according to the directions

for (i in 1:40) {
  new_pairs <- pairlist %>%
    left_join(chains, by = c("start")) %>%
    group_by(result) %>%
    summarise(value = sum(value)) %>%
    rename(start = result)
  
  pairlist <- new_pairs
}

# calculate result

pairlist_result <- pairlist %>%
  mutate(start = str_split(start,"")) %>%
  unnest_longer(start) %>%
  group_by(start) %>%
  summarise(value = sum(value) / 2)

(max(pairlist_result$value) - min(pairlist_result$value)) %>% format(scientific = F)

  