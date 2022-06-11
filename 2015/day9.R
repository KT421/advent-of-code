# aoc 2015
# day 9

# traveling salesman day

library(tidyverse)

input <- read_lines("2015/input/day9.txt")

citypairs <- input %>%
  str_split(pattern=c(" to | = ")) 

citypairs <- do.call(rbind.data.frame,citypairs)

colnames(citypairs) <- c("a","b","distance")

cities <- unique(c(citypairs$a,citypairs$b))

all_paths <- gtools::permutations(8,8,v=cities,set=TRUE,repeats.allowed = FALSE) %>%
  as.data.frame() 

colnames(all_paths) <- c("a","b","c","d","e","f","g","h")

find_distance <- function(citya,cityb) {
  path_distance <- citypairs %>%
    rowwise() %>%
    filter((any(citya %in% a, citya %in% b) && any(cityb %in% a, cityb %in% b)))
  
  path_distance$distance %>% as.integer()
}

find_total_distances <- function(path) {
  
  total_dist <- 0

  for (i in 1:7) {
    this_dist <- find_distance(path[i],path[i+1])
    total_dist <- total_dist + this_dist
  }
  
  total_dist  
}

result <- all_paths %>%
  rowwise() %>%
  mutate(dist = find_total_distances(c(a,b,c,d,e,f,g,h)))

min(result$dist)

# pt2

max(result$dist)

# it works but it's not what one would call optimized....