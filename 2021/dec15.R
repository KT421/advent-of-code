# Advent of Code 2021
# Dec 15

library(tidyverse)
library(igraph)

input <- read_lines("2021/input/dec15.txt") %>% strsplit("") %>% unlist() %>% as.numeric()

input_example <- read_lines("1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581") %>% strsplit("") %>% unlist() %>% as.numeric()


cave_network <- make_lattice(dimvector = c(100,100), directed = T, mutual = T)

# assign risk levels to vertices

V(cave_network)$risk <- input

# edges inherit risk from vertices

edgelist <- get.edgelist(cave_network)
E(cave_network)$weight <- V(cave_network)[edgelist[,2]]$risk

distances(cave_network, v = 1, to = 10000, mode = "out")


# pt 2

bigger_caves <- input %>% matrix(ncol = 100)

bigger_caves <- cbind(bigger_caves, 
                      bigger_caves + 1, 
                      bigger_caves + 2, 
                      bigger_caves + 3, 
                      bigger_caves + 4)

bigger_caves <- rbind(bigger_caves,
                      bigger_caves + 1,
                      bigger_caves + 2,
                      bigger_caves + 3,
                      bigger_caves + 4)

bigger_caves[bigger_caves > 9] <- bigger_caves[bigger_caves > 9] - 9

cave_network <- make_lattice(dimvector = c(500,500), directed = T, mutual = T)

# assign risk levels

V(cave_network)$risk <- bigger_caves

edgelist <- get.edgelist(cave_network)
E(cave_network)$weight <- V(cave_network)[edgelist[,2]]$risk

distances(cave_network, v = 1, to = 250000, mode = "out")
