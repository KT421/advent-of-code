# Advent of Code 2021
# Dec 12

library(tidyverse)

input <- read_lines("2021/input/dec12.txt") %>% strsplit("-")   

input <- read_lines("start-A
start-b
A-c
A-b
b-d
A-end
b-end") %>% strsplit("-") 

input <- input <- read_lines("dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc") %>% strsplit("-") 

input <- read_lines("fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW") %>% strsplit("-") 

# part 1
# find the total number of paths from start to end that visit small caves max of one time per route
# can visit multiple small caves but each ONLY once
# uppercase <- big cave
# lowercase <- small cave


# list of small caves
small_caves <- unlist(input) %>% str_extract_all("^[[:lower:]]+") %>% compact() %>% unique() %>% unlist()

# find next nodes

find_connecting_nodes <- function(path) {

  node <- path[length(path)]
  small_visited <- path[path %in% small_caves]
    
  connecting_nodes <- NULL
  for (i in 1:length(input)) {
    if (input[[i]][1] == node) connecting_nodes <- append(connecting_nodes, input[[i]][2])
    if (input[[i]][2] == node) connecting_nodes <- append(connecting_nodes, input[[i]][1])
    connecting_nodes <- connecting_nodes[!connecting_nodes %in% small_visited]
  }
  connecting_nodes
  }

paths <- list("start")

repeat {
  for (i in 1:length(paths)) {
    
    path <- paths[[i]]
    if (tail(path,1) %in% c("end")) next
    
    next_nodes <- find_connecting_nodes(path)
    
    if (length(next_nodes) == 0) {
      paths <- paths[-i]
      break
    }
    
    for (node in next_nodes) {
      new_path <- list(append(path,node))
      paths <- append(paths, new_path)
    }
    
    # remove the processed path from the list
    paths <- paths[-i]
  }
  
  # if they've all found the end, break
  if (all(sapply(paths,tail,n = 1) %in% c("end"))) break
}

length(paths)

###########################

# part 2 - now may visit ONE small cave twice

small_caves <- small_caves[!small_caves == "start"]

# find next nodes

find_connecting_nodes <- function(path) {
  
  node <- path[length(path)]
  small_visited <- path[path %in% small_caves]
  small_visited_twice <- table(path[path %in% small_caves])
  small_visited_twice <- names(small_visited_twice[small_visited_twice > 1])
  
  connecting_nodes <- NULL
  for (i in 1:length(input)) {
    if (input[[i]][1] == node) connecting_nodes <- append(connecting_nodes, input[[i]][2])
    if (input[[i]][2] == node) connecting_nodes <- append(connecting_nodes, input[[i]][1])
    connecting_nodes <- connecting_nodes[!connecting_nodes %in% "start"]
    if (length(small_visited_twice) == 1) {
      connecting_nodes <- connecting_nodes[!connecting_nodes %in% small_visited]
    } 
  }
  connecting_nodes
}

# expanding the list took approximately 15 million years, so this just keeps track of the 
# NUMBER of valid paths and discards the paths themselves once complete

# it STILL takes 15 minutes to run so if you're going to copypaste this solution then maybe find another one somewhere

paths <- list("start")

valid_paths <- 0

repeat {

    path <- paths[[1]]
    paths <- paths[-1]

    next_nodes <- find_connecting_nodes(path)
    
    if (length(next_nodes) == 0) {
      next
    }
    
    for (node in next_nodes) {
      new_path <- list(append(path,node))
      if (tail(new_path[[1]],1) == "end") {
        valid_paths <- valid_paths + 1

      } else {
        paths <- append(paths, new_path)
      }
      
    }

    if (valid_paths %% 10000 == 0) print(paste(Sys.time(), valid_paths))
    if (length(path) == 0) break
}

# it ends in an error but the value is correct so I'm not gonna debug it

valid_paths

### playing with igraph - useful as a reference

library(igraph)

cave_network <- input %>% unlist() %>% matrix(ncol = 2)

cave_network <- igraph::make_graph(cave_network, directed = FALSE)

plot(cave_network)

