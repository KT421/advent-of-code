# AOC 2022
# Day 07

library(tidyverse)

input <- readLines("2022/input/dec07.txt") 

current_path <- "root" # instead of / so I can be clean with detecting alpha characters
all_paths <- current_path

file_structure <- tibble(path = as.character(),
                         filesize = as.numeric())

for (i in input) {
  
  # skip ls and dir lines
  if (i == "$ ls") next
  if (str_detect(i, "dir ")) next
  if (i == "$ cd /") next
  
  # add filesize to list
  if (str_detect(i, "[:digit:]")) {
    filesize <- str_extract(i, "[:digit:]+") %>% as.numeric()
    current_file <- tibble(path = current_path,
                           filesize = filesize)
    file_structure <- rbind(file_structure,current_file)
    next
  }
  
  # change directory
  
  if (str_detect(i, "\\$ cd [:alpha:]+")) {
    dir_name <- str_split(i, " ") %>% unlist()
    dir_name <- dir_name[[3]]
    current_path <- paste0(current_path," ",dir_name)
    all_paths <- c(all_paths,current_path)
    next
  }
    
  # step up a directory
  if (i == "$ cd ..") 
    current_path <- str_remove(current_path, "(?:.(?! ))+$")
  next
}

# PT 1 

grouped <- file_structure %>%
  group_by(path) %>%
  summarise(filesize = sum(filesize))

grouped_dirs <- tibble(dir = as.character(),
                       size = as.numeric())

for (i in all_paths) {
  rolled_up_filesize <- grouped %>%
    filter(str_detect(grouped$path,i) == T) %>%
    pull(filesize) %>%
    sum()
  current_dir <- tibble(dir = i,
                         size = rolled_up_filesize)
  grouped_dirs <- rbind(grouped_dirs,current_dir)
}

grouped_dirs %>%
  filter(size <= 100000) %>%
  pull(size) %>%
  sum()
         
# PT2 

available_space <- 70000000 - sum(file_structure$filesize)
needed_space <- 30000000 - available_space

grouped_dirs %>%
  filter(size >= needed_space) %>%
  pull(size) %>%
  min()
