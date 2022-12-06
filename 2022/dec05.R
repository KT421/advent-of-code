# AOC 2022
# Day 05

input <- readLines("2022/input/dec05.txt") 

# yeah I hardcoded it. Parsing text input is hard.
crate_stacks <- list(a = c("Q","F","M","R","L","W","C","V"),
                     b = c("D","Q","L"),
                     c = c("P","S","R","G","W","C","N","B"),
                     d = c("L","C","D","H","B","Q","G"),
                     e = c("V","G","L","F","Z","S"),
                     f = c("D","G","N","P"),
                     g = c("D","Z","P","V","F","C","W"),
                     h = c("C","P","D","M","S"),
                     i = c("Z","N","W","T","V","M","P","C"))

#save a clean copy for part2
crate_stacks2 <- crate_stacks

steps <- input[11:511] %>%
  str_extract_all("[:digit:]+") 

# PT1

for (i in 1:length(steps)) {
  
  num <- as.numeric(steps[[i]][1])
  from <- as.numeric(steps[[i]][2])
  to <- as.numeric(steps[[i]][3])
  
  for (j in 1:num) {
    # add the crate to the end of the new stack
    crate_stacks[[to]] <- append(crate_stacks[[to]],crate_stacks[[from]][length(crate_stacks[[from]])])
    
    # remove it from the origin stack
    crate_stacks[[from]] <- head(crate_stacks[[from]],-1)
  }
}

mapply(tail,crate_stacks,1) %>% paste(collapse = "")

# PT2

for (i in 1:length(steps)) {
  
  num <- as.numeric(steps[[i]][1])
  from <- as.numeric(steps[[i]][2])
  to <- as.numeric(steps[[i]][3])
  
    # add the crates to the end of the new stack
    crate_stacks2[[to]] <- append(crate_stacks2[[to]],crate_stacks2[[from]][(length(crate_stacks2[[from]])-num+1):(length(crate_stacks2[[from]]))])
    
    # remove it from the origin stack
    crate_stacks2[[from]] <- head(crate_stacks2[[from]],-num)

}

mapply(tail,crate_stacks2,1) %>% paste(collapse = "")
