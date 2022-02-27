library(microbenchmark)

aoc_benchmarks <- microbenchmark(
  {source("dec1.R")},
  {source("dec2.R")},
  {source("dec3.R")},
  {source("dec4.R")},
  {source("dec5.R")},
  {source("dec6.R")},
  {source("dec7.R")},
  {source("dec8.R")},
  {source("dec9.R")},
  {source("dec10.R")},
  {source("dec11.R")}, times = 10L)

library(ggplot2)

aoc_benchmarks

autoplot(aoc_benchmarks)

ggsave("aoc_benchmarks.png")