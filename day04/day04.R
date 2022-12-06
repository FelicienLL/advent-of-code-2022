library(tidyverse)
input <- read_lines("day04/input.txt") %>%
  str_split(",") %>%
  map(str_split, "-") %>%
  map(map, as.double)

overlap <- function(x){
  elf1 <- seq.int(x[[1]][1], x[[1]][2])
  elf2 <- seq.int(x[[2]][1], x[[2]][2])
  any(all(elf2%in%elf1), all(elf1%in%elf2))
}
output04_01 <- sum(sapply(input, overlap))

overlap2 <- function(x){
  elf1 <- seq.int(x[[1]][1], x[[1]][2])
  elf2 <- seq.int(x[[2]][1], x[[2]][2])
  any(intersect(elf1, elf2))
}
output04_02 <- sum(sapply(input, overlap2))
