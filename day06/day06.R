library(tidyverse)
input <- str_split(read_lines("day06/input.txt"), "")[[1]]
is_marker <- function(x, n, k = 4){
  if(n < k) return(FALSE)
  length(unique(x[(n-(k-1)):n])) == k
}
output_06_01 <- min(which(sapply(seq_along(input), is_marker, x = input, k = 4)))
output_06_02 <- min(which(sapply(seq_along(input), is_marker, x = input, k = 14)))
