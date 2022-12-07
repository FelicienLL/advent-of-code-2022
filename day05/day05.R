library(tidyverse)
input_stack <- read_fwf("day05/input.txt", n_max = 8) %>%
  map(~ rev(str_remove_all(.x[!is.na(.x)], "\\[|\\]")))
input_command <- read_lines("day05/input.txt", skip = 10) %>%
  str_extract_all("\\d+", simplify = TRUE)
make_move <- function(x, instr){
  instr <- as.double(instr)
  move <- instr[1]
  from <- instr[2]
  to <- instr[3]
  for (i in seq_len(move)){
    x[[to]][length(x[[to]]) + 1] <- tail(x[[from]], 1)
    x[[from]] <- head(x[[from]], length(x[[from]]) - 1)
  }
  x
}
stack <- input_stack
for (i in seq_len(nrow(input_command))){
  stack <- make_move(stack, input_command[i,])
}
output_05_01 <- paste(sapply(stack, tail, 1), collapse = "")
make_move9001 <- function(x, instr){
  instr <- as.double(instr)
  move <- instr[1]
  from <- instr[2]
  to <- instr[3]
  x[[to]][(length(x[[to]])+1):(length(x[[to]]) + move)] <- tail(x[[from]], move)
  x[[from]] <- head(x[[from]], length(x[[from]]) - move)
  x
}
stack <- input_stack
for (i in seq_len(nrow(input_command))){
  stack <- make_move9001(stack, input_command[i,])
}
output_05_02 <- paste(sapply(stack, tail, 1), collapse = "")
