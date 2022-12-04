input <- strsplit(readr::read_file("day03/input.txt"), "\\n")[[1]]

find_duplicates <- function(x){
  comp1 <- stringr::str_sub(x, 1, nchar(x)/2)
  comp2 <- stringr::str_sub(x, nchar(x)/2 + 1, nchar(x))
  purrr::map2_chr(
    str_split(comp1, ""),
    str_split(comp2, ""),
    intersect
  )
}

output03_01 <- sum(match(find_duplicates(input), c(letters, LETTERS)))

makegroups <- function(x){
  split(x, rep(seq_len(length(x)/3), each = 3))
}
identify_letter <- function(x){
  y <- str_split(x, "")
  intersect(intersect(y[[1]], y[[2]]), y[[3]])
}
output03_02 <- input %>%
  makegroups() %>%
  purrr::map_chr(identify_letter) %>%
  match(c(letters, LETTERS)) %>%
  sum()
