input01 <- readr::read_file("day01/input.txt")
ans <- (input01 %>%
  stringr::str_remove("\n$") %>%
  stringr::str_split("\n\n") %>%
  lapply(str_split, "\n"))[[1]] %>%
  lapply(as.double) %>%
  sapply(sum)

out01_01 <- max(ans)

out01_02 <- sum(ans[tail(order(ans),3)])
