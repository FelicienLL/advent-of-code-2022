input <- stringr::str_split(readLines("day08/input.txt"), "", simplify = TRUE)
class(input) <- "numeric"
test <- stringr::str_split(c(30373,25512,65332,33549,35390), "", simplify = TRUE)
class(test) <- "numeric"

is.visible <- function(i, j, mat){
  maxi <- nrow(mat)
  maxj <- ncol(mat)
  if(any(i==1, j==1, i==nrow(mat), j==ncol(mat))) return(TRUE)
  any(
    all(mat[i,j] > mat[1:(i-1),j]),
    all(mat[i,j] > mat[(i+1):maxi,j]),
    all(mat[i,j] > mat[i,1:(j-1)]),
    all(mat[i,j] > mat[i,(j+1):maxi])
    )
}
output08_01 <- 0
for(i in seq_len(nrow(input))){
  for(j in seq_len(ncol(input))){
    output08_01 <- output08_01 + is.visible(i = i, j = j, mat = input)
  }
}

get.score <- function(x) if(all(x)) length(x) else  which.min(x)
scenic.score <- function(i, j, mat){
  maxi <- nrow(mat)
  maxj <- ncol(mat)
  if(any(i==1, j==1, i==nrow(mat), j==ncol(mat))) return(0)
  down <- get.score((mat[(i+1):maxi,j] - mat[i,j]) < 0)
  up <-   get.score((mat[(i-1):1,j]    - mat[i,j]) < 0)
  right <- get.score((mat[i,(j+1):maxj] - mat[i,j]) < 0)
  left <- get.score((mat[i,(j-1):1]    - mat[i,j]) < 0)
  down*up*right*left
}

output08_02 <- 0
for(i in seq_len(nrow(input))){
  for(j in seq_len(ncol(input))){
    output08_02 <- max(output08_02, scenic.score(i = i, j = j, mat = input))
  }
}
