# A Rock, B Paper, C Scissors
input <- set_names(read.table("day02/input.txt"), c("opp", "me"))

game <- function(opp, me){
  y <- character(length = length(me))
  y[opp=="A" & me=="B" | opp=="B" & me=="C" | opp=="C" & me=="A"] <- "win"
  y[opp==me] <- "draw"
  y[opp=="A" & me=="C" | opp=="B" & me=="A" | opp=="C" & me=="B"] <- "lose"
  y
}
score_choice <- function(x){
  ifelse(x == "A", 1, ifelse(x == "B", 2, 3))
}

score_game <- function(x){
  ifelse(x == "win", 6, ifelse(x == "lose", 0, 3))
}

# Signif 1: X Rock, Y Paper, Z Scissors
signif1 <- function(x){
  ifelse(x == "X", "A", ifelse(x == "Y", "B", "C"))
}
output02_01 <- sum(
  score_choice(signif1(input$me)),
  score_game(game(opp = input$opp, me = signif1(input$me)))
)

# Signification 2: X lose, Y draw, Z win
signif2 <- function(x){
  ifelse(x == "X", "lose", ifelse(x == "Y", "draw", "win"))
}

what_to_play <- function(opp, result){
  y <- character(length = length(opp))
  y[result == "win" & opp == "C" | result == "draw" & opp == "A" | result == "lose" & opp == "B"] <- "A"
  y[result == "win" & opp == "A" | result == "draw" & opp == "B" | result == "lose" & opp == "C"] <- "B"
  y[result == "win" & opp == "B" | result == "draw" & opp == "C" | result == "lose" & opp == "A"] <- "C"
  y
}

output02_02 <- sum(
  score_choice(what_to_play(opp = input$opp, result = signif2(input$me))),
  score_game(signif2(input$me))
)
