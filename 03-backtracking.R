library(tidyverse)


#testing out for loops that satisfies backtracking conditions
#cited from
#https://www.programiz.com/r/break-next

for (i in 2:length(n_seq)) {
  index <- n_seq[i]
  if (n_seq[1] > index){
    no <- i
    break
  }
} for (j in no:length(n_seq)){
  index2 <- n_seq[j]
  if(n_seq[1] < index2){
    print(n_seq)
    break
  }
}

#putting for loop in the function

gen_collatz <- function(n) {
  if (n < 1) {
    stop("Input n is invalid!")
  }
  gen <- function(n) {
    if (n %% 2 == 0){
      return(n/2)
    } else (n %% 2 != 0) 
    return(3 * n + 1)
  }
  
  n_seq <- n
  if (n == 1) {
    n_seq <- c(1)
  } else {
  while (n != 1) {
    n <- gen(n)
    n_seq <- c(n_seq, n)
  }
  }
  for (i in 2:length(n_seq)) {
    index <- n_seq[i]
    if (n_seq[1] > index){
      no <- i
      break
    }
  } 
  for (j in no:length(n_seq)){
    index2 <- n_seq[j]
    if(n_seq[1] < index2){
      return(n_seq)
      break
    }
  }
}

#testing out on various n
gen_collatz(10)
gen_collatz(9)
gen_collatz(4)
gen_collatz(1)

#creating the tibble
n <- c(2:10000)
result_back <- lapply(n, gen_collatz)

View(result_back)

back_df <- tibble(
  start = n,
  seq = result_back
)

view(back_df)

#filtering the collatz_df

backtracks_df <- back_df %>%
  filter(seq != "NULL") %>%
  select(start)

view(backtracks_df)

um <- collatz_df %>%
  unnest(seq) 

um2 <- um %>% 
  filter(seq > start) %>%

uni <- unique(um2$start)

above <- collatz_df[collatz_df$start %in% c(uni), ]


unstack(rev(um))



