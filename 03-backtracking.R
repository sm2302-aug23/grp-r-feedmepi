library(tidyverse)

##1
#testing out for loops that satisfies backtracking conditions
#inspired from
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

#filtering the collatz_df, obtaining bactracks_df

backtracks_df <- back_df %>%
  filter(seq != "NULL") %>%
  select(start)

backtracks_df
view(backtracks_df)

##2
#obtaining the instances where the sequence goes above starting integer

above <- back_df %>%
  filter(seq != "NULL") %>%
  unnest(seq) %>%
  filter(seq > start)
  
above

#obtaining the frequency of the sequences going above starting integer
#cited from https://sparkbyexamples.com/r-programming/r-count-frequency-of-all-unique-values-in-vector/#:~:text=There%20are%20multiple%20ways%20to,package%2C%20or%20aggregate()%20function.

freq <- as.data.frame(table(above$start))

#finding mode of the frequency
#cited from https://www.tutorialspoint.com/how-to-find-mode-for-an-r-data-frame-column

mode <- function(x){
  which.max(tabulate(x))
}

mode_backtrack <- mode(freq$Freq)

mode_backtrack

##3
#filtering to obtain sequences that has backtracked only once
one <- freq %>% 
  filter(Freq == 1) %>%
  select(Var1)

one

#filtering the 'above' that contains the starting integers that backtrack once
#so that we get the max value reached

max_after_backtrack <- above %>%
  filter(start %in% one$Var1) %>%
  select(seq)

max_after_backtrack

##4

as.vector(one)

above$start[as.vector(one)]


um <- collatz_df %>%
  unnest(seq) 

um2 <- um %>% 
  filter(seq > start) %>%

  uniq
uni <- unique(um2$start)

above <- collatz_df[collatz_df$start %in% c(uni), ]


unstack(rev(um))



