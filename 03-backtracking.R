library(tidyverse)

##1
#creating a function that returns only the sequences that backtracks
gen_back <- function(n) {
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
  n_seq3 <- c()
  for (i in 2:length(n_seq)) {
    if (isTRUE(n_seq[1] > n_seq[i] & n_seq[1] < n_seq[i+1]) == TRUE){
      return(n_seq)
    }
  } 
}

#testing out on various n
gen_back(10)
gen_back(9)
gen_back(4)
gen_back(1)

#creating the tibble
n <- c(1:10000)
result_back <- lapply(n, gen_back)

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
#creating a function that gives only the sequences that backtrack
gen_backseq <- function(n) {
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
  n_seq3 <- c()
  for (i in 2:length(n_seq)) {
    if (isTRUE(n_seq[1] > n_seq[i] & n_seq[1] < n_seq[i+1]) == TRUE){
      b <- n_seq[i]
      t <- n_seq[i+1]
      
      n_seq2 <- c(b,t)
      n_seq3 <- c(n_seq3, n_seq2)
    }
  } 
  return(n_seq3)
}

result_backseq <- lapply(n, gen_backseq)


backseq_df <- tibble(
  start = n,
  seq = result_backseq
)

view(backseq_df)


above <- backseq_df %>%
  filter(seq != "NULL") %>%
  unnest(seq) %>%
  filter(seq > start)
  
above

#obtaining the frequency of the sequences going above starting integer
#cited from https://sparkbyexamples.com/r-programming/r-count-frequency-of-all-unique-values-in-vector/#:~:text=There%20are%20multiple%20ways%20to,package%2C%20or%20aggregate()%20function.

start_freq <- as.data.frame(table(above$start))
view(start_freq)

#creating mode function and finding mode of the frequency
#cited from https://www.tutorialspoint.com/how-to-find-mode-for-an-r-data-frame-column

mode <- function(x){
  which.max(tabulate(x))
}

mode_backtrack <- mode(start_freq$Freq)

mode_backtrack

##3
#filtering to obtain sequences that has backtracked only once
one <- start_freq %>% 
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
#identify if starting integer is odd or even by mutate
#inspired from
#https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values

oven <- back_df %>%
  filter(seq != "NULL") %>%
  mutate(evenodd = case_when(start %% 2 == 0 ~ 'even',
                             start %% 2 != 0 ~ 'odd'))

oven

#creating a frequency table

even_odd_backtrack <- as.data.frame(table(oven$evenodd))

even_odd_backtrack



