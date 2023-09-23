library(tidyverse)

##1
#creating a function that returns only the whole sequence that backtrack occurs
#error fixing cited from
#https://stackoverflow.com/questions/7355187/error-in-if-while-condition-missing-value-where-true-false-needed
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
  filter(seq != "NULL")

backtracks_df

##2
#creating a function that returns only the sequences that backtrack
gen_back_seq <- function(n) {
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

#creating the tibble and filtering sequence numbers that are higher than start
result_back_seq <- lapply(n, gen_back_seq)


back_seq_df <- tibble(
  start = n,
  seq = result_back_seq
)

view(back_seq_df)


above <- back_seq_df %>%
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
#creating a function that returns maximum value after first backtrack
gen_back_max <- function(n) {
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
      return(n_seq[i+1])
    }
  } 
}

#creating tibble of the max values
result_back_max <- lapply(n, gen_back_max)

back_max_df <- tibble(
  start = n,
  seq = result_back_max
)

view(back_max_df)

back_max <- back_max_df %>% 
  filter(seq != "NULL")

max_after_backtrack <- back_max$seq

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

even_odd_backtrack <- as.data.frame(table(oven$evenodd))$Freq

even_odd_backtrack
