library(tidyverse)

##1
#creating a function that returns only seq where backtrack occurs
#error fixing cited from
#https://stackoverflow.com/questions/7355187/error-in-if-while-condition-missing-value-where-true-false-needed

gen_back <- function(n) {
  if (n != as.integer(n) | n < 1) {
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
    if (isTRUE(n_seq[1] > n_seq[i] & n_seq[1] < n_seq[i+1]) == TRUE){
      return(n_seq)
    }
  } 
}

#creating the tibble, filtering out "NULL" and adding more columns
n <- c(1:10000)
result_back <- lapply(n, gen_back)

backtracks_df <- tibble(
  start = n,
  seq = result_back
) %>%
  filter(seq != "NULL") %>%
  mutate(length = as.double(sapply(seq, length)),
         parity = case_when(start %% 2 == 0 ~ 'Even',
                            start %% 2 != 0 ~ 'Odd'),
         max_val = sapply(seq, max))

backtracks_df

##2
#creating a function that returns only parts of the sequences that backtrack
gen_back_seq <- function(n) {
  if (n != as.integer(n) | n < 1) {
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

#creating the tibble and removing "NULL" then unnesting and filtering sequence 
#numbers that are higher than start

result_back_seq <- lapply(n, gen_back_seq)

back_seq_df <- tibble(
  start = n,
  seq = result_back_seq
) %>%
  filter(seq != "NULL") %>%
  unnest(seq) %>%
  filter(seq > start)

#obtaining the frequency of the sequences going above starting integer
#cited from https://sparkbyexamples.com/r-programming/r-count-frequency-of-all-unique-values-in-vector/#:~:text=There%20are%20multiple%20ways%20to,package%2C%20or%20aggregate()%20function.

start_freq <- as.data.frame(table(back_seq_df$start))

#creating mode function and finding mode of the frequency
#cited from https://statisticsglobe.com/mode-in-r-programming-example

my_mode <- function(x) {                      
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

mode_backtrack <- as.integer(my_mode(start_freq$Freq))

mode_backtrack

##3
#creating a function that returns maximum value after first backtrack
gen_back_max <- function(n) {
  if (n != as.integer(n) | n < 1) {
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
    if (isTRUE(n_seq[1] > n_seq[i] & n_seq[1] < n_seq[i+1]) == TRUE){
      return(max(n_seq[-(0:i+1)]))
    }
  } 
}

#creating tibble of the max values
result_back_max <- lapply(n, gen_back_max)

back_max_df <- tibble(
  start = n,
  max = result_back_max
) %>% 
  filter(max != "NULL")

max_after_backtrack <- unlist(back_max_df$max)

max_after_backtrack

##4
#creating a frequency table

even_odd <- as.data.frame(table(backtracks_df$parity))$Freq