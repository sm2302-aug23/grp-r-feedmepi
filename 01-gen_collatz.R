library(tidyverse)
library(tinytex)
library(rmarkdown)
library(palmerpenguins)
library(dplyr)
library(tibble)

#Aqil draft

gen_collatz <- function(n) {
  gen <- function(n) {
    if (n %% 2 == 0){
      return(n/2)
    } else (n %% 2 != 0) 
    return(3 * n + 1)
  }
  
  n_seq <- n
  while (n != 1) {
    n <- gen(n)
    n_seq <- c(n_seq, n)
  }
  return(n_seq) 
}


# add safeguard

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
  while (n != 1) {
    n <- gen(n)
    n_seq <- c(n_seq, n)
  }
  return(n_seq) 
}




# apply function to all integers from 1 to 10000
# cited from 
# https://www.dataquest.io/blog/apply-functions-in-r-sapply-lapply-tapply/

n <- c(1:10000)

result_n <- lapply(n, gen_collatz) 


# create collatz data frame

collatz_df <- tibble(
  start = n,
  seq = result_n
)

collatz_df

