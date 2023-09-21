library(tidyverse)
library(tinytex)
library(rmarkdown)
library(palmerpenguins)
library(dplyr)
library(tibble)


# using if, else if and stop statements from lecture 1 -------------------------

gen_collatz <- function(n) {
  if (n > 0 && n %% 2 == 0) {
    n / 2
  } else if (n > 0 && n %% 2 != 0) {
    3 * n + 1
  } else if (n < 0) {
    stop ("Input is negative")
  }
}

# assigning integer 1 to 10000 into function(n) --------------------------------

n <- c(1 : 10000)


# creating collatz data frame --------------------------------------------------
# sapply function cited from 
#https://www.geeksforgeeks.org/apply-lapply-sapply-and-tapply-in-r/

collatz_df <- tibble(data.frame(
  start = n,
  seq = sapply(n, gen_collatz)
 )
)

collatz_df

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

gen_collatz(1)
gen_collatz(5)
gen_collatz(10)

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


gen_collatz(-1)
gen_collatz(-2)



