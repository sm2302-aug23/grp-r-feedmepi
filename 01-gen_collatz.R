# load library

library(tidyverse)
library(tinytex)
library(rmarkdown)
library(palmerpenguins)
library(dplyr)
library(tibble)

# collatz sequence with safeguard

gen_collatz <- function(n) {
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



# add additional columns 
# cited from
# https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values
# https://www.geeksforgeeks.org/apply-lapply-sapply-and-tapply-in-r/



mutate(.data = collatz_df,
       length = as.double(sapply(seq, length)),
       parity = case_when(start %% 2 == 0 ~ 'Even',
                          start %% 2 != 0 ~ 'Odd'),
       max_val = sapply(seq, max)
       )

collatz_df <- mutate(.data = collatz_df,
                     length = as.double(sapply(seq, length)),
                     parity = case_when(start %% 2 == 0 ~ 'Even',
                                        start %% 2 != 0 ~ 'Odd'),
                     max_val = sapply(seq, max)
)

