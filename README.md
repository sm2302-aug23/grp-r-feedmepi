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

collatz_df <- data.frame(
  start = n,
  seq = list(gen_collatz(n))
)


