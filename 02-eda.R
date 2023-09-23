library(tidyverse)
library(tinytex)
library(rmarkdown)
library(palmerpenguins)
library(dplyr)
library(tibble)


### 2) Exploratory data analysis

#1.  Find the top 10 starting integers that produce the longest sequences

top10longest <- collatz_df %>%
                arrange(desc(length), .by_group = TRUE) %>%
                slice(1:10, .by =  NULL, .preserve = FALSE) %>%
                select(start)

# inspired from: 
# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r

#2.  Find out which starting integer produces a sequence that reaches the
#highest maximum value 

max_Val_int <- collatz_df %>%
               arrange(desc("max_val"), .by_group = TRUE) %>%
               slice(1:1, .by = NULL, .preserve = TRUE) %>%
               select(start)

max_Val_int

#3.  What is the average length and standard deviation of the sequence
#for even starting integers compared to odd ones?
  
odd <- collatz_df %>%
       select(collatz_df$parity) %>%
       group_by(Odd) %>%
       print(collatz_df$length(1:n))

mean(odd)  
sd(odd)

even <- collatz_df %>%
        select(parity) %>%
        group_by(Even) %>%
        print(collatz_df$length)

mean(even)
sd(even)

even_odd_avg_len <- mean(odd & even)

even_odd_sd_len <- sd(odd & even)
  
# Using t-test to find out the p-value
  t.test(odd, even, var.eq = FALSE)

# If p-value is less than 0.05, therefore there is an insignificant difference.
# If p-value is greater than 0.05, therefore there is a significant difference
# where the mean is not equal.
  


  