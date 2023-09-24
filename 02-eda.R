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
top10longest
# inspired from: 
# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r

#2.  Find out which starting integer produces a sequence that reaches the
#highest maximum value 

max_val_int <- collatz_df %>%
               arrange(desc(max_val), .by_group = TRUE) %>%
               slice(1:1, .by = NULL, .preserve = FALSE) %>%
               select(start)

#3.  What is the average length and standard deviation of the sequence
#for even starting integers compared to odd ones?
  

even_odd_avg_len <- collatz_df %>%
                    group_by(parity) %>%
                    summarise(avg = mean(length))

even_odd_sd_len <- collatz_df %>%
                    group_by(parity) %>%
                    summarise(sd = sd(length))

# Using t-test to find out the p-value
  t.test(even_odd_avg_len$avg, var.eq = FALSE)

# Therefore, p-value is given to be 0.04711. Assume the confidence interval is 
# given as 95%, since p-value is less than 0.05 hen there is no significant 
# difference between even and odd.



