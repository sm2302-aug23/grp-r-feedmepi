library(tidyverse)
library(tinytex)
library(rmarkdown)
library(palmerpenguins)
library(dplyr)
library(tibble)


### 2) Exploratory data analysis

#1.  Find the top 10 starting integers that produce the longest sequences

top10longest <- gen_collatz %>%
                arrange(desc(gen_collatz$length), .by_group = TRUE) %>%
                select(gen_collatz$length(1:10) & gen_collatz$start) %>%
                print(gen_collatz$start)
  
# inspired from: 
# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r

#2.  Find out which starting integer produces a sequence that reaches the
#highest maximum value 

max_Val_int <- gen_collatz %>%
              select(gen_collatz$start & gen_collatz$max_val) %>%
              arrange(desc(gen_collatz$max_val), .by_group = TRUE) %>%
              select(gen_collatz$start(1:1))


#3.  What is the average length and standard deviation of the sequence
#for even starting integers compared to odd ones?
  
odd <- gen_collatz %>%
      select(gen_collatz$parity) %>%
      group_by(Odd) %>%
      print(gen_collatz$length)

mean(odd)  
sd(odd)

even <- gen_collatz %>%
        select(gen_collatz$parity) %>%
        group_by(Even) %>%
        print(gen_collatz$length)

mean(even)
sd(even)

even_odd_avg_len <- mean(odd & even)

even_odd_sd_len <- sd(odd & even)
  
# Using Shapiro Wilk test to find out the p-value
  shapiro.test(mean = even_odd_avg_len, sd = even_odd_sd_len)

# If p-value is less than 0.05, therefore there is an insignificant difference.
# If p-value is greater than 0.05, therefore there is a significant difference
# where the mean is not equal.
  