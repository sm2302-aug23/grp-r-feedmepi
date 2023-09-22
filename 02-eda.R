library(tidyverse)
library(tinytex)
library(rmarkdown)
library(palmerpenguins)
library(dplyr)
library(tibble)

# due to conflicted package, I ran the code to force all conflicts to become errors
install.packages("devtools")
devtools::install_github("r-lib/conflicted", force = TRUE)
# ✖ dplyr::filter() masks stats::filter()
# ✖ dplyr::lag()    masks stats::lag()

### 2) Exploratory data analysis

#Use `{tidyverse}` data wrangling techniques to find the answers to the
#following questions. For each question, save the R object with the
#suggested name.

#1.  Find the top 10 starting integers that produce the longest sequences
#\[`top10longest`\].

top10longest <- gen_collatz %>%
                  arrange(desc(gen_collatz$length)) %>%
                  select(gen_collatz$length(1:10) & gen_collatz$start) %>%
                  print(gen_collatz$start)
  
# inspired from: 
# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r

#2.  Find out which starting integer produces a sequence that reaches the
#highest maximum value \[`max_val_int`\].

max_Val_int <- gen_collatz %>%
                select(gen_collatz$start & gen_collatz$max_val) %>%
                arrange(desc(gen_collatz$max_val))


#3.  What is the average length and standard deviation of the sequence
#for even starting integers compared to odd ones?
#  \[`even_odd_avg_len` and `even_odd_sd_len`\]
#<!-- Is there a significant^[Apply an appropriate hypothesis test and report the 
#$p$-value.] difference? -->
  
odd_avg_len <- gen_collatz %>%
                select(gen_collatz$parity) %>%
                group_by(gen_collatz$parity, Odd) %>%
                mean()
  


even_odd_avg_len <- 

even_odd_sd_len <- gen_collatz %>%
                    
# Using Shapiro Wilk test to find out the p-value
  shapiro.test(mean = even_odd_avg_len , sd = even_odd_sd_len)

# Thus producing a p-value of 
# Since p-values is greater/less than 0.05, therefore there is a(n) (in)significant
# difference.
  