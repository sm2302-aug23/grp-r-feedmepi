library(tidyverse)
library(tinytex)
library(rmarkdown)
library(palmerpenguins)
library(dplyr)
library(tibble)

### 2) Exploratory data analysis

#Use `{tidyverse}` data wrangling techniques to find the answers to the
#following questions. For each question, save the R object with the
#suggested name.

#1.  Find the top 10 starting integers that produce the longest sequences
#\[`top10longest`\].

top10longest <- gen_collatz %>%
                  arrange(desc(gen_collatz$length)) %>%
                  select(gen_collatz$length(1:10))

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
  
even_odd_avg_len <- gen_collatz %>%
                  select(gen_collatz$parity )

even_odd_sd_len <- gen_collatz %>%
                    
# Using shapiro.wilke test to find out the p-value
  
  