
# load library
library(ggplot2)


# plot 1 draft -----------------------------------------------------------------
# top 10 starting integer
# cited from
# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r

top_10_startint <- backtracks_df %>%
  arrange(desc(length)) %>%
  group_by(length) %>%
  slice(1:10)


# ggplot 1
ggplot(data = backtracks_df,
       mapping = aes(x = start,
                     y = length )) +
  geom_point(data = top_10_startint, 
             aes(x = start,
                 y = length,
                 col = length)) +
  labs(
    title = "Collatz Conjecture",
    subtitle = paste(
      "Starting integer and Sequence length"),
    x = "Starting integer",
    y = "Sequence length"
  )


#plot 2 draft ------------------------------------------------------------------
#unique function cited from
#https://www.geeksforgeeks.org/unique-function-in-r/

highest <- unique(backtracks_df[c("max_val")]) %>%
  arrange(desc(max_val))

highest

top_10_highest <- backtracks_df %>% 
  arrange(desc(max_val)) %>%
  mutate(top10 = case_when(max_val >= highest$max_val[10] ~ 'top10',
                           max_val < highest$max_val[10] ~ 'nottop10'))


ggplot(data = top_10_highest,
       mapping = aes(x = start,
                     y = max_val)
       ) +
  geom_point(aes(col = top10)
             ) +
  labs(
    title = "Collatz Conjecture",
    subtitle = paste(
      "Starting integer and Highest Sequence Value"),
    x = "Starting integer",
    y = "Highest Sequence Value"
  ) 


# plot 3------------------------------------------------------------------------
# boxplot comparing the distributions of sequence lengths for even and odd 
# starting integers

ggplot(data = collatz_df ,
       mapping =  aes( x = length, 
                       y = parity)) +
geom_boxplot()

# Are there any noticeable differences?

# Although both boxplot are right skewed, it is evident that Odd numbers have a
# higher average length than Even numbers. Also, Odd numbers have a few outlier
# unlike Even numbers, where Even numbers has no outlier.









