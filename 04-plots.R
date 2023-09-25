
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

top_10_highest <- backtracks_df %>% 
  arrange(desc(max_val)) %>%
  filter(max_val >= highest$max_val[10])
  

ggplot(data = backtracks_df,
       mapping = aes(x = start,
                     y = max_val)
       ) +
  geom_point(data = top_10_highest,
             aes(x = start,
                 y = max_val,
                 col = max_val)
             ) +
  labs(
    title = "Collatz Conjecture",
    subtitle = paste(
      "Starting integer and Highest Sequence Value"),
    x = "Starting integer",
    y = "Highest Sequence Value"
  ) 

