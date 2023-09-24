
# load library
library(ggplot2)


# top 10 starting integer
top_10_startint <- slice(arrange(.data = start_freq, desc(Freq)), 1:10)
view(top_10_startint)


# plot 1 draft -----------------------------------------------------------------
ggplot(data = start_freq,
       mapping = aes(x = Var1,
                     y = Freq)) +
  geom_point(data = top_10_startint, 
             aes(x = Var1,
                 y = Freq,
                 col = Var1)) +
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
                 col = max_val))

