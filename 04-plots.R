
# load library
library(ggplot2)


# plot 1 draft -----------------------------------------------------------------
# top 10 starting integer
# cited from
# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r

top_10_seq_length <- backtracks_df %>%
  arrange(desc(length)) %>%
  slice(1:10) %>%
  group_by(length)

top_10_startint <- backtracks_df %>%
  arrange(desc(length)) %>%
  group_by(length)

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
  mutate(top10 = case_when(max_val >= highest$max_val[10] ~ 'Top10',
                           max_val < highest$max_val[10] ~ 'Not_Top10'))


ggplot(data = top_10_highest,
       mapping = aes(x = start,
                     y = max_val)
       ) +
  geom_point(aes(colour = top10),
             alpha = 0.8
             ) +
  scale_x_continuous(
    breaks = seq(0, 10000, by = 1000)
  ) +
  scale_y_continuous(
    breaks = seq(0, 2.8e+07, by = 0.15e+07)
  ) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(
    title = "Backtracking within Collatz Sequences",
    subtitle = paste(
      "Highest Value Reached in the Sequence against Starting Integer"),
    x = "Starting Integer",
    y = "Highest Sequence Value",
    colour = "Top 10 Highest Values"
  ) +
  theme_minimal()


# plot 3------------------------------------------------------------------------
# boxplot comparing the distributions of sequence lengths for even and odd 
# starting integers

ggplot(data = backtracks_df ,
       mapping =  aes( x = parity, 
                       y = length)) +
  
geom_boxplot() +
  
labs(title = "Distribution of Sequence Lengths when Backtrack occurs",
     x = "Parity of Starting Integers",
     y = "Sequence Lengths")
  
# Are there any noticeable differences?

# The most noticeable difference is that Odd starting integers has outliers where
# Even starting integers does not have outliers. Also, Odd starting integers has
# a higher average of length compared to Even starting integers.









