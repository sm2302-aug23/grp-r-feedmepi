
# Question
# How often specific numbers (other than 1) appear in sequences, across all starting integers

# Answer
# Numbers 2, 4, 8, 16 have been found in most of the sequences, across all starting integers



# finding the appearance frequencies of these numbers

seq_freq <- collatz_df %>%
  filter(seq != "NULL") %>%
  filter(start != 1) %>%
  select(start, seq) %>%
  unnest(seq)

seq_freq2 <- as.data.frame(table(seq_freq$seq)) %>%
  filter(Var1 != 1) %>%
  arrange(desc(Freq), .by_group = TRUE) %>%
  slice(1:10)

ggplot(data = seq_freq2,
       aes(Var1,
           Freq,
           fill = Var1)
) + geom_bar(stat = "identity")+
  labs(
    x = "Starting Integer",
    y = "Frequency",
  )
