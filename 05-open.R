## 5) Open-ended exploration

# Does odd numbers produce longer sequences?

top20longest <- collatz_df %>%
                arrange(desc(length), .by_group = TRUE) %>%
                slice(1:20, .by =  NULL) %>%
                select(start, parity) 

# A tibble: 20 × 2
# start parity
# <int> <chr> 
# 1  6171 Odd   
# 2  9257 Odd   
# 3  6943 Odd   
# 4  7963 Odd   
# 5  8959 Odd   
# 6  6591 Odd   
# 7  9887 Odd   
# 8  9897 Odd   
# 9  7422 Even  
# 10  7423 Odd   
# 11  3711 Odd   
# 12  5567 Odd   
# 13  8351 Odd   
# 14  9225 Odd   
# 15  6919 Odd   
# 16  7785 Odd   
# 17  5838 Even  
# 18  5839 Odd   
# 19  2919 Odd   
# 20  8758 Even  

# When arranged based on the length of the sequence, it is shown that 17 out of  
# 20 of the longest sequence has odd number as the starting integer.
# Referring to the boxplot in question 4.3), it is also evident that Odd numbers
# produces longer sequence length. 

