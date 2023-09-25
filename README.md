FeedMePi Group Assignment
================

## Contributions

- Task 1: @Sia Yee Zee, @AqilMN
- Task 2: @izzati-aziz
- Task 3: @AqilMN
- Task 4: @Sia Yee Zee, @AqilMN, @izzati-aziz
- Task 5: @izzati-aziz
- Task 6: @Sia Yee Zee
- README: @AqilMN

## Tasks

### 1) Generating the Collatz Conjecture

Creating a function called `gen_collatz` that takes a positive integer
`n` and generates the Collatz sequence.

``` r
gen_collatz <- function(n) {
  if (n != as.integer(n) | n < 1) {
    stop("Input n is invalid!")
  }
  gen <- function(n) {
    if (n %% 2 == 0){
      return(n/2)
    } else (n %% 2 != 0) 
    return(3 * n + 1)
  }
  
  n_seq <- n
  if (n == 1) {
    n_seq <- c(1)
  } else {
    while (n != 1) {
      n <- gen(n)
      n_seq <- c(n_seq, n)
    }
  }
  return(n_seq) 
}
```

There is a safeguard implemented for if `n` is less than 1 or not an
integer:

`if (n != as.integer(n) | n < 1) {     stop("Input n is invalid!")   }`

Applying all integers from 1 to 10,000 to the function. Then creating a
tibble named `collatz_df`, which contains the starting integer (`start`)
and the Collatz sequence (`seq`)

``` r
n <- c(1:10000)
result_n <- lapply(n, gen_collatz) 

collatz_df <- tibble(
  start = n,
  seq = result_n
)

collatz_df
```

    ## # A tibble: 10,000 × 2
    ##    start seq       
    ##    <int> <list>    
    ##  1     1 <dbl [1]> 
    ##  2     2 <dbl [2]> 
    ##  3     3 <dbl [8]> 
    ##  4     4 <dbl [3]> 
    ##  5     5 <dbl [6]> 
    ##  6     6 <dbl [9]> 
    ##  7     7 <dbl [17]>
    ##  8     8 <dbl [4]> 
    ##  9     9 <dbl [20]>
    ## 10    10 <dbl [7]> 
    ## # ℹ 9,990 more rows

By using the mutate function, we add columns for the length of the
sequence (`length`), whether the starting integer is even or odd
(`parity`) and the maximum value reached in the sequence (`max_val`)

``` r
collatz_df <- mutate(.data = collatz_df,
                     length = as.double(sapply(seq, length)),
                     parity = case_when(start %% 2 == 0 ~ 'Even',
                                        start %% 2 != 0 ~ 'Odd'),
                     max_val = sapply(seq, max)
)

collatz_df
```

    ## # A tibble: 10,000 × 5
    ##    start seq        length parity max_val
    ##    <int> <list>      <dbl> <chr>    <dbl>
    ##  1     1 <dbl [1]>       1 Odd          1
    ##  2     2 <dbl [2]>       2 Even         2
    ##  3     3 <dbl [8]>       8 Odd         16
    ##  4     4 <dbl [3]>       3 Even         4
    ##  5     5 <dbl [6]>       6 Odd         16
    ##  6     6 <dbl [9]>       9 Even        16
    ##  7     7 <dbl [17]>     17 Odd         52
    ##  8     8 <dbl [4]>       4 Even         8
    ##  9     9 <dbl [20]>     20 Odd         52
    ## 10    10 <dbl [7]>       7 Even        16
    ## # ℹ 9,990 more rows

### 2) Exploratory data analysis

##### 1. The top 10 starting integers that produce the longest sequences

Saved as `top10longest`

``` r
top10longest <- collatz_df %>%
                arrange(desc(length), .by_group = TRUE) %>%
                slice(1:10, .by =  NULL) %>%
                select(start) %>%
                unlist()

top10longest
```

    ##  start1  start2  start3  start4  start5  start6  start7  start8  start9 start10 
    ##    6171    9257    6943    7963    8959    6591    9887    9897    7422    7423

##### 2. The starting integer which produces a sequence that reaches the highest maximum value

Saved as `max_val_int`

``` r
max_val_int <- collatz_df %>%
               arrange(desc(max_val), .by_group = TRUE) %>%
               slice(1:1, .by = NULL, .preserve = FALSE) %>%
               select(start) %>%
               unlist()

max_val_int
```

    ## start 
    ##  9663

##### 3. The average length and standard deviation of the sequence for even starting integers compared to odd ones

The average length of the sequences for even and odd starting integers,
saved as `even_odd_avg_len`

``` r
even_odd_avg <- collatz_df %>%
                       group_by(parity) %>% 
                       summarise(avg = mean(length)) %>%
                       select(avg)
  
even_odd_avg_len <- even_odd_avg$avg 

even_odd_avg_len
```

    ## [1] 79.5936 92.3396

The standard deviation of the length of the sequences for even and odd
starting integers, saved as `even_odd_sd_len`

``` r
even_odd_sd <- collatz_df %>%
                    group_by(parity) %>%
                    summarise(sd = sd(length)) %>%
                    select(sd)

even_odd_sd_len <- even_odd_sd$sd

even_odd_sd_len
```

    ## [1] 45.10308 47.18387

### 3) Investigating “backtracking” in sequences

Backtracking is when a sequence reaches a value that is less than the
starting integer, but then increases again above the starting integer at
least once before reaching 1.

##### 1. Retain starting integers that exhibit backtracking in their sequences

Modifying the function from **Task 1**. By changing the function name to
`gen_back` and adding a for loop with the conditions for sequences that
backtrack. The function returns sequences that backtrack, and returns
“NULL” for the sequences that do not backtrack.

``` r
gen_back <- function(n) {
          ...
          ...
          ...
    while (n != 1) {
      n <- gen(n)
      n_seq <- c(n_seq, n)
    }
  }
  n_seq3 <- c()
  for (i in 2:length(n_seq)) {
    if (isTRUE(n_seq[1] > n_seq[i] & n_seq[1] < n_seq[i+1]) == TRUE){
      return(n_seq)
    }
  } 
}
```

Creating a tibble called `back_df` after applying integers from 1 to
10,000 to the function.

``` r
n <- c(1:10000)
result_back <- lapply(n, gen_back)

back_df <- tibble(
  start = n,
  seq = result_back
)
```

Obtaining `backtracks_df` by filtering and adding columns `length`,
`parity` and `max_val`

``` r
backtracks_df <- back_df %>%
  filter(seq != "NULL") %>%
  mutate(length = as.double(sapply(seq, length)),
         parity = case_when(start %% 2 == 0 ~ 'Even',
                            start %% 2 != 0 ~ 'Odd'),
         max_val = sapply(seq, max)
  )

backtracks_df
```

    ## # A tibble: 8,229 × 5
    ##    start seq        length parity max_val
    ##    <int> <list>      <dbl> <chr>    <dbl>
    ##  1     6 <dbl [9]>       9 Even        16
    ##  2     7 <dbl [17]>     17 Odd         52
    ##  3     9 <dbl [20]>     20 Odd         52
    ##  4    10 <dbl [7]>       7 Even        16
    ##  5    11 <dbl [15]>     15 Odd         52
    ##  6    12 <dbl [10]>     10 Even        16
    ##  7    13 <dbl [10]>     10 Odd         40
    ##  8    14 <dbl [18]>     18 Even        52
    ##  9    15 <dbl [18]>     18 Odd        160
    ## 10    17 <dbl [13]>     13 Odd         52
    ## # ℹ 8,219 more rows

##### 2 Most frequently occuring number of times sequences that backtrack go above their starting integer

Modifying the function from **Task 3 Part 1**. By changing the function
name to `gen_back_seq` and adding a for loop with the conditions
sequences that backtrack. The function returns parts of sequences that
experiences backtracking and returns “NULL” for the sequences that do
not backtrack.

``` r
gen_back_seq <- function(n) {
          ...
          ...
          ...
  n_seq3 <- c()
  for (i in 2:length(n_seq)) {
    if (isTRUE(n_seq[1] > n_seq[i] & n_seq[1] < n_seq[i+1]) == TRUE){
      b <- n_seq[i]
      t <- n_seq[i+1]
      
      n_seq2 <- c(b,t)
      n_seq3 <- c(n_seq3, n_seq2)
    }
  } 
  return(n_seq3)
}
```

## Including Plots

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
