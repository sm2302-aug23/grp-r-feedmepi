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

Applying all integers from 1 to 10,000. Then creating a tibble named
`collatz_df`, which contains the starting integer (`start`) and the
Collatz sequence (`seq`)

``` r
n <- c(1:10000)
result_n <- lapply(n, gen_collatz) 

collatz_df <- tibble(
  start = n,
  seq = result_n
)
```

``` r
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
```

``` r
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
```

``` r
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
```

``` r
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
```

``` r
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
```

``` r
even_odd_sd_len
```

    ## [1] 45.10308 47.18387

## Including Plots

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
