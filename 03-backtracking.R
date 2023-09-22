collatz_df

#testing out for loops that satisfies backtracking conditions
for (i in 2:length(n_seq)) {
  index <- n_seq[i]
  if (n_seq[1] > index){
    no <- i
    break
  }
} for (j in no:length(n_seq)){
  index2 <- n_seq[j]
  if(n_seq[1] < index2){
    print(n_seq)
    break
  }
}

#putting for loop in the function

gen_collatz <- function(n) {
  if (n < 1) {
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
  for (i in 2:length(n_seq)) {
    index <- n_seq[i]
    if (n_seq[1] > index){
      no <- i
      break
    }
  } 
  for (j in no:length(n_seq)){
    index2 <- n_seq[j]
    if(n_seq[1] < index2){
      return(n_seq)
      break
    }
  }
}

#testing out on various n
gen_collatz(10)
gen_collatz(9)
gen_collatz(4)
gen_collatz(1)

###
n <- c(2:10000)
result_n <- lapply(n, gen_collatz)

View(result_n)

collatz_df <- tibble(
  start = n,
  seq = result_n
)

view(collatz_df)

###


um <- collatz_df %>%
  unnest(seq) 

um2 <- um %>% 
  filter(seq > start) %>%

uni <- unique(um2$start)

above <- collatz_df[collatz_df$start %in% c(uni), ]


unstack(rev(um))



