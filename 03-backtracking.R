collatz_df

collatz_df %>% 
  filter(seq %in% 1)

view(collatz_df)

result_n <- as.list(result_n)
str

typeof(result_n)

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
  for (i in 2:length(n_seq)){
    index <- n_seq[i]
    if (n_seq[1] > index) {
      for (j in i+1:length(n_seq)){
        sec_index <- n_seq[j]
        if (n_seq[1] < sec_index){
          return(n_seq)
        } else {NULL}
      }
      
    } else {NULL}
  } 
}

gen_collatz(3)
###

gen_collatz2 <- function(n) {
  gen <- function(n) {
    if (n %% 2 == 0){
      return(n/2)
    } else (n %% 2 != 0) 
    return(3 * n + 1)
  }
  n_seq <- n
  n_seq2 <- n
  if (n < 1) {
    stop("Input n is invalid!")
  } else if (n == 1) {
    n_seq <- c(1)
  } else {
  while (n != 1) {
    n <- gen(n)
    if(gen(n) > n_seq2){
    n_seq <- c(n_seq, n)
    }
  }
  return(n_seq) 
}
}

gen_collatz(1)
length(gen_collatz(5))
gen_collatz(5)[2]

n <- c(1:10000)

result_n <- lapply(n, gen_collatz)
result_n2 <- lapply(n, gen_collatz2)

collatz_df <- tibble(
  start = n,
  seq = result_n
)

collatz_df2 <- tibble(
  start = n,
  seq = result_n2
)

collatz_df
view(collatz_df)

um <- collatz_df %>%
  unnest(seq) 

um2 <- um %>% 
  filter(seq > start) %>%

uni <- unique(um2$start)

above <- collatz_df[collatz_df$start %in% c(uni), ]


unstack(rev(um))

as.list(c(5, 4, 3))

for (i in 2:length(n_seq)){
  index <- n_seq[i]
  if (n_seq[1] > index) {
    for (j in i+1:length(n_seq)){
      sec_index <- n_seq[j]
      if (n_seq[1] < sec_index){
        return(n_seq)
      } else {NULL}
    }
    
  } else {NULL}
}

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
  }
}
