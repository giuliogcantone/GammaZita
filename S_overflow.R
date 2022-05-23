EX <- function (x,y) {
      for (i in (1:length(x))) {
       x*y[(i+1):(i+length(x))]}
  }

EX(runif(5,0,1),runif(10,0,100)) -> prova

x = runif(5,0,1)
x = x/sum(x)
y = runif(10,0,100)

dat <- tibble(col1 = 1:5,
              col2 = data.frame(col3 = 1:5,
                                col4 = 5:9)
              )

dat <- tibble(col1 = 1:5, col2 = data.frame(col3 = 1:5, col4 = 5:9))

dat %>% unnest(col2)
