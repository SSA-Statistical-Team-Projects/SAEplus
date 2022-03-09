#### Writing loops in R

x <- c(100, 200, 300, 400, 500) #fake object with values 1-5 in it.

z <- character()

for (i in seq_along(x)){

  z[i] <- paste("Daylan", x[i], sep = "-")

}


#### write a function to apply a loop

attach_daylan <- function(numbers){

  result <- paste("Daylan", numbers, sep = "-")

  return(result)

}

y <- lapply(X = x,
            FUN = attach_daylan)

y <- unlist(y)

#### create a dataset and show how to run loop on each variable of said dataset
dt <- data.table(names = rep(c("siwei", "daylan", "muhsine", "rostand"), 25),
                 math = abs(rnorm(n = 100))*100,
                 english = abs(rnorm(n = 100))*100,
                 science = abs(rnorm(n = 100))*100)

dt[,c("math", "science", "english")][,apply(.SD, MARGIN = 2, FUN = add_zero)]

add_zero <- function(X){

  return(sprintf("%02f", X))

}

### if dt was a data.frame

dt <- as.data.frame(dt)

apply(dt[,c("math", "science", "english")], MARGIN = 2, FUN = add_zero)




