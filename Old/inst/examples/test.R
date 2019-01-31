# Example for trying out debugR.
# usage: debugR("test.R")

f <- function() {
   sum <- 0
   sum2 <- 0
   for (i in 1:3) {
      sum <- sum + i
      sum2 <- sum2 + i*i
   }
   c(sum,sum2)
}

