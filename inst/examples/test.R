# Example for trying out debugR.
# usage: debugR("test.R")

f <- function() {
   sum <- 0
   for (i in 1:3) {
      sum <- sum + 1
   }
   sum
}

