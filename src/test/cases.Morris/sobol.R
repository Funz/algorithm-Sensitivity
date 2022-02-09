## This file should provide following objects, when loaded:
# f : function
# input.f : list of input dimensions, contains list of properties like lower & upper bounds of each dimensions
# output.f : list of output dimensions
# *.f : list of math properties. To be compared with algorithm results
# [print.f] : method to print/plot the function for information

f <- function(X) {
  a <- c(0, 1, 4.5, 9, 99, 99, 99, 99)
  y <- 1
  for (j in 1:8) {
    y <- y * (abs(4 * X[, j] - 2) + a[j]) / (1 + a[j])
  }
  matrix(y,nrow=nrow(X))
}

input.f = list(
    x1=list(min=0,max=1),
    x2=list(min=0,max=1),
    x3=list(min=0,max=1),
    x4=list(min=0,max=1),
    x5=list(min=0,max=1),
    x6=list(min=0,max=1),
    x7=list(min=0,max=1),
    x8=list(min=0,max=1)
)
output.f = "sobol"
mu.f = c(2.6129, 2.1610 ,0.4980, 0.4273 ,0.0333 ,0.0375 ,0.0189 ,0.0473)

test = function(algorithm_file) {
    results = run.algorithm(algorithm_file, options=NULL,fun=list(input=input.f,output=output.f,fun=f))
    library(testthat)
    # Replace following test by something suitable
    test_that("sobol mu",{expect_equal(as.numeric(results$mu),mu.f,tolerance = .0001)})
}

