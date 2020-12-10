library(checkmate)
library(checklist)
library(testthat)

lag <- function(x, n = 1L) {
  checkmate::assert_atomic_vector(x)
  checkmate::assert_int(n, lower = 0, upper = length(x))
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}


# test input parameters
test_vec <- c(1, 2, 3, 4)
test_matrix <- matrix(1:4, ncol = 2)

expect_equal(lag(test_vec, 1), c(NA, 1, 2, 3))
expect_equal(lag(test_vec, 0), test_vec)
expect_error(lag(test_vec, -1))
expect_error(lag(test_matrix))
expect_error(lag(test_vec, 3.5))
expect_error(lag(test_vec, 5))



path_to_here <- paste0(here::here(), "/defensive-lag-sol.R")
checklist(path_to_here)

