# Given n individuals, what is the probability that two people
# have the same birthday.
# 
# See Introduction to Mathematical Statistics, Hogg 2019,
# Example 1.3.3.
prob_bday <- function (n) {
  n <- as.integer(n)
  if (n < 2) {
    stop("input argument n must be greater than or equal to 2.")
  }
  # Inverse (complement) probability of like birthdays.
  prob_bday_inv <- 1
  for (i in seq(1, n - 1)) {
    prob_bday_inv <- prob_bday_inv * (365 - i) / 365
  }
  return (1 - prob_bday_inv)
}