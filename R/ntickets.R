#' Title
#'
#' @param N the number of seats in the flight
#' @param gamma the probability a plane will be truly overbooked
#' @param p the probability of a "show"
#'
#' @return a named list containing nd, nc, N, p and gamma -
#' where nd is calculated using the discrete distribution
#' and nc is the same calculated with normal approximation.
#' @importFrom stats pbinom
#' @importFrom stats pnorm
#' @importFrom stats optimize
#' @importFrom graphics curve
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N, gamma, p) {
  x=NULL
  # sequence of n values
  nvalues <- seq(N, floor(N+0.1* N), by = 1)

  #objective function for discrete distribution
  objd <- 1 - gamma - pbinom(q = N, size = nvalues, prob = p)

  #objective function for continuous distribution
  objc <- function(nvalues) {
    1 - gamma - pnorm(N + 0.5, nvalues * p, sqrt(nvalues * p * (1 - p)))
  }

  #number of tickets sold for discrete
  nd <- nvalues[which.min(abs(objd))]

  #number of tickets sold for continuous
  nc <- optimize(f = function(x) abs(objc(x)), interval = nvalues)$minimum

  # discrete graph
  plot(x=nvalues, y = objd, type = 'b', main = paste("Objective vs. nvalues to find optimal tickets sold\nvalues(nd =", nd, ", gamma =", gamma, ", N =", N, "discrete)"), ylab = "Objective", xlab="n", bg = "blue",pch=21)
  abline(v = nd, h = 0, col = "red")

  # continuous graph
  curve(objc(x), N, floor(N + 0.1 * N), main = paste("Objective vs. nvalues to find optimal tickets sold\nvalues(nc =", nc, ", gamma =", gamma, ", N =", N, "continuous)"), ylab = "Objective", xlab = "n")
  abline(v = nc, h = 0, col = "blue")

  #print list
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
