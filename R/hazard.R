#' Hazard Rate Function
#'
#' For a given vector of times and a probability distribution function,
#' this function calculates the hazard rate values.
#'
#' @param t a numeric value as time
#' @param P a cumulative density function
#' @param D a density function
#' @param ... More parameters
#'
#' @return  A numeric vector of hazard rate values.
#'
#' @details The more details should be added.
#'
#' @examples
#' ## Standard normal distribution
#' t <- c(0.1, 0.01)
#' P <- pnorm
#' D <- dnorm
#' hazard(t, D, P)
#'
#' ## Uniform distribution with min=2.0 and max=2.5
#' t <- 2.2
#' P <- punif
#' D <- dunif
#' hazard(t, D, P, 2.0, 2.5)
#' @export
hazard <- function(t, D, P, ...) {
  P <- sapply(t, P, lower.tail = FALSE, ...)
  D <- sapply(t, D, ...)
  return(D / P)
}
