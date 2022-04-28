#' Transition Probability Function
#'
#' This function returns the transition probability for a given basic event and a proxel.
#'
#' @param BE a list containing states, transition matrix, distributions and their parameters for a basic event
#' @param state a string value
#' @param proxel a data frame containing a state, age intensity and a probability.
#' @param delta a numeric value as time step
#'
#' @return a numeric value between 0 and 1 as the transition probability
#'
#'
#' @examples
#' ## A repairable basic event with Uniform(2, 2.5) failure distribution function
#' ## and a fixed repair time of 0.3.
#' delta <- 0.2
#' BE <- list(
#'   states = c("OK", "F"),
#'   G = rbind(
#'     c(NA, 1),
#'     c(1, NA)
#'   ),
#'   dist = c("unif", "unif"),
#'   param = list(c(2, 2.5), c(0.3 - delta, 0.3 + delta))
#' )
#' state <- "OK"
#' proxel <- data.frame(State = "OK", ageInt = 0, Prob = 1)
#' calProb(BE, state, proxel, delta)
#' @export
calProb <- function(BE, state, proxel, delta) {
  t <- proxel$ageInt
  m <- TM(BE$G, BE$dist, BE$param, t, delta, BE$states)
  return(m[proxel$State, state])
}
