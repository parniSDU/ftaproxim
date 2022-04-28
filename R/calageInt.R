
#' Age Intensity Function
#'
#' This function returns a numeric value indicating
#' the pending time (in terms of time steps, delta) for a given state change of a proxel.
#'
#' @param state a string value
#' @param proxel a data frame containing a state, age intensity and the probability.
#' @param delta a numeric value as time step
#' @return a numeric value as the age intensity
#'
#'
#' @examples
#' proxel <- data.frame(State = "OK", ageInt = 0, Prob = 1)
#' state <- "OK"
#' delta <- 0.2
#' calageInt(state, proxel,delta)
#' @export
calageInt <- function(state, proxel,delta) {
  ifelse(proxel$State != state, ageint <- 0, ageint <- proxel$ageInt + delta)
  ageint
}
