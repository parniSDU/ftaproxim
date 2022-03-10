#' Proxels of the next time step
#'
#' For a given basic event and a proxel, this function
#' calculates all the possible proxels for the next time step.
#'
#' @param BE a list containing states, transition matrix, distributions and their parameters for a basic event
#' @param proxel a data frame containing a state, age intensity and a probability.
#' @param delta a numeric value as time step
#'
#' @return a data frame where each row is a proxel
#'
#' @details The more details should be added.
#'
#' @examples
#' ## A multi-state basic event with Weibull(2, 3) transition distribution function from working (OK) to
#' ## an Intermediate State (IS), a fixed time of 0.5 transtion from IS to failure (F),
#' ## and a fixed repair time of 0.1 (transition from state F to state OK).
#' BE <- list(
#'   states = c("OK", "IS", "F"),
#'   G = rbind(
#'     c(NA, 1, 0),
#'     c(0, NA, 1),
#'     c(1, 0, NA)
#'   ),
#'   dist = c("weibull", "unif", "unif"),
#'   param = list(c(2, 3), c(0.5 - delta, 0.5 + delta), c(0.1 - delta, 0.1 + delta))
#' )
#' proxel <- data.frame(State = "IS", ageInt = 0.1, Prob = 0.9)
#' delta <- 0.2
#' nextLevel(BE, proxel, delta)
#'
#' @export
nextLevel <- function(BE, proxel, delta) {
  ns <- length(BE$states)
  if (ns == 2) {
    left <- data.frame(
      State = "OK", ageInt = calageInt("OK", proxel,delta),
      Prob = proxel$Prob * calProb(BE, "OK", proxel, delta)
    )
    right <- data.frame(
      State = "F", ageInt = calageInt("F", proxel,delta),
      Prob = proxel$Prob * calProb(BE, "F", proxel, delta)
    )
    return(rbind(left, right))
  } else {
    left <- data.frame(
      State = "OK", ageInt = calageInt("OK", proxel,delta),
      Prob = proxel$Prob * calProb(BE, "OK", proxel, delta)
    )
    mid <- data.frame(
      State = "IS", ageInt = calageInt("IS", proxel,delta),
      Prob = proxel$Prob * calProb(BE, "IS", proxel, delta)
    )
    right <- data.frame(
      State = "F", ageInt = calageInt("F", proxel,delta),
      Prob = proxel$Prob * calProb(BE, "F", proxel, delta)
    )
    return(rbind(left, mid, right))
  }
}



