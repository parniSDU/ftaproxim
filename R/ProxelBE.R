#' Instantaneous Unavailability Vector
#'
#' This function calculates the isntantaneous unavailablity/reliabality values
#' of a basic event.
#'
#' @param BE a list containing states, transition matrix, distributions and their parameters for a basic event
#' @param state a string value for the state
#' @param totaltime an integer value for the total time
#' @param delta a numeric value as time step
#' @param tol a numeric value for the tolerance level
#'
#' @return a numeric vector of instantaneous unavailabilities when the state is F
#'
#' @details For a multistate event, if the state is IS this function returns a vector of
#'         instantaneous probabilities of being in the intermediate state
#'
#' @examples
#' require(plyr)
#' require(dplyr)
#'
#' #A multi-state basic event with Weibull(2, 3) transition distribution function
#' #from working (OK) to an Intermediate State (IS), a fixed time of 0.5 transtion
#' #from IS to failure (F), and a fixed repair time of 0.1 (transition from state F to state OK).
#' delta <- 0.1
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
#' probIS <- ProxelBE(BE, state = "IS", totaltime = 20, delta = 0.1, tol = 0.000000001)
#' plot(probIS, type = "l")
#' @importFrom plyr ldply
#' @importFrom stats aggregate
#' @export
ProxelBE <- function(BE, state, totaltime, delta, tol) {
  ns <- length(BE$states)

  steps <- totaltime / delta
  ins <- numeric(steps)
  ins[1] <- 0
  proxel <- list()
  proxel[[1]] <- data.frame(State = "OK", ageInt = 0, Prob = 1)

  funs <- function(BE, s, delta) {
    pL <- nextLevel(BE, prox[s, ], delta)
    ind <- which(pL$Prob < tol)
    if (length(ind) != 0) {
      pL <- pL[-ind, ]
    }
    return(pL)
  }
  ffuns <- function(s) {
    return(funs(BE, s, delta))
  }
  i <- 2
  while (i <= (steps)) {
    Pro <- NULL
    prox <- proxel[[i - 1]]
    pL <- ldply(lapply(1:nrow(prox), ffuns), data.frame)
    Pro <- aggregate(pL["Prob"], by = pL[c("State", "ageInt")], sum)
    indx <- which(Pro$State == state)
    ins[i] <- sum(Pro$Prob[indx])
    proxel[[i]] <- Pro
    print(data.frame(i, Pro))
    i <- i + 1
  } # i loop
  return(ins)
}
