#' Transition Probability Matrix
#'
#' This function returns a matrix of transition probabilities at a time point for
#' a given basic event with specified transition distribution functions.
#'
#' @param t a numeric value as time
#' @param delta a numeric value as time step
#' @param states a string vector of states' labels for the basic event
#' @param G a matrix of 1's, 0's and NA's. 1 and NA: transition is possible, 0: transition is not possible
#' @param dist a string vector of transition distribution functions
#' @param param a list of parameters of the transition distribution functions
#'
#' @return  A numeric matrix of transition probabilities.
#'
#' @details The more details should be added.
#'
#' @examples
#' ## failure distribution function Uniform(2, 2.5)
#' ## and a fixed repair time of 0.3
#' t <- 0.1
#' delta <- 0.2
#' states <- c("OK", "F")
#' G <- rbind(c(NA, 1), c(1, NA))
#' dist <- c("unif", "unif")
#' param <- list(c(2, 2.5), c(0.3 - delta, 0.3 + delta))
#' TM(G, dist, param, t, delta, states)
#'
#' ## failure distribution function exp(0.001)
#' ## and not repairable
#' t <- 0.1
#' delta <- 0.2
#' states <- c("OK", "F")
#' G <- rbind(c(NA, 1), c(0, 1))
#' dist <- c("exp")
#' param <- list(c(0.001))
#' TM(G, dist, param, t, delta, states)
#' @export
TM <- function(G, dist, param, t, delta, states, ...) {
  ns <- nrow(G)
  if (length(dist) == 1) {
    P <- eval(parse(text = paste("p", dist, sep = "")))
    D <- eval(parse(text = paste("d", dist, sep = "")))
    par <- param[[1]]
    G[1, ] <- G[1, ] * delta * hazard(t, D, P, par)
    G[1, 1] <- 1 - sum(G[1, ], na.rm = TRUE)
  } else {
    for (i in 1:ns) {
      P <- eval(parse(text = paste("p", dist[i], sep = "")))
      D <- eval(parse(text = paste("d", dist[i], sep = "")))
      par <- param[[i]]
      if (length(par) == 1) {
        G[i, ] <- G[i, ] * delta * hazard(t, D, P, par)
      } else {
        G[i, ] <- G[i, ] * delta * hazard(t, D, P, par[1], par[2])
      }
    }
    for (i in 1:ns) {
      G[i, i] <- 1 - sum(G[i, ], na.rm = TRUE)
    }
  }
  row.names(G) <- states
  colnames(G) <- states
  return(G)
}

