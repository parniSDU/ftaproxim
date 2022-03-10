proxel <- data.frame(State = "OK", ageInt = 0, Prob = 1)
state <- "OK"
delta <- 0.2
calageInt(state, proxel,delta)
# ------------------
BE <- list(
  states = c("OK", "F"),
  G = rbind(
    c(NA, 1),
    c(1, NA)
  ),
  dist = c("unif", "unif"),
  param = list(c(2, 2.5), c(0.3 - delta, 0.3 + delta))
)
state <- "OK"
proxel <- data.frame(State = "OK", ageInt = 0, Prob = 1)
delta <- 0.2
calProb(BE, state, proxel, delta)

# ---------------------------

t <- c(0.1, 0.01)
P <- pnorm
D <- dnorm
hazard(t, D, P)

## Uniform distribution with min=2.0 and max=2.5
t <- 2.2
P <- punif
D <- dunif
hazard(t, D, P, 2.0, 2.5)


# -----------------------------------------
BE <- list(
  states = c("OK", "IS", "F"),
  G = rbind(
    c(NA, 1, 0),
    c(0, NA, 1),
    c(1, 0, NA)
  ),
  dist = c("weibull", "unif", "unif"),
  param = list(c(2, 3), c(0.5 - delta, 0.5 + delta), c(0.1 - delta, 0.1 + delta))
)
proxel <- data.frame(State = "IS", ageInt = 0.1, Prob = 0.9)
delta <- 0.2
nextLevel(BE, proxel, delta)





