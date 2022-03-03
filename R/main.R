#' Hazard Rate Function
#'
#' For a given vector of times and a probability distribution function,
#' this function calculates the hazard rate values.
#'
#' @param t a numeric value as time
#' @param P cumulative density function
#' @param D density function
#'
#' @return  A numeric vector of hazard rate values.
#'
#' @details The more details should be added.
#'
#' @examples
#' ## Standard normal distribution
#' t <- c(0.1,0.01)
#' P <- pnorm
#' D <- dnorm
#' hazard(t, D,P)
#'
#' ## Uniform distribution with min=2.0 and max=2.5
#' t <- 2.2
#' P <- punif
#' D <- dunif
#' hazard(t, D, P, 2.0, 2.5)
#' @export
hazard<-function(t, D,P,...){
  P<-sapply(t,P,lower.tail = FALSE,...)
  D<-sapply(t,D,...)
  return(D/P)
}


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
#' states=c("OK","F")
#' G=rbind(c(NA,1),c(1,NA))
#' dist=c("unif", "unif")
#' param=list(c(2, 2.5), c(0.3-delta,0.3+delta))
#' TM(G, dist,param, t, delta, states)
#'
#' ## failure distribution function exp(0.001)
#' ## and not repairable
#' t <- 0.1
#' delta <- 0.2
#' states=c("OK","F")
#' G=rbind(c(NA,1),c(0,1))
#' dist=c("exp")
#' param=list(c(0.001))
#' TM(G, dist,param, t, delta, states)
#' @export
TM<-function(G, dist,param, t, delta, states,...){
  ns<-nrow(G)
  if (length(dist)==1){
    P<-eval(parse(text=paste("p",dist,sep="")))
    D<-eval(parse(text=paste("d",dist,sep="")))
    par<-param[[1]]
    G[1,]<-G[1,]*delta*hazard(t, D, P, par)
    G[1,1]<-1-sum(G[1,], na.rm = TRUE)
  }else{
    for (i in 1:ns){
      P<-eval(parse(text=paste("p",dist[i],sep="")))
      D<-eval(parse(text=paste("d",dist[i],sep="")))
      par<-param[[i]]
      if(length(par)==1){G[i,]<-G[i,]*delta*hazard(t, D, P, par)
      }else{
        G[i,]<-G[i,]*delta*hazard(t, D, P, par[1], par[2])
      }

    }
    for (i in 1:ns){
      G[i,i]<-1-sum(G[i,], na.rm = TRUE)
    }
  }
  row.names(G) <-states
  colnames(G)<-states

  G
}


#' Age Intensity Function
#'
#' This function returns a numeric value indicating
#' the pending time (in terms of time steps, delta) for a given state change of a proxel.
#'
#' @param state a string value
#' @param proxel a data frame containing a state, age intensity and the probability.
#'
#' @return a numeric value as the age intensity
#'
#' @details The more details should be added.
#'
#' @examples
#' proxel<-data.frame(State="OK",ageInt=0, Prob=1)
#' state<-"OK"
#' calageInt(state, proxel)
#' @export
calageInt<-function(state, proxel){
  ifelse (proxel$State!=state, ageint<-0,ageint<-proxel$ageInt+delta)
  ageint
}


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
#' @details The more details should be added.
#'
#' @examples
#' ## A repairable basic event with Uniform(2, 2.5) failure distribution function
#' ## and a fixed repair time of 0.3.
#' BE<-list(
#'states=c("OK","F"),
#'G=rbind(c(NA,1),
#'        c(1,NA)),
#'dist=c("unif", "unif"),
#'param=list(c(2, 2.5), c(0.3-delta,0.3+delta))
#')
#'state<-"OK"
#'proxel<-data.frame(State="OK",ageInt=0, Prob=1)
#'delta<-0.2
#'calProb(BE, state,proxel,delta)
#' @export
calProb<-function(BE, state, proxel, delta){
  t<-proxel$ageInt
  m<-TM(BE$G, BE$dist, BE$param, t,delta, BE$states)
  m[proxel$State,state]
}


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
#' BE<-list(
#' states=c("OK","IS","F"),
#' G=rbind(c(NA,1,0),
#'        c(0,NA,1),
#'        c(1,0,NA)),
#' dist=c("weibull", "unif","unif"),
#' param=list(c(2,3), c(0.5-delta,0.5+delta), c(0.1-delta,0.1+delta))
#' )
#'proxel<-data.frame(State="IS",ageInt=0.1, Prob=0.9)
#'delta<-0.2
#'nextLevel(BE, proxel,delta)
#' @export
nextLevel<-function(BE, proxel,delta){
  ns<-length(BE$states)
  if (ns==2){

    left<-data.frame(State="OK", ageInt=calageInt("OK",proxel),
                     Prob=proxel$Prob*calProb(BE,"OK",proxel,delta))
    right<-data.frame(State="F", ageInt=calageInt("F",proxel),
                      Prob=proxel$Prob*calProb(BE,"F",proxel,delta))
    return(rbind(left,right))
  }else{
    left<-data.frame(State="OK", ageInt=calageInt("OK",proxel),
                     Prob=proxel$Prob*calProb(BE,"OK",proxel,delta))
    mid<-data.frame(State="IS", ageInt=calageInt("IS",proxel),
                    Prob=proxel$Prob*calProb(BE,"IS",proxel,delta))
    right<-data.frame(State="F", ageInt=calageInt("F",proxel),
                      Prob=proxel$Prob*calProb(BE,"F",proxel,delta))
    return(rbind(left,mid,right))
  }
}




#' Instantaneous Unavailability/Reliability Vector
#'
#' This function calculates the isntantaneous unavailablity/reliabality values
#' of a basic event.
#'
#' @param BE a list containing states, transition matrix, distributions and their parameters for a basic event
#' @param state a string value for the state
#' @param TotalTime an integer value for the total time
#' @param delta a numeric value as time step
#' @param tol a numeric value for the tolerance level
#'
#' @return a numeric vector of instantaneous unavailabilities/reliabilities if the state is F/OK
#'
#' @details The more details should be added.
#'
#' @examples
#' library(dplyr)
#' library(plyr)
#' ## A multi-state basic event with Weibull(2, 3) transition distribution function from working (OK) to
#' ## an Intermediate State (IS), a fixed time of 0.5 transtion from IS to failure (F),
#' ## and a fixed repair time of 0.1 (transition from state F to state OK).
#' BE<-list(
#' states=c("OK","IS","F"),
#' G=rbind(c(NA,1,0),
#'        c(0,NA,1),
#'        c(1,0,NA)),
#' dist=c("weibull", "unif","unif"),
#' param=list(c(2,3), c(0.5-delta,0.5+delta), c(0.1-delta,0.1+delta))
#' )
#'unavailability<-ProxelBE(BE, state="F", TotalTime=20, delta=0.1, tol=0.000000001)
#'plot(unavailability, type="l")
#' @export
ProxelBE<-function(BE, state, TotalTime, delta, tol){

  ns<-length(BE$states)

  total<-TotalTime
  steps<-total/delta
  ins<-numeric(steps)
  ins[1]<-0
  proxel<-list()
  proxel[[1]]<-data.frame(State="OK",ageInt=0, Prob=1)

  funs<-function(BE,s, delta){
    pL<-nextLevel(BE, prox[s,],delta)
    ind<-which(pL$Prob<tol)
    if(length(ind)!=0){pL<-pL[-ind,]}
    pL
  }


  ffuns = function(s)
    funs(BE, s,delta)
  i<-2
  while(i<=(steps)){
    Pro<-NULL
    prox<-proxel[[i-1]]
    pL<-ldply(lapply(1:nrow(prox),ffuns),data.frame)
    Pro<-aggregate(pL['Prob'], by=pL[c('State',"ageInt")], sum)
    indx<-which(Pro$State==state)
    ins[i]<-sum(Pro$Prob[indx])
    proxel[[i]]<-Pro
    print(data.frame(i,Pro))
    i<-i+1
  } # i loop



  return(ins)

}
