#' Hazard Rate Function
#'
#' This function calculates ....
#'
#' @param t a numeric value as time
#' @param delta a numeric value  time step
#' @param P cumulative density function
#' @param D density function
#'
#' @return  A numeric vector of hazard rate values.
#'
#' @details The more details should be added.
#'
#' @examples
#' t <- 0.1
#' delta <- 0.01
#' P <- pnorm
#' D <- dnorm
#' h(t, delta, D,P)
#' @export
h<-function(t, delta, D,P,...){
  P<-sapply(t,P,lower=FALSE,...)
  D<-sapply(t,D,...)
  return(delta*D/P)
}


#Transition Matrix
TM<-function(G, dist,param, t, delta, states,...){
  ns<-nrow(G)
  if (length(dist)==1){
    P<-eval(parse(text=paste("p",dist,sep="")))
    D<-eval(parse(text=paste("d",dist,sep="")))
    par<-param[[1]]
    G[1,]<-G[1,]*h(t, delta, D, P, par)
    G[1,1]<-1-sum(G[1,], na.rm = TRUE)
  }else{
    for (i in 1:ns){
      P<-eval(parse(text=paste("p",dist[i],sep="")))
      D<-eval(parse(text=paste("d",dist[i],sep="")))
      par<-param[[i]]
      if(length(par)==1){G[i,]<-G[i,]*h(t, delta, D, P, par)
      }else{
        G[i,]<-G[i,]*h(t, delta, D, P, par[1], par[2])
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



#Calculate Age Intensity
calageInt<-function(state, proxel){
  ifelse (proxel$State!=state, ageint<-0,ageint<-proxel$ageInt+delta)
  ageint
}

# Calculate Probability
calProb<-function(BE, s,proxel,delta){
  t<-proxel$ageInt
  m<-TM(BE$G, BE$dist, BE$param, t,delta, BE$states)
  m[proxel$State,s]
}


#Next Level
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



#Calculate Proxel
ProxelBE<-function(BE, stat="F", TotalTime=20, delta=0.1, tol=0.000000001){

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
    indx<-which(Pro$State==stat)
    ins[i]<-sum(Pro$Prob[indx])
    proxel[[i]]<-Pro
    print(data.frame(i,Pro))
    i<-i+1
  } # i loop



  return(ins)

}


