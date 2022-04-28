#' Fault Tree Unavailability
#'
#' This function returns a list where the first element is a data frame of unavailabilities
#' and the second element is the plot of the unavailabilities
#'
#' @param belist a list containing all the basic components of a fault tree
#' @param mcs a list of minimal cuts sets
#' @param totaltime an integer value for the total time
#' @param delta a numeric value as time step
#' @param tol a numeric value for the tolerance level
#'
#' @return  A list of unavailabilities for the basic events as well as the system,
#'          and their plots againts time steps
#'
#'
#' @examples
#' A<-list(
#'  states=c("OK","F"),
#'  G=rbind(c(NA,1),
#'          c(1,NA)),
#' dist=c("exp", "exp"),
#'  param=list(c(0.1), c(1))
#' )
#'
#' B<-list(
#'  states=c("OK","F"),
#'  G=rbind(c(NA,1),
#'          c(1,NA)),
#'  dist=c("exp", "exp"),
#'  param=list(c(0.01), c(2))
#' )
#'
#'
#' C<-list(
#'  states=c("OK","F"),
#'  G=rbind(c(NA,1),
#'          c(1,NA)),
#'  dist=c("exp", "weibull"),
#' param=list(c(0.1), c(5,2))
#' )
#'
#' D<-list(
#'  states=c("OK", "F"),
#'  G=rbind(c(NA, 1),
#'          c(1,NA)),
#'  dist=c("lnorm", "exp"),
#'  param=list(c(2, 0.1), 2)
#' )
#'
#' BElist<-list(A,B,C,D)
#' names(BElist)<-c("A","B","C","D")
#' MCS<-list(c("A", "C", "D"), c("B", "C", "D"))
#'
#' x<-FTUna(BElist, MCS, 10, 0.1, 1e-07)
#'
#' # Unavailabilities
#' x$Unavailability
#'
#' #Plots
#' x$Plot
#'
#' @export
FTUna<-function(belist,mcs,totaltime, delta, tol){

  una<-NULL

  for (i in names(belist)){
    BE<-belist[[i]]
    out<-ProxelBE(BE, "F", totaltime, delta, tol)
    una<-cbind(una, out)
  }

  colnames(una)<-names(belist)

  te<-0
  for (i in 1:length(mcs)){
    if (length(mcs[[i]])==1){p<-una[,mcs[[i]]]}else{
      p<-apply(una[,mcs[[i]]], 1, prod)
    }
    te<-p+te
  }
outUna<- as.data.frame(cbind(una, TE=te))

timeSteps<-totaltime/delta
df<-NULL
for (i in 1:ncol(outUna)){
  dfp<-data.frame(Unavailability=outUna[,i],Time=1:timeSteps,Event=names(outUna)[i])
  df<-rbind(df,dfp)
}

p <- ggplot(df, aes(x = Time))
p <- p + geom_line(aes(y = Unavailability, colour = Event ,linetype = Event), size= 0.75)
p <- p + theme(legend.position = "top")+ xlab(label = 'Time Steps')

out<-list(Unavailability=outUna, Plot=p)

  return(out)
}
