#' @export
TEUna<-function(mcs,una){
  te<-0
  for (i in 1:length(mcs)){
    if (length(mcs[[i]])==1){p<-una[,mcs[[i]]]}else{
      p<-apply(una[,mcs[[i]]], 1, prod)
    }
    te<-p+te
  }
  return(te)
}
