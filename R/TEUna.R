#' The title of function
#'
#' This function returns a ....
#'
#' @param mcs a ...
#' @param una a ...
#'
#' @return  A numeric matrix of transition probabilities.
#'
#' @details The more details should be added.
#'
#' @examples
#' ...
#'
#' @export
TEUna <- function(mcs,una){
  te<-0
  for (i in 1:length(mcs)){
    if (length(mcs[[i]])==1){p<-una[,mcs[[i]]]}else{
      p<-apply(una[,mcs[[i]]], 1, prod)
    }
    te<-p+te
  }
  return(te)
}
