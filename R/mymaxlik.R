#' @title Calculation of the Maximum Log Likelihood
#'
#' @param lfun The log function to be evaluated using the function
#' @param x The vector of values, or a sample
#' @param param The paramter of interest; e.g., mean, standard deviation
#' @param ... Any additional number of named or unnamed arguments
#'
#' @return A summary of slopes, indieces, the parameter of interest, and
#' a graphical output of where the x and param intersect
#' @importFrom graphics abline points axis
#' @export
#'
#' @examples
#' \dontrun{y = c(3,3,4,5)
#' logbin=function(x,param) log(dbinom(x,prob=param,size=20))
#' mymaxlik(x=y,param=seq(0,1,length=1000),lfun=logbin}
mymaxlik=function(lfun,x,param,...){
  ## For repeated sampling from same distribution
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z=outer(x,param,lfun)
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum) # A

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y))) # B
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  invisible(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
