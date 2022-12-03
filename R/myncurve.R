#' myncurve
#'
#' @param mu mean
#' @param sigma  standard deviation
#' @param a lower tail probability starting point
#'
#' @return prints out lower tail
#' @export
#'
#' @examples
#' myncurve(mu=7,sigma=4,a=5)
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim=c(mu-3*sigma, mu+3*sigma))

  xcurve= seq(mu-4*sigma, a, length=1000)
  ycurve= dnorm(xcurve, mu, sigma)

  polygon(c(0,xcurve,a), c(0,ycurve,0), col= "red")

  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  prob
}
