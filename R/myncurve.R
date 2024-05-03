#' Mycurve function
#'
#' @param a a numeric vector.
#' @param mu a numeric vector.
#' @param sigma a numeric vector.
#'
#' @return plot
#' @importFrom graphics curve
#' @importFrom stats dnorm
#' @importFrom graphics polygon
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' myncurve(5,10,3)
myncurve = function(a, mu, sigma){
  #function from lab 8
  x=NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), ylab = "Normal Density")

  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(mu-3*sigma,a,length=1000)
  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  # Area
  area=pnorm(a,mean=mu,sd=sigma)
  area=round(area,4)
  list(mu = mu, sigma = sigma,area=area)
  # needs 3 components

}
