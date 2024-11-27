#' @title Curved Graph
#'
#' @param x x
#' @param m mean
#' @param l standard deviation
#' @importFrom stats pnorm
#' @importFrom graphics curve
#' @importFrom stats dnorm
#' @importFrom graphics polygon
#'
#' @return A sophisticated curved plot
#'
#' @examples
#' mycurve(5,10,3)
#' @export
mycurve = function(x,m,l){
  curve(dnorm(x, mean = m, sd = l), xlim = c(m - 3 * l, m + 3 * l))

xcurve3=seq(m - 3 * l,x,length=1000)
ycurve3=dnorm(xcurve3,m,l)

polygon(c(m-3*l,xcurve3,x), c(0,ycurve3,0), col = "Cyan")

area4=pnorm(x, mean = m, sd = l)
area4=round(area4,4)
print(area4)

}
