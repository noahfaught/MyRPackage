#' myttest
#'
#' @param x value
#' @param y value
#'
#' @return simple 2 sample test
#' @export
#'
#' @examples
myttest<-function(x,y){
set.seed(30); x=rnorm(15,mean=10,sd=7)
set.seed(40); y=rnorm(20,mean=12,sd=4)

var.test(y,x)
}
