#' @Title myread
#'
#' @param csv 
#'
#' @return A csv file
#' @export
#'
#' @examples
#' \dontrun{fin.df=myread("FINTUBES.csv")}
myread = function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}