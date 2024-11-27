#' myread
#'
#' @param csv a csv file
#' @importFrom utils read.table
#' @global dird
#' @return A csv file
#' @export
#'
#' @examples
#' fin.df=myread("FINTUBES.csv")
myread = function(csv){
  dird <- read.table(file, header = TRUE)  # Defined within the function
  return(dird)
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
