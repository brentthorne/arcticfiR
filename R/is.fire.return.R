#' Check Fire Return Data
#'
#' Ensure that user data is suitable for use of the fire_return() function.
#'
#' @param x User's data
#'
#' @examples
#' my.df <- data.frame(c("a","b","c","d"),
#' c(1,"None","None","None"),
#' c("None",1,"None","None"),
#' c(1,"None","None",1),
#' c(1,"None",1,"None"),
#' c("None",1,"None",1),
#' c(1,"None","None","None"))
#'
#' colnames(my.df) <- c("ID","Year1","Year2","Year3","Year4","Year5","Year6")
#'
#' is.fire_return(my.df)
#'
#' @export


is.fire_return <- function(x){

  tst.fire.ID <- sapply(x[,1],is.factor)
  if (FALSE %in% tst.fire.ID){
    warning(paste("Coloumn number 1, named:", colnames(x)[1], "should be a as.factor()", sep = " "))
    return(FALSE)
  }

  tst.fire.years <- sapply(x[,2:ncol(x)],is.factor)
  if (FALSE %in% tst.fire.years){
    warning(paste("Coloumns for each year should be a as.factor()", sep = ""))
    return(FALSE)
  }
  return(TRUE)
}
