#' Average Annual Fire Return
#'
#' Calculate mean values of years between fire occurances
#'
#' @param x A data frame where is.fire_return_() returns "TRUE"
#'
#' @examples
#'
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
#' fire_return(x=my.df)
#'
#' @export



fire_return <- function(x){

  # set initial parameters
  fire_ret_mean <- matrix(nrow = nrow(x))
  t=1

  # parent loop for each location where data is collected
  for (t in 1:nrow(x)){
    site.list <- c()
    i=2
    list = 0

    # child loop for each year or column of severity values
    for (i in 2:ncol(x)){

      if (x[t,i] == "None"){
        if (i == ncol(x)){
          list <- list + 1
          site.list <- rbind(site.list, list)
        }else{list <- list + 1}
      }else{
        site.list <- rbind(site.list, list)
        list <- 0
      }
    }

    mean.tmp <- mean(site.list)
    fire_ret_mean[t] <- mean.tmp

  }

  # Merge fire data with calculated average fire return values for easy tidyverse useage stuff
  data.new <- cbind(x,fire_ret_mean)

}

