#' Put zootechnical data in suitable format
#'
#' @param data data.table of zootechnical data
#' @param n_fasenW numeric number of phases in the trial
#' @param vars character vector of variable names for zootechnical parameters in data
#' @param VARfasenames character vector of names to be used for these parameters in reshaped data.frame
#'
#' @return data.table with zootechnical parameters in long format over phases
#' @export
#'
#' @examples
#' groeidataLong <- makeLong(groeidata,2)
makeLong <- function(data, n_fasenW, vars = c("g","dg","dvo","vc"), VARfasenames = c("Weight","Daily_gain","Daily_feed_intake","Feed_conversion_ratio")){
  hok <- data[!is.na(data$dgtot),]

  faseW<-c(as.character(1:n_fasenW))

  varsW<-list()
  for(i in 1:length(vars))varsW[[i]]<-paste0(vars[i],"",1:n_fasenW)
  varsW<-c(varsW)

  longW<-reshape(hok,varying=varsW
                 ,timevar="faseW",times=faseW,v.names=VARfasenames,direction="long")
  longW
}
