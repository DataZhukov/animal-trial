#' Create weight classes
#'
#' @param data data.table
#'
#' @return data.table
createWeightClass <- function(data){
  Sex <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  x <- nrow(data[Sex=="B"]) / 3
  y <- nrow(data[Sex=="Z"]) / 3

  data$Gew_klasse <- as.factor(c(rep("L",x),rep("M",x),rep("Z",x),rep("L",x),rep("M",x),rep("Z",x)))

  return(data)
}

#' Function to optimize for switchInPen function
#'
#' @param x numeric pen number
#' @param data data.table
#' @param ... parameters to be applied to other functions
#'
#' @return numeric
#'
#' @import stats
OF <- function(x, data, ...){
  sd(tapply(data$Speen_gew, x, mean))
}

#' Select siblings to switch
#'
#' @param x numeric vector of pens
#' @param data data.table
#' @param sowids character vector
#'
#' @return numeric vector
nb <- function(x, data, sowids) {
  s <- sowids[sample(length(sowids), 1)]
  ij <- sample(which(data$Zeugnr == s))[1:2]
  x[ij] <- x[rev(ij)]
  x
}

#' Avoid siblings in pens
#'
#' @param data data.table
#' @param cutoff numeric
#'
#' @return data.table
#'
#' @import NMOF
switchInPen <- function(data, cutoff){
  Sex <- Gew_klasse <- Hok <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  tempPens <- data[order(Sex,Gew_klasse,Hok)]

  x <- tempPens$Hok

  OF(x, data= tempPens)

  sowids <- names(table(tempPens$Zeugnr)[table(tempPens$Zeugnr) > 1])

  sol.ls <- LSopt(OF, list(x0 = tempPens$Hok,
                           neighbour = nb,
                           nI = 1000),
                  data = tempPens, sowids = sowids)

  tempPens$Hok <- sol.ls$xbest
  return(tempPens)
}

#' Assign piglets to pens
#'
#' @param data data.table
#' @param nH numeric
#' @param cutoff numeric
#'
#' @return data.table
#' @export
#'
#' @import stats
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
#' animalsInTrial <- assignPens(animalsInTrial,6,0.1)
assignPens <- function(data, nH, cutoff){
  Sex <- Gew_klasse <- Zeugnr <- Rand <-Speen_gew <- Hok <- V1 <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  x <- nrow(data[Sex=="B"])  / (3 * nH)
  y <- nrow(data[Sex=="Z"])  / (3 * nH)
  tempAIT <- data

  tempAIT <- createWeightClass(tempAIT)

  tempAIT$Rand <- runif(nrow(tempAIT),0,1)

  tempAIT <- tempAIT[order(Sex,Gew_klasse,Zeugnr,Rand)]

  tempAIT$Hok <- c(rep(1:x,nH), rep((x+1):(2*x),nH), rep((2*x+1):(3*x),nH), rep((3*x+1):(3*x+y),nH), rep((3*x+1+y):(3*x+2*y),nH), rep((3*x+1+2*y):(3*x+3*y),nH))

  print("Mean weight per pen before:")
  print(tempAIT[,mean(Speen_gew),Hok])

  if (nrow(tempAIT[,sum(duplicated(Zeugnr)),Hok][V1 != 0]) != 0){
    print("The following pens contain siblings:")
    print(tempAIT[,sum(duplicated(Zeugnr)),Hok][V1 != 0])
  }

  for (i in seq(1,(x*3),by=4)){
    tempAIT <- tempAIT[order(Sex,Gew_klasse,Hok)]
    partPens <- tempAIT[Hok%in%c(i:(i+x-1))]

    partPens <- switchInPen(partPens, cutoff)

    tempAIT[Hok%in%c(i:(i+x-1))] <- partPens
  }

  for (i in seq(x*3+1,(x*3+3*y),by=4)){
    tempAIT <- tempAIT[order(Sex,Gew_klasse,Hok)]
    partPens <- tempAIT[Hok%in%c(i:(i+x-1))]

    partPens <- switchInPen(partPens, cutoff)

    tempAIT[Hok%in%c(i:(i+x-1))] <- partPens
  }

  tempAIT <- tempAIT[order(Sex,Gew_klasse,Hok)]
  return(tempAIT)
}
