#' Assign pens to compartments
#'
#' @param data data.table
#' @param beh character vector
#'
#' @return data.table
#' @export
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
#' animalsInTrial <- assignPens(animalsInTrial,6,0.1)
#' animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Groen","Rood","Geel"))
#' animalsInTrial <- assignComp(animalsInTrial,c("Wit","Groen","Rood","Geel"))
assignComp <- function(data,beh){
  Sex <- Gew_klasse <- Beh <- Hok <- Comp <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  comp <- data[,list(unique(Sex),unique(Gew_klasse),unique(Beh)),Hok]
  names(comp) <- c("Hok","Sex","Gew_klasse","Beh")
  x <- nrow(comp) / 8

  compNewB<-data.frame()
  for (i in 1:length(beh)) {
    compTempB <- comp[Beh==beh[i]&Sex=="B"]

    count <- 0
    while (count == 0){
      compTempB$Comp <- sample(c(rep(1:x,(nrow(compTempB)/x))),replace = F)
      if (max(compTempB[,sum(duplicated(Gew_klasse)),Comp]$V1) == 0){
        count <- count + 1
      }
    }

    compNewB <- rbind(compNewB,compTempB)
  }

  compNewZ<-data.frame()
  for (i in 1:length(beh)) {
    compTempZ <- comp[Beh==beh[i]&Sex=="Z"]

    count <- 0
    while (count == 0){
      compTempZ$Comp <- sample(c(rep(1:x,(nrow(compTempZ)/x))),replace = F)
      if (max(compTempZ[,sum(duplicated(Gew_klasse)),Comp]$V1) == 0){
        count <- count + 1
      }
    }

    compNewZ <- rbind(compNewZ,compTempZ)
  }

  compNew <- rbind(compNewB,compNewZ)

  tempAIT <- data
  tempAIT$Comp <- tempAIT$Hok
  for (i in 1:nrow(tempAIT)){
    tempAIT$Comp[i] <- compNew$Comp[which(compNew$Hok==tempAIT$Comp[i])]
  }

  tempAIT <- tempAIT[order(Comp)]
  return(tempAIT)
}
