#' Assign pens to compartments
#'
#' @param data data.table
#' @param beh character vector
#' @param nPC numeric
#'
#' @return data.table
#' @export
#'
#' @import groupdata2
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
#' animalsInTrial <- assignPens(animalsInTrial,6,0.1)
#' animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Groen","Rood","Geel"))
#' animalsInTrial <- assignComp(animalsInTrial,c("Wit","Groen","Rood","Geel"))
assignComp <- function(data,beh,nPC=8){
  Sex <- Gew_klasse <- Beh <- Hok <- Comp <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  comp <- data[,list(unique(Sex),unique(Gew_klasse),unique(Beh)),Hok]
  names(comp) <- c("Hok","Sex","Gew_klasse","Beh")
  x <- nrow(comp) / nPC

  chi.p <- 0
while (chi.p<0.1) { compNewB<-data.frame()
  for (i in 1:length(beh)) {
    compTempB <- comp[Beh==beh[i]&Sex=="B"]

    compTempB <- fold(compTempB, k = x,method="n_dist")

    compNewB <- rbind(compNewB,compTempB)
  }

  compNewZ<-data.frame()
  for (i in 1:length(beh)) {
    compTempZ <- comp[Beh==beh[i]&Sex=="Z"]

    compTempZ <- fold(compTempZ, k = x,method="n_dist")

    compNewZ <- rbind(compNewZ,compTempZ)
  }

  compNew <- rbind(compNewB,compNewZ)

  tempAIT <- data
  tempAIT$Comp <- tempAIT$Hok
  for (i in 1:nrow(tempAIT)){
    tempAIT$Comp[i] <- compNew$.folds[which(compNew$Hok==tempAIT$Comp[i])]
  }

  chi <- chisq.test(table(tempAIT$Comp,tempAIT$Gew_klasse))
  chi.p <- chi$p.value
}

  tempAIT <- tempAIT[order(Comp)]
  return(tempAIT)
}
