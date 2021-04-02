#' Select trial animals
#'
#' Select which animals to include and which to exclude from the trial
#'
#' @param data data.table
#' @param nB numeric
#' @param nZ numeric
#' @param minWeight numeric
#' @param maxWeight numeric
#'
#' @return list
#' @export
#'
#' @import data.table
#'
#' @examples
#' selectTrialAnimals(biggen,72,72,5.4,9.5)
selectTrialAnimals <- function(data, nB, nZ, minWeight,maxWeight){
  Opm. <- Sex <- Speen_gew <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  buitenProef <- data[!is.na(Opm.)]
  tempBiggen <- data[is.na(Opm.)]

  tempBiggen <- tempBiggen[order(Sex,Speen_gew)]

  biggenB <- tempBiggen[Sex=="B"]
  biggenZ <- tempBiggen[Sex=="Z"]

  if (nrow(biggenB[Speen_gew >= minWeight & Speen_gew <= maxWeight]) >= nB){
    tempBuitenProefB <- biggenB[Speen_gew < minWeight & Speen_gew > maxWeight]
    biggenB <- biggenB[Speen_gew >= minWeight | Speen_gew <= maxWeight]
  }else{
    tempBuitenProefB <- biggenB[1: (nrow(biggenB) - nB)]
    biggenB <- biggenB[(nrow(biggenB) - nB + 1):nrow(biggenB)]
  }

  if (nrow(biggenZ[Speen_gew >= minWeight & Speen_gew <= maxWeight]) >= nZ){
    tempBuitenProefZ <- biggenZ[Speen_gew < minWeight | Speen_gew > maxWeight]
    biggenZ <- biggenZ[Speen_gew >= minWeight & Speen_gew <= maxWeight]
  }else{
    tempBuitenProefZ <- biggenZ[1: (nrow(biggenZ) - nZ)]
    biggenZ <- biggenZ[(nrow(biggenZ) - nZ + 1):nrow(biggenZ)]
  }

  while (nrow(biggenB) > nB| nrow(biggenZ) > nZ){
    if (nrow(biggenB) > nB){
      if (biggenB[,mean(Speen_gew)] > (biggenZ[,mean(Speen_gew)])){
        tempBuitenProefB <- rbind(tempBuitenProefB,biggenB[nrow(biggenB)])
        biggenB <- biggenB[-nrow(biggenB)]
      }
      if (biggenB[,mean(Speen_gew)] < (biggenZ[,mean(Speen_gew)])){
        tempBuitenProefB <- rbind(tempBuitenProefB,biggenB[1])
        biggenB <- biggenB[-1]
      }
    }

    if (nrow(biggenZ) > nZ){
      if (biggenZ[,mean(Speen_gew)] > (biggenB[,mean(Speen_gew)])){
        tempBuitenProefZ <- rbind(tempBuitenProefZ,biggenZ[nrow(biggenZ)])
        biggenZ <- biggenZ[-nrow(biggenZ)]
      }
      if (biggenZ[,mean(Speen_gew)] < (biggenB[,mean(Speen_gew)])){
        tempBuitenProefZ <- rbind(tempBuitenProefZ,biggenZ[1])
        biggenZ <- biggenZ[-1]
      }
    }
  }

  tempBuitenProef <- rbindlist(list(buitenProef,tempBuitenProefB, tempBuitenProefZ))
  tempBuitenProef <- tempBuitenProef[order(Sex,Speen_gew)]
  tempBiggen <- rbind(biggenB, biggenZ)

  print(paste("Mean weight B in trial = ",biggenB[,mean(Speen_gew)]))
  print(paste("Mean weight Z in trial = ",biggenZ[,mean(Speen_gew)]))

  return(list(animalsInTrial = tempBiggen, animalsOutTrial = tempBuitenProef))
}
