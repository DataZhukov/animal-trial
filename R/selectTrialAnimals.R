#' Select trial animals
#'
#' Select which animals to include and which to exclude from the trial
#'
#' @param data data.table with columns Opm. (comments), Sex (sex of piglets), and Speen_gew (weaning weights of piglets)
#' @param nB numeric number of barrows to be selected
#' @param nZ numeric number of gilts to be selected
#' @param minWeight numeric minimum weight of piglets to be selected
#' @param maxWeight numeric maximum weight of piglets to be selected
#'
#' @details If not enough piglets meet the in trial criteria the function removes the lowest weight animals until it is left with the required amount.
#' The function tries to select piglets in such a way that the Coefficient of Variation op weaning weights is minimized
#'
#' @return list with elements animalsInTrial (piglets selected for trial) and animalsOutTrial (piglets left out of trial)
#' @export
#'
#' @import data.table
#' @import goeveg
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
selectTrialAnimals <- function(data, nB, nZ, minWeight,maxWeight){
  Opm. <- Sex <- Speen_gew <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  buitenProef <- data[!base::is.na(Opm.)] #All piglets with comments to be left out of trial
  tempBiggen <- data[base::is.na(Opm.)] #For now keep all piglets without comments in the trial

  tempBiggen <- tempBiggen[base::order(Sex,Speen_gew)] #sort piglets by sex and weaning weight

  biggenB <- tempBiggen[Sex=="B"] #select only barrows
  biggenZ <- tempBiggen[Sex=="Z"] #select only gilts

  #for barrows:
  if (base::nrow(biggenB[Speen_gew >= minWeight & Speen_gew <= maxWeight]) >= nB){ #if enough piglets meet the weight criteria
    tempBuitenProefB <- biggenB[Speen_gew < minWeight & Speen_gew > maxWeight] #move all piglets that don't meet weight criteria out of trial
    biggenB <- biggenB[Speen_gew >= minWeight | Speen_gew <= maxWeight] #move all piglets that do meet weight criteria in the trial
  }else{ #if there is a shortage of piglets that meet the weight criteria
    if (nrow(biggenB) > nB){
      tempBuitenProefB <- biggenB[1: (base::nrow(biggenB) - nB)] #move the lowest weight piglets to out of trial until we have the required number of piglets
      biggenB <- biggenB[(base::nrow(biggenB) - nB + 1):base::nrow(biggenB)] #move the remaining piglets in trial
    }else{
      print(base::paste("Not enough barrows, number of selected barrows (",base::nrow(biggenB),") is lower than desired (",nB,")",sep=""))
      tempBuitenProefB <-biggenB[0]
      }
  }

  #for gilts:
  if (base::nrow(biggenZ[Speen_gew >= minWeight & Speen_gew <= maxWeight]) >= nZ){ #if enough piglets meet the weight criteria
    tempBuitenProefZ <- biggenZ[Speen_gew < minWeight | Speen_gew > maxWeight] #move all piglets that don't meet weight criteria out of trial
    biggenZ <- biggenZ[Speen_gew >= minWeight & Speen_gew <= maxWeight] #move all piglets that do meet weight criteria in the trial
  }else{ #if there is a shortage of piglets that meet the weight criteria
    if (nrow(biggenZ) > nZ){
      tempBuitenProefZ <- biggenZ[1: (base::nrow(biggenZ) - nZ)] #move the lowest weight piglets to out of trial until we have the required number of piglets
      biggenZ <- biggenZ[(base::nrow(biggenZ) - nZ + 1):base::nrow(biggenZ)] #move the remaining piglets in trial
    }else{
      print(base::paste("Not enough gilst, number of selected gilts (",base::nrow(biggenZ),") is lower than desired (",nZ,")",sep=""))
      tempBuitenProefZ <-biggenZ[0]
    }
  }

  while (base::nrow(biggenB) > nB| base::nrow(biggenZ) > nZ){ #repeat until we have the required number of barrows and gilts
    #for barrows:
    if (base::nrow(biggenB) > nB){ #if the number of barrows exceeds the required amount
      if (biggenB[-1,goeveg::cv(Speen_gew)] >= biggenB[-base::nrow(biggenB),goeveg::cv(Speen_gew)]){ #if the CV is lower with heaviest removed compared to with lightest removed
        tempBuitenProefB <- base::rbind(tempBuitenProefB,biggenB[base::nrow(biggenB)]) #move the heaviest male piglet out of trial
        biggenB <- biggenB[-base::nrow(biggenB)] #remove the heaviest male piglet from the in trial piglets
      }
      else {
        tempBuitenProefB <- base::rbind(tempBuitenProefB,biggenB[1]) #move the lightest male piglet out of trial
        biggenB <- biggenB[-1] #remove the lightest male piglet from the in trial piglets
      }
    }

    #for gilts:
    if (base::nrow(biggenZ) > nZ){ #if the number of gilts exceeds the required amount
      if (biggenZ[-1,goeveg::cv(Speen_gew)] >= biggenZ[-base::nrow(biggenZ),goeveg::cv(Speen_gew)]){ #if the CV is lower with heaviest removed compared to with lightest removed
        tempBuitenProefZ <- base::rbind(tempBuitenProefZ,biggenZ[base::nrow(biggenZ)]) #move the heaviest female piglet out of trial
        biggenZ <- biggenZ[-base::nrow(biggenZ)] #remove the heaviest female piglet from the in trial piglets
      }
      else {
        tempBuitenProefZ <- base::rbind(tempBuitenProefZ,biggenZ[1]) #move the lightest female piglet out of trial
        biggenZ <- biggenZ[-1] #remove the lightest female piglet from the in trial piglets
      }
    }
  }

  tempBuitenProef <- data.table::rbindlist(list(buitenProef,tempBuitenProefB, tempBuitenProefZ)) #paste all out of trial sets together
  tempBuitenProef <- tempBuitenProef[base::order(Sex,Speen_gew)] #sort the out of trial set by sex and weaning weight
  tempBiggen <- base::rbind(biggenB, biggenZ) #paste both in trial sets together

  print(paste("Mean weight B in trial = ",biggenB[,base::mean(Speen_gew)])) #print the mean weaning weight of the barrows in trial
  print(paste("Mean weight Z in trial = ",biggenZ[,base::mean(Speen_gew)])) #print the mean weaning weight of the gilts in trial
  print(paste("Coefficient of Variation B in trial = ",biggenB[,goeveg::cv(Speen_gew)])) #print the CV weaning weight of the barrows in trial
  print(paste("Coefficient of Variation Z in trial = ",biggenZ[,goeveg::cv(Speen_gew)])) #print the CV weaning weight of the gilts in trial

  return(base::list(animalsInTrial = tempBiggen, animalsOutTrial = tempBuitenProef)) #return a list with the in trial piglets and the out of trial piglets
}
