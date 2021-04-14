#' Assign pens to compartments
#'
#' Assign pens to their compartment randomly but balanced over sex, treatment, and as much as possible over weight class
#'
#' @param data data.table with columns Sex (sex of piglets), Gew_klasse (weight class), Beh (Treatment), and Hok (Pen)
#' @param nPC numeric number of pens per compartment
#' @param etime numeric number of seconds to allow iterations before dropping the stringency for level of balance to strive for in weight class distribution over compartments
#'
#' @return data.table with added column Comp (compartment)
#' @export
#'
#' @import groupdata2
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
#' animalsInTrial <- assignPens(animalsInTrial,6)
#' animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Groen","Rood","Geel"))
#' animalsInTrial <- assignComp(animalsInTrial)
assignComp <- function(data,nPC=8, etime=10){
  Sex <- Gew_klasse <- Beh <- Hok <- Comp <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  comp <- data[,list(unique(Sex),unique(Gew_klasse),unique(Beh)),Hok]
  names(comp) <- c("Hok","Sex","Gew_klasse","Beh")
  x <- nrow(comp) / nPC #determine number of compartments
  beh <- unique(data$Beh)

  chi.p <- 0
  alpha <- 0.1
  mult <- 0
  start_time <- Sys.time()

while (chi.p < alpha) {

  compNewB <-data.frame()
  for (i in 1:length(beh)) {
    compTempB <- comp[Beh==beh[i]&Sex=="B"] #select pens for a treatment and barrow combination

    compTempB <- fold(compTempB, k = x,method="n_dist") #distribute those pens over the x compartments randomly but evenly

    compNewB <- rbind(compNewB,compTempB)
  }

  compNewZ<-data.frame()
  for (i in 1:length(beh)) {
    compTempZ <- comp[Beh==beh[i]&Sex=="Z"] #select pens for a treatment and gilt combination

    compTempZ <- fold(compTempZ, k = x,method="n_dist") #distribute those pens over the x compartments randomly but evenly

    compNewZ <- rbind(compNewZ,compTempZ)
  }

  compNew <- rbind(compNewB,compNewZ) #this is lookup table which shows which pens go to which compartment

  tempAIT <- data
  tempAIT$Comp <- tempAIT$Hok
  for (i in 1:nrow(tempAIT)){
    tempAIT$Comp[i] <- compNew$.folds[which(compNew$Hok==tempAIT$Comp[i])] #look up which pen corresponds to which compartment
  }

  chi <- chisq.test(table(tempAIT$Comp,tempAIT$Gew_klasse)) #test if weight classes are evenly distributed over compartments
  chi.p <- chi$p.value

  end_time <- Sys.time()
  dif <- difftime(end_time, start_time, units = "secs") #determine how long this iteration took in seconds
  if(dif > etime){ #if iteration took over 10 seconds
    alpha <- alpha / 2 #lower the p-value threshold to be achieved by half
    mult <- mult + 1 #add 1 to a counter to know how many iterations have past
    start_time <- Sys.time() #reset start_time to time the next iteration
    }
}

  if(mult > 0){warning(paste("Total elapsed time > ",etime * mult,"s, balancing criteria were made less stringent, weight classes might not be distributed as evenly as possible across compartments, consider manipulating manually or setting etime > ",etime,"s.",sep=""))}

  print("Table of compartments vs. weight classes:")
  print(table(tempAIT$Comp,tempAIT$Gew_klasse))

  tempAIT <- tempAIT[order(Comp)]
  return(tempAIT)
}
