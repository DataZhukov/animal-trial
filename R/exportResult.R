#' Export the results
#'
#' @param dataIn data.table
#' @param dataOut data.table
#' @param filename character filename (default =  "biggen")
#' @param fileLoc character file location (default = "~/Downloads")
#'
#' @return no return, only creates file in fileLoc folder
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom utils write.table
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
#' animalsOutTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[2]]
#' animalsInTrial <- assignPens(animalsInTrial,6)
#' animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Groen","Rood","Geel"))
#' animalsInTrial <- assignComp(animalsInTrial)
#' exportResult(animalsInTrial,animalsOutTrial)
exportResult <- function(dataIn, dataOut, filename = "biggen", fileLoc = "H:\\Downloads"){
  biggen <- bind_rows(dataIn,dataOut)
  write.table(biggen,file = paste(fileLoc,"\\",filename,".txt",sep=""),sep="|",dec=".",na="NA",row.names = F)
}

#' Give summary statistics
#'
#' @param data data.table
#'
#' @return no return, only prints summary
#' @export
#'
#' @import doBy
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
#' animalsInTrial <- assignPens(animalsInTrial,6)
#' animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Groen","Rood","Geel"))
#' animalsInTrial <- assignComp(animalsInTrial)
#' trialSummary(animalsInTrial)
trialSummary <- function(data){
  print(summaryBy(Speen_gew~Sex, data, FUN=c(min, mean, max, sd,length)))
  print(summaryBy(Speen_gew~Beh, data, FUN=c(min, mean, max, sd,length)))
  print(summaryBy(Speen_gew~Comp, data, FUN=c(min, mean, max, sd,length)))
}
