#' Assign treatments to pens
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
assignTreatment <- function(data,beh){
  Sex <- Gew_klasse <- Hok <- Speen_gew <- Beh <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  treat <- data[order(Sex,Gew_klasse,Hok)]
  x <- nrow(treat[Sex=="B" & Gew_klasse=="L"]) / length(beh)
  y <- nrow(treat[Sex=="Z" & Gew_klasse=="L"]) / length(beh)

  treat$Beh <- c(rep(sort(rep(beh,x)),3),rep(sort(rep(beh,y)),3))

  for (i in 1:length(beh)) {
    print(paste("Mean weight ",beh[i]," in trial = ",treat[Beh==beh[i],mean(Speen_gew)]))
  }

  return(treat)
}
