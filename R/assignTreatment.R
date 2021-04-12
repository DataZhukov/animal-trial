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

  nWC <- length(unique(treat$Gew_klasse))
  nH <- as.numeric(summary(as.factor(treat$Hok))[1])

  x <- nrow(treat[Sex=="B"]) / (length(beh) * nWC)
  y <- nrow(treat[Sex=="Z"]) / (length(beh) * nWC)

  if(x %% nH != 0){stop("Number of barrows assigned to a treatment is not a multiple of the number of animals per pen, try running assignPens again with different number of weight classes.")}
  if(y %% nH != 0){stop("Number of gilts assigned to a treatment is not a multiple of the number of animals per pen, try running assignPens again with different number of weight classes.")}

  treat$Beh <- c(rep(sort(rep(beh,x)),nWC),rep(sort(rep(beh,y)),nWC))

  for (i in 1:length(beh)) {
    print(paste("Mean weight ",beh[i]," in trial = ",treat[Beh==beh[i],mean(Speen_gew)]))
  }

  return(treat)
}
