#' Assign treatments to pens
#'
#' Assign pens to treatments
#'
#' @param data data.table
#' @param beh character vector of treatment names
#'
#' @return data.table with added column Beh (Treatment)
#' @export
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
#' animalsInTrial <- assignPens(animalsInTrial,6)
#' animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Groen","Rood","Geel"))
assignTreatment <- function(data,beh){
  Sex <- Gew_klasse <- Hok <- Speen_gew <- Beh <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  treat <- data[base::order(Sex,Gew_klasse,Hok)] #sort data by sex, weight class and pen

  nWC <- base::length(base::unique(treat$Gew_klasse))
  nH <- as.numeric(base::summary(base::as.factor(treat$Hok))[1])

  x <- base::nrow(treat[Sex=="B"]) / (base::length(beh) * nWC)
  y <- base::nrow(treat[Sex=="Z"]) / (base::length(beh) * nWC)

  if(x %% nH != 0){stop("Number of barrows assigned to a treatment is not a multiple of the number of animals per pen, try running assignPens again with different number of weight classes.")}
  if(y %% nH != 0){stop("Number of gilts assigned to a treatment is not a multiple of the number of animals per pen, try running assignPens again with different number of weight classes.")}

  treat$Beh <- c(base::rep(base::sort(base::rep(beh,x)),nWC),base::rep(base::sort(base::rep(beh,y)),nWC))

  for (i in 1:base::length(beh)) {
    base::print(base::paste("Mean weight ",beh[i]," in trial = ",treat[Beh==beh[i],base::mean(Speen_gew)]))
  }

  return(treat)
}
