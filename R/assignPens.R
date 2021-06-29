#' Create weight classes
#'
#'Assigns weight class to each piglet
#'
#' @param data data.table
#' @param nWC numeric number of weight classes to assign can be 1, 2, 3 or 4
#'
#' @return data.table
createWeightClass <- function(data, nWC=3){
  Sex <- Speen_gew <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html

  if(!(nWC %in% c(1,2,3,4))){base::stop("Can only have 1, 2, 3 or 4 weight classes")}

  data <- data[base::order(Sex,Speen_gew)] #Sort piglets by sex and weaning weight

  x <- base::nrow(data[Sex=="B"]) / nWC #Size of each weight class for barrows
  y <- base::nrow(data[Sex=="Z"]) / nWC #Size of each weight class for gilts

  #Assign weight class to each piglet
  if(nWC==1){data$Gew_klasse <- base::as.factor(c(base::rep("M",x),base::rep("M",x)))}
  if(nWC==2){data$Gew_klasse <- base::as.factor(c(base::rep("L",x),base::rep("Z",x),base::rep("L",x),base::rep("Z",x)))}
  if(nWC==3){data$Gew_klasse <- base::as.factor(c(base::rep("L",x),base::rep("M",x),base::rep("Z",x),base::rep("L",x),base::rep("M",x),base::rep("Z",x)))}
  if(nWC==4){data$Gew_klasse <- base::as.factor(c(base::rep("L",x),base::rep("ML",x),base::rep("MZ",x),base::rep("Z",x),base::rep("L",x),base::rep("ML",x),base::rep("MZ",x),base::rep("Z",x)))}

  return(data)
}

#' Function to optimize for switchInPen function
#'
#' Calculates standard deviation of average weight per pen
#'
#' @param x numeric vector of pen numbers
#' @param data data.table with column Speen_gew (weaning weights of piglets)
#' @param ... parameters to be applied to sd and tapply
#'
#' @return numeric
#'
#' @import stats
OF <- function(x, data, ...){
  stats::sd(base::tapply(data$Speen_gew, x, base::mean))
}

#' Select siblings to switch
#'
#' Selects and switches siblings randomly
#'
#' @param x numeric vector of pens
#' @param data data.table with column Zeugnr (sow ID)
#' @param sowids character vector of sow ID's that appear more than once (i.e. siblings)
#'
#' @return numeric vector
nb <- function(x, data, sowids) {
  s <- sowids[base::sample(base::length(sowids), 1)] #select random sow ID
  ij <- base::sample(base::which(data$Zeugnr == s))[1:2] #select sibling of this sow together with individual sow
  if(data$Sex[ij[1]] == data$Sex[ij[2]]){
  x[ij] <- x[base::rev(ij)] #switch pens for these siblings
  x #return pen vector with switch
  }else{x}
}

#' Avoid siblings in pens
#'
#' Switches siblings in same pen to other pen while keeping standard deviation in average weight per pen to a minimum
#'
#' @param data data.table
#'
#' @return data.table
#'
#' @import NMOF
switchInPen <- function(data){
  Sex <- Gew_klasse <- Hok <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  tempPens <- data[base::order(Sex,Gew_klasse,Hok)]

  sowids <- base::names(base::table(tempPens$Zeugnr)[base::table(tempPens$Zeugnr) > 1]) #select sow ID's that appear multiple times

  sol.ls <- NMOF::LSopt(OF, base::list(x0 = tempPens$Hok,
                           neighbour = nb,
                           nI = 1000),
                  data = tempPens, sowids = sowids) #use standard deviation in average weight per pen as optimizing function in LSopt function while switching siblings between pens, keep optimal solution from 1000 iterations

  tempPens$Hok <- sol.ls$xbest #apply optimal solution to pen variable
  return(tempPens)
}

#' Assign piglets to pens
#'
#' Assign piglets to their pens keeping standard deviation in average weight per pen to a minimum.
#'
#' @param data data.table with columns Zeugnr (sow ID),  Sex (sex of piglets), and Speen_gew (weaning weights of piglets)
#' @param nH numeric number of piglets to be housed in each pen
#' @param nWC numeric number of weight classes to be created (default = 3)
#' @param separateSex boolean Should barrows and gilts be kept in separate pens (default = TRUE)? If FALSE, the number of barrows and gilts should be equal in total and per pen.
#'
#' @return data.table with added column Hok (pen)
#' @export
#'
#' @import stats
#'
#' @examples
#' animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
#' animalsInTrial <- assignPens(animalsInTrial,6)
assignPens <- function(data, nH, nWC=3, separateSex = T){
  Sex <- Gew_klasse <- Zeugnr <- Rand <-Speen_gew <- Hok <- V1 <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html

  if((base::nrow(data[Sex=="B"]) %% nH) != 0){stop("Number of barrows is not a multiple of number of piglets per pen")}
  if((base::nrow(data[Sex=="Z"]) %% nH) != 0){stop("Number of gilts is not a multiple of number of piglets per pen")}

  if((base::nrow(data[Sex=="B"]) %% nWC) != 0){stop("Number of barrows is not a multiple of weight classes, consider changing nWC, possible values are c(1, 2, 3)")}
  if((base::nrow(data[Sex=="Z"]) %% nWC) != 0){stop("Number of gilts is not a multiple of weight classes, consider changing nWC, possible values are c(1, 2, 3)")}

  x <- base::nrow(data[Sex=="B"])  / (nWC * nH)
  y <- base::nrow(data[Sex=="Z"])  / (nWC * nH)

  tempAIT <- data

  tempAIT <- createWeightClass(tempAIT,nWC) #create weight classes

  tempAIT$Rand <- stats::runif(base::nrow(tempAIT),0,1) #create variable of random numbers

  tempAIT <- tempAIT[base::order(Sex,Gew_klasse,Zeugnr,Rand)] #sort data by sex, weight class, sow ID, and random number column

  if(separateSex==F){
    B <- tempAIT$Sex == "B"
    xB <- integer(length(B))
    xB[B] <- cumsum(rep(c((x*(nH/2)) - 1,rep(0,(x*(nH/2)) - 1)), length.out=sum(B))) - 1
    xB[!B] <- cumsum(rep(c((x*(nH/2)) - 1,rep(0,(x*(nH/2)) - 1)), length.out=sum(!B)))
    tempAIT <- tempAIT[order(xB),]
    tempAIT[Sex=="Z"] <- tempAIT[Sex=="Z"][order(Gew_klasse,-Zeugnr),]
  }

  #assign pen numbers
  if(nWC==1){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(x+y),nH))}
  if(nWC==2){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(2*x),nH), base::rep((2*x+1):(2*x+y),nH), base::rep((2*x+1+y):(2*x+2*y),nH))}
  if(nWC==3){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(2*x),nH), base::rep((2*x+1):(3*x),nH), base::rep((3*x+1):(3*x+y),nH), base::rep((3*x+1+y):(3*x+2*y),nH), base::rep((3*x+1+2*y):(3*x+3*y),nH))}
  if(nWC==4){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(2*x),nH), base::rep((2*x+1):(3*x),nH), base::rep((3*x+1):(4*x),nH), base::rep((4*x+1):(4*x+y),nH), base::rep((4*x+1+y):(4*x+2*y),nH), base::rep((4*x+1+2*y):(4*x+3*y),nH), base::rep((4*x+1+3*y):(4*x+4*y),nH))}

  base::print("Mean weight per pen before:")
  base::print(tempAIT[,base::mean(Speen_gew),Hok]) #print the initial mean weights of each pen

  for (i in base::seq(1,(x*nWC),by=x)){
    tempAIT <- tempAIT[base::order(Sex,Gew_klasse,Hok)]
    partPens <- tempAIT[Hok%in%c(i:(i+x-1))]

    partPens <- switchInPen(partPens)

    tempAIT[Hok%in%c(i:(i+x-1))] <- partPens
  }

  for (i in base::seq(x*nWC+1,(x*nWC+nWC*y),by=y)){
    tempAIT <- tempAIT[base::order(Sex,Gew_klasse,Hok)]
    partPens <- tempAIT[Hok%in%c(i:(i+y-1))]

    partPens <- switchInPen(partPens)

    tempAIT[Hok%in%c(i:(i+y-1))] <- partPens
  }

  tempAIT <- tempAIT[base::order(Sex,Gew_klasse,Hok)]

  base::print("Mean weight per pen after:")
  base::print(tempAIT[,base::mean(Speen_gew),Hok])

  if (base::nrow(tempAIT[,base::sum(base::duplicated(Zeugnr)),Hok][V1 != 0]) != 0){
    base::print("The following pens contain siblings:")
    base::print(tempAIT[,sum(duplicated(Zeugnr)),Hok][V1 != 0])
  }

  return(tempAIT)
}
