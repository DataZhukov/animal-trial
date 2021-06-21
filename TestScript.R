#Test script - use to test out package functions

animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
animalsOutTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[2]]
min(animalsInTrial$Speen_gew)
animalsInTrial <- assignPens(animalsInTrial,6,nWC=3)
animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Geel","Groen","Rood"))
animalsInTrial <- assignComp(animalsInTrial)
table(animalsInTrial$Comp,animalsInTrial$Gew_klasse,animalsInTrial$Beh)

data <- animalsInTrial

x <- base::nrow(data[Sex=="B"])  / (nWC * nH)
y <- base::nrow(data[Sex=="Z"])  / (nWC * nH)

tempAIT <- data

tempAIT <- createWeightClass(tempAIT,nWC) #create weight classes

tempAIT$Rand <- stats::runif(base::nrow(tempAIT),0,1) #create variable of random numbers

tempAIT <- tempAIT[base::order(Sex,Gew_klasse,Zeugnr,Rand)] #sort data by sex, weight class, sow ID, and random number column

#assign pen numbers
if(nWC==1){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(x+y),nH))}
if(nWC==2){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(2*x),nH), base::rep((2*x+1):(2*x+y),nH), base::rep((2*x+1+y):(2*x+2*y),nH))}
if(nWC==3){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(2*x),nH), base::rep((2*x+1):(3*x),nH), base::rep((3*x+1):(3*x+y),nH), base::rep((3*x+1+y):(3*x+2*y),nH), base::rep((3*x+1+2*y):(3*x+3*y),nH))}


