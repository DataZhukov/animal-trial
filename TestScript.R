#Test script - use to test out package functions

animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
animalsOutTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[2]]
min(animalsInTrial$Speen_gew)
animalsInTrial <- assignPens(animalsInTrial,6,separateSex = T)
animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Geel","Groen","Rood"))
animalsInTrial <- assignComp(animalsInTrial)
table(animalsInTrial$Comp,animalsInTrial$Gew_klasse,animalsInTrial$Beh)
exportResult(animalsInTrial,animalsOutTrial)
trialSummary(animalsInTrial)


data <- animalsInTrial

x <- base::nrow(data[Sex=="B"])  / (4 * 6)
y <- base::nrow(data[Sex=="Z"])  / (4 * 6)

tempAIT <- data

tempAIT <- createWeightClass(tempAIT,4) #create weight classes

tempAIT$Rand <- stats::runif(base::nrow(tempAIT),0,1) #create variable of random numbers

tempAIT <- tempAIT[base::order(Sex,Gew_klasse,Zeugnr,Rand)] #sort data by sex, weight class, sow ID, and random number column

#assign pen numbers
if(nWC==1){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(x+y),nH))}
if(nWC==2){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(2*x),nH), base::rep((2*x+1):(2*x+y),nH), base::rep((2*x+1+y):(2*x+2*y),nH))}
if(nWC==3){tempAIT$Hok <- c(base::rep(1:x,nH), base::rep((x+1):(2*x),nH), base::rep((2*x+1):(3*x),nH), base::rep((3*x+1):(3*x+y),nH), base::rep((3*x+1+y):(3*x+2*y),nH), base::rep((3*x+1+2*y):(3*x+3*y),nH))}

nBShort <- 1
nZShort <- 0

existingDF <- as.data.frame(matrix(seq(20),nrow=5,ncol=4))
r <- 3
newrow <- seq(4)
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
insertRow(existingDF, newrow, r)

dummies <- data.frame(Sex=c(rep("B",nBShort),rep("Z",nZShort)),Oornr=c(1:(nBShort+nZShort)),Geb_dat=NA,Zeugnr=c(paste("0/",1:(nBShort+nZShort),sep="")),Beer="DUMMY",Geb_gew=NA,Speen_gew=NA,Oud=NA,K.H.=NA,Opm.=NA)

split(animalsInTrial, ceiling(seq_along(animalsInTrial)/3))
split(animalsInTrial, cut(seq_along(animalsInTrial), 2, labels = FALSE))


boxplotZoo(var="Weight",treatment="behandeling1",lab="Treatment",
       group1="faseW",group2="ronde",group3 = "sex",lab2="Round",data=groeidataLong,pen="hok",xlab="Treatment groups per phase",
       ylab= "Weight (kg)")

hok <- groeidata[!is.na(groeidata$dgtot),]
test <- modelsZoo(groeidataLong,VARnames = c("geinde","dvotot"),effects = "behandeling1+gbegin+(1|compartiment/ronde)")
data <- groeidataLong
VARnames <- c("Weight","Daily_gain","Daily_feed_intake","Feed_conversion_ratio")
effects <- "faseW+behandeling1+ronde+faseW:behandeling1+ronde:behandeling1+gbegin+(1|hok)"
group1 <- "faseW"
treatment <- "behandeling1"
precision <- 2

groeidataLong <- makeLong(groeidata,2)
boxplotZoo(data=groeidataLong,var="Weight",xlab="Treatment groups per phase",ylab= "Weight (kg)")
tabel <- modelsZoo(groeidataLong,VARnames = c("geinde"),effects = "behandeling1+gbegin+(1|compartiment/ronde)")

