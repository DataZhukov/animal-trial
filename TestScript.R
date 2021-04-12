#Test script - use to test out package functions

animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
min(animalsInTrial$Speen_gew)
animalsInTrial <- assignPens(animalsInTrial,6,0.1,nWC=3)
animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Geel"))
animalsInTrial <- assignComp(animalsInTrial,c("Wit","Geel"))
table(animalsInTrial$Comp,animalsInTrial$Gew_klasse,animalsInTrial$Beh)


