#Test script - use to test out package functions

animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
animalsOutTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[2]]
min(animalsInTrial$Speen_gew)
animalsInTrial <- assignPens(animalsInTrial,6,nWC=3)
animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Geel","Groen","Rood"))
animalsInTrial <- assignComp(animalsInTrial,c("Wit","Geel","Groen","Rood"))
table(animalsInTrial$Comp,animalsInTrial$Gew_klasse,animalsInTrial$Beh)

group_by

comp<-data.table(Pen=c(2,3,4,5,16,18,20,22,9,10,11,12,13,14,15,19,1,6,7,8,17,21,23,24),Sex=rep(c("B","B","B","B","Z","Z","Z","Z"),3),Weight=c("L","L","L","M","L","M","M","Z","Z","Z","Z","Z","L","L","L","M","L","M","M","M","M","Z","Z","Z"),Treatment=c("G","W","W","G","W","G","W","G","G","G","W","W","G","G","W","W","G","G","W","W","G","G","W","W"))

comp <- comp[order(Sex,Treatment,Weight)]

test <-fold(comp[Treatment=="W"&Sex=="B"][c(1,3,5)], k = 3,method="n_dist")

table(test$.folds,test$Weight)
table(test$.folds,test$Treatment)
table(test$.folds,test$Sex)

table(animalsInTrial$Comp,animalsInTrial$Gew_klasse)
chi<- chisq.test(table(animalsInTrial$Comp,animalsInTrial$Gew_klasse))
chi$p.value
table(animalsInTrial$Comp,animalsInTrial$Beh)
table(animalsInTrial$Comp,animalsInTrial$Sex)

biggen <- bind_rows(animalsInTrial,animalsOutTrial)
write.table(biggen,file = "Output/biggen.txt",sep="|",dec=".",na="NA",row.names = F)

devtools::install_github("DataZhukov/animalTrial")

sowids <- rownames(table(partPens$Zeugnr,partPens$Hok)[table(partPens$Zeugnr,partPens$Hok) == 1])
