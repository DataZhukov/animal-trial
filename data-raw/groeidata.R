## code to prepare `zootechnical` dataset

in.file.name<-"zootechnicalRaw.xlsx"
data.sheet.nameG1 <- "groeidata"
na.strings <- c("-","."," ","",NULL,"NA")

dat <- read_excel(in.file.name,data.sheet.nameG1,na=na.strings)
dat<-dat[,colSums(is.na(dat)) != nrow(dat)]
dat<-dat[rowSums(is.na(dat)) != ncol(dat),]
groeidata <- dat

groeidata <- as.data.table(groeidata)
names(groeidata)
factors <- c("bedrijf","ronde","compartiment","hok","behandeling1","behandeling2","sex","oornr","moeder","beer")
for (i in factors) {
  groeidata[[i]]<-as.factor(groeidata[[i]])
}
