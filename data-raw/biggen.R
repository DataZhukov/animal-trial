## code to prepare `biggen` dataset

biggen <- read.csv2("biggenRaw.csv",header = T, sep = ";",na.strings = c(" ",""), dec = ".", quote = "")
biggen <- as.data.table(biggen)
