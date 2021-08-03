#' Run models for each parameter
#'
#' @param data data.table
#' @param precision numeric number of digits after the decimal
#' @param VARnames character vector of variables to use from data
#' @param effects formula for starting model
#' @param treatment character name of treatment variable
#' @param group1 character name of second grouping variable, typically faseW
#'
#' @return table of means and sd with significance codes
#' @export
#'
#' @import lmerTest
#' @import car
#' @import emmeans
#' @import multcomp
#'
#' @examples
#' groeidataLong <- makeLong(groeidata,2)
#' tabel <- modelsZoo(groeidataLong,VARnames = c("geinde","dgtot"),effects = "behandeling1+gbegin+(1|compartiment/ronde)")
modelsZoo <- function(data,precision = 2, VARnames, effects, treatment = "behandeling1", group1 = NULL){
 if(is.null(group1)){
  means <- data[,lapply(.SD,function(x)sprintf(paste("%.",precision,"f",sep=""), round(mean(x,na.rm=T),precision+1)))
               ,.SDcols=VARnames,keyby=get(treatment)]
  names(means)[1] <- c(treatment)
  sds <- data[,lapply(.SD,function(x)sprintf(paste("%.",precision,"f",sep=""), round(sd(x,na.rm=T),precision+1)))
             ,.SDcols=VARnames,keyby=get(treatment)]
  names(sds)[1] <- c(treatment)
 }else{
   means <- data[,lapply(.SD,function(x)sprintf(paste("%.",precision,"f",sep=""), round(mean(x,na.rm=T),precision+1)))
                 ,.SDcols=VARnames,keyby=list(get(group1),get(treatment))]
   names(means)[1:2] <- c(group1,treatment)
   sds <- data[,lapply(.SD,function(x)sprintf(paste("%.",precision,"f",sep=""), round(sd(x,na.rm=T),precision+1)))
               ,.SDcols=VARnames,keyby=list(get(group1),get(treatment))]
   names(sds)[1:2] <- c(group1,treatment)
  }

  for (i in VARnames) {
    formula <- as.formula(paste(i,"~",effects))
    model <- lmerTest::lmer(formula,data=data)
    selection <- lmerTest::step(model,ddf ="Kenward-Roger",alpha.random = 0.1,alpha.fixed = 0.1,reduce.fixed = TRUE,reduce.random = F,keep= treatment)
    model <- get_model(selection)
    ano <- Anova(model,type="III",test.statistic="F")
    print(summary(model))
    print(ano)
    qqPlot(residuals(model))
    if(is.null(group1) || !(group1 %in% row.names(ano))){
      cld <- cld(emmeans(model,as.formula(paste("~",treatment))),Letters=letters,sort=T,reversed =T)
      cld <- cld[order(cld[,treatment]),]
      cld <-gsub(" ", "", cld[,7], fixed = TRUE)
      }else{
      cld <- cld(emmeans(model,as.formula(paste("~",treatment,"|",group1))),Letters=letters,sort=T,reversed =T)
      cld <- cld[order(cld[,group1],cld[,treatment]),]
      cld <-gsub(" ", "", cld[,8], fixed = TRUE)
    }


    if (length(unique(cld))>1){
      means[[i]] <- paste(means[[i]],cld,sep="")
    }
  }

  if(is.null(group1)){tot <- merge(means,sds,by=treatment)}else{
    tot <- merge(means,sds,by=c("behandeling1","faseW"))
  }

  tot
}
