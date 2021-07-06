#' Find outliers in vector
#'
#' @param v numeric vector
#' @param coef numeric coefficient  to multiply with IQR (Default = 1.5)
#'
#' @return boolean vector
#' @export
check_outlier <- function(v, coef=1.5){
  quantiles <- quantile(v,probs=c(0.25,0.75))
  IQR <- quantiles[2]-quantiles[1]
  res <- v < (quantiles[1]-coef*IQR)|v > (quantiles[2]+coef*IQR)
  return(res)
}

#' Boxplots of treatments over phases in feed trial
#'
#' @param data data.table
#' @param var character name of zootechnical parameter of interest
#' @param treatment character name of treatment variable (default= "behandeling1")
#' @param group1 character name of grouping variable 1 (default= "faseW")
#' @param group2 character name of grouping variable 2 (default= "ronde")
#' @param group3 character name of grouping variable 1 (default= NULL)
#' @param lab character name to be used in legend for grouping variable 1 (default = "Treatment")
#' @param lab2 character name to be used in legend for grouping variable 2 (default = "Round")
#' @param pen character name for pen variable (default= "hok")
#' @param xlab character label for x-axis
#' @param ylab character label for y-axis
#' @param colours character vector names for colours for each treatments (defaults to selection of n(treatment) colours from "Set3" of RColorBrewer palette)
#'
#' @return plot
#' @export
#'
#' @import ggplot2
#' @import RColorBrewer
#'
#' @examples
#' groeidataLong <- makeLong(groeidata,2)
#' boxplotZoo(data=groeidataLong,var="Weight",xlab="Treatment groups per phase",ylab= "Weight (kg)")
boxplotZoo <- function(data,var,treatment = "behandeling1",group1 = "faseW",group2 = "ronde",group3 = NULL,lab = "Treatment",lab2 = "Round",pen = "hok",xlab,ylab,colours = brewer.pal(length(unique(data[,get(treatment)])),"Set3")){
  label <- outlier <- NULL #To prevent 'no visible binding' note according to https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html

  if(is.null(group3)){data[,outlier:=check_outlier(get(var)),by=list(get(group1),get(treatment))]}else{
    data[,outlier:=check_outlier(get(var)),by=list(get(group1),get(group3),get(treatment))]
  }

  data[,label:=ifelse(outlier,as.character(get(pen)),"")]

  if(is.null(group3)){
    figuur<- ggplot(data, aes(x=factor(get(treatment), levels=rev(levels(get(treatment)))),y=get(var), fill=get(treatment)))+ xlab(xlab)  +ylab(ylab) +
      geom_boxplot() + scale_fill_manual(values=colours) + geom_text(aes(label=label),hjust=0.3,vjust=-0.4) + coord_flip() +facet_grid(rows=vars(get(group1))) +
      geom_point(aes(shape = factor(get(group2)),colour=factor(get(group2))))+ guides(fill=guide_legend(title=lab),shape=guide_legend(title=lab2),colour=guide_legend(title=lab2))
  }else{
    figuur<- ggplot(data, aes(x=factor(get(treatment), levels=rev(levels(get(treatment)))),y=get(var), fill=get(treatment)))+ xlab(xlab)  +ylab(ylab) +
      geom_boxplot() + scale_fill_manual(values=colours) + geom_text(aes(label=label),hjust=0.3,vjust=-0.4) + coord_flip() +facet_grid(rows=vars(get(group1),get(group3))) +
      geom_point(aes(shape = factor(get(group2)),colour=factor(get(group2))))+ guides(fill=guide_legend(title=lab),shape=guide_legend(title=lab2),colour=guide_legend(title=lab2))

  }
  figuur
}
