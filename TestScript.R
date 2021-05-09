#Test script - use to test out package functions

animalsInTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[1]]
animalsOutTrial <- selectTrialAnimals(biggen,72,72,5.4,9.5)[[2]]
min(animalsInTrial$Speen_gew)
animalsInTrial <- assignPens(animalsInTrial,6,nWC=3)
animalsInTrial <- assignTreatment(animalsInTrial,c("Wit","Geel","Groen","Rood"))
animalsInTrial <- assignComp(animalsInTrial)
table(animalsInTrial$Comp,animalsInTrial$Gew_klasse,animalsInTrial$Beh)

data <- biggen

buitenProef <- data[!base::is.na(Opm.)] #All piglets with comments to be left out of trial
tempBiggen <- data[base::is.na(Opm.)] #For now keep all piglets without comments in the trial

tempBiggen <- tempBiggen[base::order(Sex,Speen_gew)] #sort piglets by sex and weaning weight

biggenB <- tempBiggen[Sex=="B"] #select only barrows
biggenZ <- tempBiggen[Sex=="Z"] #select only gilts

test<-data.frame(sex=c("m","m","f","f","m","m","f","m","f","m"),weight=runif(10,5,9))

library("plotly")
library("shiny")
library("DT")

ui <- fluidPage(
  mainPanel(
    plotlyOutput("plot")
  ),
  DT::dataTableOutput('tbl')
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    p <- plot_ly(x = test$weight, type = "histogram")
  })


  output$tbl <- renderDataTable({
    s <- event_data("plotly_selected")
    s
  })

}

shinyApp(ui, server)
