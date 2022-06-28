# Start with update
# Be sure RAM is empty at start
rm(list=ls())
library(tidyverse)
library(shiny)
#source("update.R")
# Create  missing_imports list + Import the missing_files 



server <- function(input,output){
    TC1 <-read.csv("./outputs/2017/TC1.csv")
    data_for_plot<-TC1 %>% filter(typo_A_det=="Ensemble" & typo_A=="Ensemble des secteurs d'activité" & famille=="Ensemble" & ESS=="ESS + Hors ESS")%>% select(DEP, rem_brut)
    data_for_plot$rem_brut<-as.numeric(data_for_plot$rem_brut)
    
    
    output$plot <- renderPlot({
        barplot(rem_brut ~ DEP,data=data_for_plot)    
    })


}


shinyApp(
    ui=htmlTemplate("www/index.html"),
    server=server
)


