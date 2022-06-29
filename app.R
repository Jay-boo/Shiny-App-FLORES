# Start with update
# Be sure RAM is empty at start
rm(list=ls())
library(tidyverse)
library(shiny)
library(r2d3)
#source("update.R")
# Create  missing_imports list + Import the missing_files 


pieChart_etab = d3Output("pieChart_etab")
pieChart_rem=d3Output("pieChart_rem")

server <- function(input,output){
    TC1 <-read.csv("./outputs/2017/TC1.csv")
    data_for_plot<-TC1 %>% filter(typo_A_det=="Ensemble" & typo_A=="Ensemble des secteurs d'activité" & famille=="Ensemble" & ESS=="ESS + Hors ESS")%>% select(DEP, rem_brut)
    data_for_plot$rem_brut<-as.numeric(data_for_plot$rem_brut)
    
    TC13 <-read.csv("./outputs/2017/TC13.csv")
    df<-TC13 %>% filter(typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble" & famille=="Ensemble" & ESS=="ESS")%>% select(REG,nb_etab,rem_brut)
    df<-df%>% filter(REG %in% c("France entière","Bourgogne-Franche-Comté"))
    df$nb_etab<-as.numeric(df$nb_etab)
    df$rem_brut<-as.numeric(df$rem_brut)
    
    
    
    output$plot <- renderPlot({
        barplot(rem_brut ~ DEP,data=data_for_plot)
    })

    output$pieChart_etab <- renderD3({
        tmp_data <- df %>% select(REG, nb_etab)
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(width=100,height=100,background_color='rgb(48, 76, 89)')
        )
    })

    output$pieChart_rem<-renderD3({
        tmp_data <- df %>% select(REG, rem_brut)
        colnames(tmp_data)<-c("REG","nb_etab")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(width=100,height=100,background_color='rgb(48, 76, 89)')
        )
    })

    


}


shinyApp(
    ui = htmlTemplate(
        "www/index.html",
        pieChart_etab = pieChart_etab,
        pieChart_rem = pieChart_rem
        ),
    server = server
)


