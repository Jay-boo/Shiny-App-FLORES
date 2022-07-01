# Start with update
# Be sure RAM is empty at start
rm(list=ls())
library(tidyverse)
library(shiny)
library(r2d3)
#source("update.R")
# Create  missing_imports list + Import the missing_files 



#-------------------------------
# D3 output
pieChart_etab = d3Output("pieChart_etab",height = "300px")
pieChart_rem= d3Output("pieChart_rem",height = "300px")
pieChart_emploi = d3Output("pieChart_emploi",height = "300px")



selectbox_HTML=""
for (choice in list.dirs("./outputs/", recursive = FALSE, full.names = FALSE) ){
    selectbox_HTML=paste(selectbox_HTML,'<option value="',choice,'">',choice,'</option>',sep="")
}
selectbox_HTML
dashboard_select_Input_YEARS=HTML(paste('<select id="dashboard_year_select">',selectbox_HTML,'</select>',sep=""))



#---------------------------------
# Pre import 



#----------------------------------
# Server

server <- function(input,output){
    #--------------------------------------------------------------
    # Headers
    TC1 <-read.csv("./outputs/2017/TC1.csv")
    #TC13 <-read.csv("./outputs/2017/TC13.csv")
    #TC16 <- read.csv("./outputs/2017/TC16.csv")
    #
#
    TC1<-TC1 %>% filter(typo_A_det=="Ensemble" & typo_A=="Ensemble des secteurs d'activité" & famille=="Ensemble" & ESS=="ESS + Hors ESS")%>% select(DEP, rem_brut)
    TC1$rem_brut<-as.numeric(TC1$rem_brut)
    #
    #
    #dashboard_key<-TC13 %>% filter(typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble" & famille=="Ensemble" & ESS=="ESS")%>% select(REG,nb_etab,rem_brut)
    #dashboard_key<-dashboard_key%>% filter(REG %in% c("France entière","Bourgogne-Franche-Comté"))
    #dashboard_key$nb_etab<-as.numeric(dashboard_key$nb_etab)
    #dashboard_key$rem_brut<-as.numeric(dashboard_key$rem_brut)
#
    #dashboard_key_bis<-TC16 %>% filter(
    #    tolower(famille)=="ensemble" & 
    #    tolower(sexe)=="ensemble" & 
    #    tolower(type_emploi)=="ensemble" &
    #    ESS=="ESS" & 
    #    REG %in% c("France entière","Bourgogne-Franche-Comté")
    #    )%>% select(REG,nb_poste)
#
    
    
    
    
    #---------------------------------
    # DASHBOARD
    dataDashBoard_PATH <-reactive({
        PATH = paste("./outputs/",input$dashboard_year_select,"/",sep="")
        print(PATH)
        return(PATH)

    })
    TC13_dashboard <-reactive({
         tmp <- read.csv(paste(dataDashBoard_PATH(),"TC13.csv",sep=""))

         tmp <- tmp %>% filter(typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble" & famille=="Ensemble" & ESS=="ESS")%>% select(REG,nb_etab,rem_brut)
         tmp <- tmp%>% filter(REG %in% c("France entière","Bourgogne-Franche-Comté"))
         tmp$nb_etab<-as.numeric(tmp$nb_etab)
         tmp$rem_brut<-as.numeric(tmp$rem_brut)
        return(tmp)
    })
    TC16_dashboard <-reactive({
        tmp<-read.csv(paste(dataDashBoard_PATH(),"TC16.csv",sep=""))
        tmp<-tmp %>% filter(
        tolower(famille)=="ensemble" & 
        tolower(sexe)=="ensemble" & 
        tolower(type_emploi)=="ensemble" &
        ESS=="ESS" & 
        REG %in% c("France entière","Bourgogne-Franche-Comté")
        )%>% select(REG,nb_poste)

        return(tmp)
    })





    #----------------
    # ALL  Dashboard key INPUT
    #input$dashboard_year_select
    

    #----------------
    # Dashboard key OUTPUT
    output$pieChart_etab <- renderD3({
        tmp_data <- TC13_dashboard() %>% select(REG, nb_etab)
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(background_color='rgb(48, 76, 89)')
            
        )
    })

    output$pieChart_rem<-renderD3({
        tmp_data <- TC13_dashboard() %>% select(REG, rem_brut)
        colnames(tmp_data)<-c("REG","nb_etab")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(background_color='rgb(48, 76, 89)')
        )
    })

    output$pieChart_emploi <- renderD3({
        tmp_data <- TC16_dashboard()
        colnames(tmp_data) <- c("REG","nb_etab")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(background_color='rgb(48, 76, 89)')
        )
    })

    output$nb_etab_txt_FR<- renderText({
        tmp<-TC13_dashboard() %>%filter(tolower(REG) == "france entière")%>% select(nb_etab)
        paste(tmp[[1]], " établissements dans la France Entière")
    })

    output$nb_etab_txt_GE<- renderText({
        tmp<-TC13_dashboard() %>%filter(REG == "Bourgogne-Franche-Comté")%>% select(nb_etab)
        paste(tmp[[1]], " établissements dans la region Grand Est")
    })


    output$emploi_txt_FR<- renderText({
        val<-TC16_dashboard() %>%filter(tolower(REG) == "france entière")%>% select(nb_poste)
        paste(val[[1]], " emplois dans la France Entière")
    })

    output$emploi_txt_GE<- renderText({
        val<-TC16_dashboard() %>%filter(REG == "Bourgogne-Franche-Comté")%>% select(nb_poste)
        paste(val[[1]], " emplois dans la region Grand Est")
    })


    output$masse_salariale_txt_GE <-renderText({
        GE <- TC13_dashboard() %>%filter(REG == "Bourgogne-Franche-Comté")%>% select(rem_brut)
        paste(GE[[1]], "€ de masse salariale dans la région Grand-Est\n")
    })
    output$masse_salariale_txt_FR <-renderText({
        FR <- TC13_dashboard() %>%filter(tolower(REG) == "france entière")%>% select(rem_brut)
        paste(FR[[1]], "€ de masse salariale en France Entière",input$dashboard_year_select)
    })



    #---------------------------------
    #   Dashboard part EPCI

    output$plot <- renderPlot({
        barplot(rem_brut ~ DEP,data=TC1)
    })



    


}


shinyApp(
    ui = htmlTemplate(
        "www/index.html",
        pieChart_etab = pieChart_etab,
        pieChart_rem = pieChart_rem,
        pieChart_emploi = pieChart_emploi,
        dashboard_select_Input_YEARS=dashboard_select_Input_YEARS
        ),
    server = server
)



