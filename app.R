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
pieChart_etab = d3Output("pieChart_etab",height = "70%")
pieChart_rem= d3Output("pieChart_rem",height = "70%")
pieChart_emploi = d3Output("pieChart_emploi",height = "70%")
barPlot_famille_etab = d3Output("barPlot_famille_etab",height="30%",width = "80%")
barPlot_famille_eff31 = d3Output("barPlot_famille_eff31",height="30%",width = "80%")
barPlot_famille_nb_emploi=d3Output("barPlot_famille_nb_emploi",height="30%",width = "80%")

selectbox_HTML=""
for (choice in list.dirs("./outputs/", recursive = FALSE, full.names = FALSE) ){
    selectbox_HTML=paste(selectbox_HTML,'<option value="',choice,'">',choice,'</option>',sep="")
}

dashboard_select_Input_YEARS_key=HTML(paste('<select id="dashboard_year_select_key">',selectbox_HTML,'</select>',sep=""))

dashboard_select_Input_YEARS_part2=HTML(paste('<select id="dashboard_year_select_part2">',selectbox_HTML,'</select>',sep=""))


#---------------------------------
# Pre import 



#----------------------------------
# Server

server <- function(input,output){

    #-------------------------------
    # INPUTS
    #   dashboard_year_select_key
    #   dashboard_year_select_part2
    
    #---------------------------------
    # DASHBOARD
    dataDashBoard_PATH <-reactive({
        PATH = paste("./outputs/",input$dashboard_year_select_key,"/",sep="")
        return(PATH)

    })
    TC13_dashboard <-reactive({
         tmp <- read.csv(paste(dataDashBoard_PATH(),"TC13.csv",sep=""))

         tmp <- tmp %>% filter(typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble" & famille=="Ensemble" & ESS=="ESS")%>% select(REG,nb_etab,rem_brut)
         tmp <- tmp%>% filter(REG %in% c("France entière","Grand-Est"))
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
        REG %in% c("France entière","Grand-Est")
        )%>% select(REG,nb_poste)

        return(tmp)
    })


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
        tmp<-TC13_dashboard() %>%filter(REG == "Grand-Est")%>% select(nb_etab)
        paste(tmp[[1]], " établissements dans la region Grand Est")
    })


    output$emploi_txt_FR<- renderText({
        val<-TC16_dashboard() %>%filter(tolower(REG) == "france entière")%>% select(nb_poste)
        paste(val[[1]], " emplois dans la France Entière")
    })

    output$emploi_txt_GE<- renderText({
        val<-TC16_dashboard() %>%filter(REG == "Grand-Est")%>% select(nb_poste)
        paste(val[[1]], " emplois dans la region Grand Est")
    })


    output$masse_salariale_txt_GE <-renderText({
        GE <- TC13_dashboard() %>%filter(REG == "Grand-Est")%>% select(rem_brut)
        paste(GE[[1]], "€ de masse salariale dans la région Grand-Est\n")
    })
    output$masse_salariale_txt_FR <-renderText({
        FR <- TC13_dashboard() %>% filter(tolower(REG) == "france entière")%>% select(rem_brut)
        paste(FR[[1]], "€ de masse salariale en France Entière")
    })



    #---------------------------------
    #   Dashboard part.2
    
    dataDashBoard_PATH_part2 <-reactive({
        PATH = paste("./outputs/",input$dashboard_year_select_part2,"/",sep="")
        return(PATH)
    })
    TC13_dashboard_bis <-reactive({
        tmp <- read.csv(paste(dataDashBoard_PATH_part2(),"TC13.csv",sep=""))
        TC13_famille_tmp<-strsplit(tmp$famille,"[.]")
        tmp$famille<-sapply(TC13_famille_tmp,FUN = function(i){
            if(length(i)>1){
                return(i[2])
            }else{
                return(i[1])
            }
        })
        tmp <- tmp %>% filter(typo_B_det=="Ensemble" & ESS=="ESS" & REG=="Grand-Est" & typo_B=="Ensemble des secteurs d'activité")
        
        return(tmp)
    })
    TC16_dashboard_bis<-reactive({
        tmp<-read.csv(paste(dataDashBoard_PATH_part2(),"TC16.csv", sep = ""))
        TC16_famille_tmp<-strsplit(tmp$famille,"[.]")
        tmp$famille<-sapply(TC16_famille_tmp,FUN = function(i){
            if(length(i)>1){
                return(i[2])
            }else{
                return(i[1])
            }
        })
        tmp<-tmp %>% filter( ESS=="ESS" & REG=="Grand-Est" & type_emploi=="Ensemble")
        return(tmp)
    })

    output$barPlot_famille_etab <- renderD3({
        tmp_data <- TC13_dashboard_bis() %>% select(famille, nb_etab)
        tmp_data<-tmp_data %>% filter(tolower(famille)!="ensemble")
        colnames(tmp_data)<-c("country","value")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/barplot_circular.js",
            options = list(
                background_color = 'rgb(197, 184, 184)',
                title="Nombre établissements par forme juridique"
            )
        )
    })

    output$barPlot_famille_eff31 <- renderD3({
        tmp_data <- TC13_dashboard_bis() %>% select(famille, eff_31)
        tmp_data<-tmp_data %>% filter(tolower(famille)!="ensemble")
        colnames(tmp_data)<-c("country","value")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/barplot_circular.js",
            options = list(
                background_color = 'rgb(197, 184, 184)',
                title="Effectifs par forme juridique"
            )
        )
    })

    output$barPlot_famille_nb_emploi <- renderD3({
        tmp_data <- TC16_dashboard_bis() %>% 
            filter(tolower(sexe)!="ensemble" & tolower(famille)=="ensemble")%>%
            select(sexe, nb_poste)
        
        colnames(tmp_data)<-c("country","value")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/barplot_circular.js",
            options = list(
                background_color = 'rgb(197, 184, 184)',
                title = "Repartition Homme Femmes"
            )
        )
    })


    



    


}


shinyApp(
    ui = htmlTemplate(
        "www/index.html",
        pieChart_etab = pieChart_etab,
        pieChart_rem = pieChart_rem,
        pieChart_emploi = pieChart_emploi,
        barPlot_famille_etab=barPlot_famille_etab,
        barPlot_famille_eff31=barPlot_famille_eff31,
        barPlot_famille_nb_emploi=barPlot_famille_nb_emploi,
        dashboard_select_Input_YEARS_key=dashboard_select_Input_YEARS_key,
        dashboard_select_Input_YEARS_part2=dashboard_select_Input_YEARS_part2
        ),
    server = server
)
