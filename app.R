# Start with update
# Be sure RAM is empty at start
rm(list=ls())
library(tidyverse)
library(shiny)
library(r2d3)
library(dplyr)
library(readODS)
require(skimr)
#source("update.R")
# Create  missing_imports list + Import the missing_files 


# types <-list(
# 	"numeric"=c("taille_etab","nb_poste","nb_etab","nb_ent","eff_31","eff_EQTP","rem_brut"),
# 	"factor"=c("DEP","REG","ESS",
# 			   "famille","typo_A","typo_A_det",
# 			   "typo_B","typo_B_det",
# 			   "typo_C","typo_C_det",
# 			   "typo_Asso","typo_Asso_det",
# 			   "typo_Mut","typo_Mut_det",
# 			   "typo_Coop","typo_Coop_det",
# 				"sexe",
# 			   "CSP",
# 			   "nom_jurid_coop",
# 			   "nom_jurid_2",
# 			   "type_emploi",
# 			   "IDF"
# 	)
# )






#-------------------------------
# D3 output
pieChart_etab <- d3Output("pieChart_etab",height = "70%")
pieChart_rem= d3Output("pieChart_rem",height = "70%")
pieChart_emploi = d3Output("pieChart_emploi",height = "70%")
barPlot_famille_etab = d3Output("barPlot_famille_etab",height="100%",width = "45%")
barPlot_famille_eff31 = d3Output("barPlot_famille_eff31",height="100%",width = "45%")
barPlot_H_F=d3Output("barPlot_H_F",height="27%",width = "35%")
mapReg=d3Output("mapReg",height ="30%",width = "95%")

selectbox_HTML=""
for (choice in list.dirs("./outputs/", recursive = FALSE, full.names = FALSE) ){
    selectbox_HTML=paste(selectbox_HTML,'<option value="',choice,'">',choice,'</option>',sep="")
}

dashboard_select_Input_YEARS_key=HTML(paste('<select id="dashboard_year_select_key">',selectbox_HTML,'</select>',sep=""))

dashboard_select_Input_YEARS_part2=HTML(paste('<select id="dashboard_year_select_part2">',selectbox_HTML,'</select>',sep=""))

DEP_GE=c("08","10","51","52","54","55","57","67","68","88")
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
        PATH = paste("outputs/",input$dashboard_year_select_key,"/",sep="")
        return(PATH)

    })
    TC13_dashboard <-reactive({
         tmp <- read.csv(paste(dataDashBoard_PATH(),"TC13.csv",sep=""),fileEncoding="latin1")

         tmp <- tmp %>% filter(typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble" & famille=="Ensemble" & ESS=="ESS")%>% select(REG,nb_etab,rem_brut)
         tmp <- tmp%>% filter(REG %in% c("France entière","Grand-Est"))
         tmp$nb_etab<-as.numeric(tmp$nb_etab)
         tmp$rem_brut<-as.numeric(tmp$rem_brut)
        return(tmp)
    })
    TC16_dashboard <-reactive({
        tmp<-read.csv(paste(dataDashBoard_PATH(),"TC16.csv",sep=""),fileEncoding="latin1")
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
        tmp<-TC13_dashboard() %>% filter(REG == "Grand-Est")%>% select(nb_etab)
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
        PATH = paste("outputs/",input$dashboard_year_select_part2,"/",sep="")
        return(PATH)
    })
    TC13_dashboard_bis <-reactive({
        tmp <- read.csv(paste(dataDashBoard_PATH_part2(),"TC13.csv",sep=""),fileEncoding="latin1")
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
        tmp<-read.csv(paste(dataDashBoard_PATH_part2(),"TC16.csv", sep = ""),fileEncoding="latin1")
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

    TC1_dashboard <-reactive({
        TC1<-read.csv(paste(dataDashBoard_PATH(),"TC1.csv",sep=""),fileEncoding="latin1")
        TC1_dep_num=TC1$DEP %>% as.numeric 
        for (i in 1: length(TC1_dep_num)){
            if (!is.na(TC1_dep_num[i]) & TC1_dep_num[i]<10){
                TC1$DEP[i]<-paste("0",TC1$DEP[i],sep="")
            } 
        }

        TC1_famille_tmp<-strsplit(TC1$famille,"[.]")
        TC1$famille<-sapply(TC1_famille_tmp,FUN = function(i){
            if(length(i)>1){
                return(i[2])
            }else{
                return(i[1])
            }
        })
        tmp<-TC1 %>% filter(
        typo_A_det =="Ensemble" &
        ESS=="ESS" &
        DEP %in% DEP_GE
        )
        return(tmp)
    })

    output$barPlot_famille_etab <- renderD3({
        tmp_data <- TC13_dashboard_bis() %>% select(famille, nb_etab)
        tmp_data<-tmp_data %>% filter(tolower(famille)!="ensemble")
        tmp_data$nb_etab<-as.numeric(tmp_data$nb_etab)
        colnames(tmp_data)<-c("country","value")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/barplot_classic.js",
            options = list(
                background_color = 'rgb(215, 245, 255)',
                title=paste("Nombre établissements par forme juridique (",input$dashboard_year_select_part2,")",sep=""),
                var_name="Nombre établissements",
                short_var_name="étab",
                year=input$dashboard_year_select_part2
            )
        )
    })

    output$barPlot_famille_eff31 <- renderD3({
        tmp_data <- TC13_dashboard_bis() %>% select(famille, eff_31)
        tmp_data<-tmp_data %>% filter(tolower(famille)!="ensemble")
        tmp_data$eff_31<-as.numeric(tmp_data$eff_31)
        colnames(tmp_data)<-c("country","value")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/barplot_classic.js",
            options = list(
                background_color = 'rgb(215, 245, 255)',
                title=paste("Effectifs par forme juridique (",input$dashboard_year_select_part2,")",sep=""),
                var_name=paste("Effectifs 31/12/",input$dashboard_year_select_part2,sep=""),
                short_var_name=paste("eff 31/12/",input$dashboard_year_select_part2,sep=""),
                year=input$dashboard_year_select_part2
            )
        )
    })

    output$barPlot_H_F <- renderD3({
        tmp_data <- TC16_dashboard_bis() %>% 
            filter(tolower(sexe)!="ensemble" & tolower(famille)=="ensemble")%>%
            select(sexe, nb_poste)
        tmp_data$nb_poste<-as.numeric(tmp_data$nb_poste)
        colnames(tmp_data)<-c("country","value")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/barplot_circular.js",
            options = list(
                background_color = 'rgb(215, 245, 255)',
                title = "Repartition Homme Femmes"
            )
        )
    })

    output$mapReg <-renderD3({
        tmp_data <- TC1_dashboard()%>%
        filter(
            typo_A!="Ensemble des secteurs d'activité" &
            tolower(famille)=="ensemble"
        )%>%
        select(DEP,famille,nb_etab)%>% arrange(DEP)

        r2d3(
            data=tmp_data,
            script="www/assets/map_dep.js",
            options = list(
                background_color = 'rgb(215, 245, 255)'
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
        barPlot_H_F=barPlot_H_F,
        mapReg=mapReg,
        dashboard_select_Input_YEARS_key=dashboard_select_Input_YEARS_key,
        dashboard_select_Input_YEARS_part2=dashboard_select_Input_YEARS_part2
        ),
    server = server
)
