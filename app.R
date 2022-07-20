# Start with update
# Be sure RAM is empty at start
rm(list=ls())
library(tidyverse)
library(shiny)
library(r2d3)
library(dplyr)
library(readODS)
require(skimr)
require(jsonlite)
require("httr")
#source("update.R")
# Create  missing_imports list + Import the missing_files 

#------------------
# helpers functions
f1 <- function(num) {
        format(num, big.mark = ' ')
}
getIndexCol  <- function(var_lab,data){
	return(which(colnames(data)==var_lab))
}

types <- list(
			  "numeric"=c("nb_etab","nb_ent","eff_31","eff_EQTP","rem_brut","nb_poste","non_annexe"),
			  "factor"=c("typo_A","typo_A_det",
						"typo_B","typo_B_det",
						"typo_C","typo_C_det",
						"famille",
						"REG","DEP",
						"ESS","ESS_simp",
						"typo_Asso","typo_Asso_det",
						"typo_Mut","typo_Mut_det",
						"typo_Coop","typo_Coop_det",
						"sexe",
						"type_emploi",
						"taille_etab",
						"nom_jurid_coop",
						"typo1","typo2","jurid1","jurid2","EPCI","nom_complet","dep_epci"
			  )
)

all_vars  <- c("nb_etab","nb_ent","eff_31","eff_EQTP","rem_brut","nb_poste",
						"typo_A","typo_A_det",
						"typo_B","typo_B_det",
						"typo_C","typo_C_det",
						"famille",
						"REG","DEP",
						"ESS","ESS_simp",
						"typo_Asso","typo_Asso_det",
						"typo_Mut","typo_Mut_det",
						"typo_Coop","typo_Coop_det",
						"sexe",
						"type_emploi",
						"taille_etab",
						"non_annexe",
						"nom_jurid_coop",
						"jurid1","jurid2","typo1","typo2","EPCI",
						"nom_complet","dep_epci"

			   )
#------------------------------------
# DEP and EPCI links
overAll_filter_REG  <- "Grand-Est"
request=paste("https://geo.api.gouv.fr/regions?nom=",overAll_filter_REG,sep="")
res  <- GET(request)
codeReg <- fromJSON(rawToChar(res$content))$code[1]

request=paste("https://geo.api.gouv.fr/departements?codeRegion=",codeReg,sep="")
res  <- GET(request)
deps <- fromJSON(rawToChar(res$content))

available_DEP <-cbind(deps$nom,deps$code) %>% data.frame
colnames(available_DEP)  <- c("nom","code")

available_EPCI <- read.csv("outputs/2017/EPCI_T1.csv",fileEncoding="latin1") %>% filter(dep_epci %in% available_DEP$code)%>%select(nom_complet)%>%unique

construct_select_box <-function(optionsVector,id){

	selectbox_HTML=""
	for (option in optionsVector){
		selectbox_HTML=paste(selectbox_HTML,'<option value="',option,'">',option,'</option>',sep="")
	}
	return(HTML(paste('<select class="select_box_custom"id="',id,'">',selectbox_HTML,'</select>',sep="")))
}

#---------------------------------
# import  tables : in tables list 


dirs <- list.dirs("outputs/",full.names = FALSE)
dirs <- dirs[which(dirs!="")]

labels <- c()
tables <- list()
for (dir in dirs){
	root  <-  paste("outputs/",dir,sep="")
	files <- list.files(root)	
	for( file in files ){
		#---------------------
		# Supp trash vars + convert type before stack table into the list
		data_tmp  <- read.csv(paste(root,"/",file,sep=""),fileEncoding="latin1")
		index_to_del  <- which(!(colnames(data_tmp) %in% all_vars))
		if (length(index_to_del)>0){
			data_tmp<-data_tmp[,-index_to_del]
		}
		counter=1
		for (var in colnames(data_tmp)){
			counter_list_type <- 1
			for (type in types){
				if (var %in% type){
					if (counter_list_type==1){
						data_tmp[,counter]<- as.numeric(data_tmp[,counter])
					}
				}
				counter_list_type  <-  counter_list_type +1
			}
			counter  <-  counter+ 1
		}

		tables <- append(tables,list(data_tmp))
		labels <- c(labels,paste(root,"/",file,sep=""))
	}
}

names(tables) <- labels



print("--------------------pre_treat end-------------------------------")



#-------------------------------
# D3 output
pieChart_etab <- d3Output("pieChart_etab",height = "70%")
pieChart_rem= d3Output("pieChart_rem",height = "70%")
pieChart_emploi = d3Output("pieChart_emploi",height = "70%")
pieChart_effEQTP= d3Output("pieChart_effEQTP",height = "70%")
pieChart_ent=d3Output("pieChart_ent",height = "70%")
plot_dashboard_part_2 =d3Output("plot_dashboard_part_2",height = "90%")
plot_tabs2_part1_1 =d3Output("plot_tabs2_part1_1",height = "100%",width="90%")

selectbox_HTML=""
for (choice in list.dirs("./outputs/", recursive = FALSE, full.names = FALSE) ){
    selectbox_HTML=paste(selectbox_HTML,'<option value="',choice,'">',choice,'</option>',sep="")
}

dashboard_select_Input_YEARS_key=HTML(paste('<select id="dashboard_year_select_key">',selectbox_HTML,'</select>',sep=""))

select_year_nb_etab=HTML(paste('<select id="select_year_nb_etab" class="select_box_custom">',selectbox_HTML,'</select>',sep=""))


select_year_tabs2_part1=HTML(paste('<select id="select_year_tabs2_part1" class="select_box_custom">',selectbox_HTML,'</select>',sep=""))

select_scale_det_REG= construct_select_box(c(overAll_filter_REG),"select_scale_det")
DEP_GE=c("08","10","51","52","54","55","57","67","68","88")



#----------------------------------
# Server

server <- function(input,output,session){

	#----------------------------------------------------------------------------------
	#           DASHBOARD
	#---------------------------------------------------------------------------

    #-------------------------------
    # INPUTS
    #   dashboard_year_select_key
    #   dashboard_year_select_part2
    #---------------------------------

    dataDashBoard_PATH <-reactive({
        PATH = paste("outputs/",input$dashboard_year_select_key,"/",sep="")
        return(PATH)

    })

    TC13_dashboard <-reactive({
         df <- tables[[paste(dataDashBoard_PATH(),"TC13.csv",sep="")]]
		 
		 pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame

		 colnames(pattern)  <-  c("ESS","famille")
		 patterns  <- paste(pattern$ESS,pattern$famille,sep="")

		 df  <- df %>% filter(REG=="Grand-Est" & typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble")
		 df[which( c(df$ESS,df$famille) %in% pattern )]
		 df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns) %>% select(-REG, -ESS, -typo_B,-typo_B_det)
		 df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
        return(df)
    })








    #--------------------------------------------------
    # DASHBOARD FIRST PART
	#-------------------------------------------------

    output$pieChart_etab <- renderD3({
        tmp_data <- TC13_dashboard() %>% select(famille, nb_etab)
		colnames(tmp_data) <- c("REG","nb_etab")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(background_color='rgb(48, 76, 89)')
            
        )
    })

    output$pieChart_rem<-renderD3({
        tmp_data <- TC13_dashboard() %>% select(famille, rem_brut)
        colnames(tmp_data)<-c("REG","nb_etab")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(background_color='rgb(48, 76, 89)')
        )
    })

    output$pieChart_emploi <- renderD3({
        tmp_data <- TC13_dashboard() %>% select(famille,eff_31)
        colnames(tmp_data) <- c("REG","nb_etab")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(background_color='rgb(48, 76, 89)')
        )
    })

    output$pieChart_effEQTP <- renderD3({
        tmp_data <- TC13_dashboard() %>% select(famille,eff_EQTP)
        colnames(tmp_data) <- c("REG","nb_etab")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(background_color='rgb(48, 76, 89)')
        )
    })

    output$pieChart_ent <- renderD3({
        tmp_data <- TC13_dashboard() %>% select(famille,nb_ent)
        colnames(tmp_data) <- c("REG","nb_etab")
        tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
        r2d3(
            data = tmp_data,
            script ="www/assets/pieChart.js",
            options = list(background_color='rgb(48, 76, 89)')
        )
    })


    #--------------------------------------------------
    # DASHBOARD SECOND PART
	#-------------------------------------------------

	# Utilisation de TC13.csv pour REG , TC1.csv pour DEP et EPCI_T1 pour les EPCI:
	#On aura un barplot particulier avec des dodge position 

	data_dashboard_part2  <- reactive({
		nameTable = ""

		if(input$select_scale_nb_etab=="REG"){
			nameTable="TC13.csv"
		}else if(input$select_scale_nb_etab=="DEP"){
			nameTable="TC1.csv"
		}else{
			nameTable="EPCI_T1.csv"
		}

        PATH = paste("outputs/",input$select_year_nb_etab,"/",sep="")
		nameTable  <- paste(PATH,nameTable,sep="")
		return (tables[[nameTable]])
	})






	plot_dashboard_part_2  <- reactive({
		df  <- data_dashboard_part2()
		varLab  <- input$select_var_nb_etab

		if(input$select_scale_nb_etab=="REG"){
			
			first_part  <- df %>%filter( REG== "Grand-Est" & famille=="Ensemble"  & ESS=="ESS" & typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble") %>% select(-typo_B,-typo_B_det,-famille,-ESS)
			
			first_part  <- data.frame(first_part$REG,first_part[,getIndexCol(varLab,first_part)])
			colnames(first_part)=c("REG",varLab)

			tmp  <-  df %>%filter( REG !="France entière" & famille=="Ensemble"  & ESS=="ESS" & typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble") %>% select(-typo_B,-typo_B_det,-famille,-ESS) 

			tmp  <- data.frame(tmp$REG,tmp[,getIndexCol(varLab,tmp)])
			colnames(tmp)=c("REG",varLab)

			second_part  <- df %>%filter( !(REG %in% c("France entière","Grand-Est")) & famille=="Ensemble"  & ESS=="ESS" & typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble") %>% select(-typo_B,-typo_B_det,-famille,-ESS) 

			second_part  <- data.frame(second_part$REG,second_part[,getIndexCol(varLab,second_part)])
			colnames(second_part)=c("REG",varLab)
			second_part  <- arrange(second_part,desc(second_part[,getIndexCol(varLab,second_part)]))
			second_part  <- second_part%>%head(3)
			first_part <-  rbind(c("moyenne",mean(tmp[,getIndexCol(varLab,tmp)])),first_part)
			data  <- rbind(first_part,second_part)

		}else if(input$select_scale_nb_etab=="DEP"){

			tmp  <-  df %>%filter( DEP !="France entière" & famille=="Ensemble"  & ESS=="ESS" & typo_A=="Ensemble des secteurs d'activité" & typo_A_det=="Ensemble") %>% select(-typo_A,-typo_A_det,-famille,-ESS) %>%filter(as.numeric(DEP) %in% as.numeric(available_DEP$code)) 

			tmp  <- data.frame(tmp$DEP,tmp[,getIndexCol(varLab,tmp)])
			colnames(tmp)  <- c("DEP",varLab)

			code  <-available_DEP%>%filter(nom==input$select_scale_det)%>% select(code) 
			code  <- code[[1]]
			first_part  <- df %>% filter(as.numeric(DEP)==as.numeric(code) &  famille=="Ensemble" & ESS=="ESS" & typo_A=="Ensemble des secteurs d'activité" & typo_A_det=="Ensemble")%>% select(-typo_A,-typo_A_det,-famille,-ESS)

			first_part  <- data.frame(first_part$DEP,first_part[,getIndexCol(varLab,first_part)])
			colnames(first_part)  <- c("DEP",varLab)

			first_part[1,1] =input$select_scale_det
			second_part  <- df %>% filter (as.numeric(DEP) %in% as.numeric(available_DEP$code) & famille=="Ensemble" & ESS=="ESS" & typo_A=="Ensemble des secteurs d'activité" & typo_A_det=="Ensemble")%>%select(-typo_A,-typo_A_det,-famille,-ESS)

			second_part  <- data.frame(second_part$DEP ,second_part[,getIndexCol("nb_etab",second_part)])
			colnames(second_part) <- c("DEP",varLab)
			second_part  <- second_part %>% filter(as.numeric(DEP) !=as.numeric(code))
			second_part  <-  arrange(second_part,desc(second_part[,getIndexCol(varLab,second_part)]))
			second_part  <- second_part %>%head(3)
			available_DEP_bis <- available_DEP
			available_DEP_bis$code  <- as.numeric(available_DEP_bis$code) 
			colnames(second_part)  <- c("code",varLab)
			second_part$code  <- as.numeric(second_part$code)
			second_part  <- left_join(second_part,available_DEP_bis)
			second_part  <- data.frame(second_part$nom,second_part[,getIndexCol(varLab,second_part)])
			colnames(second_part) = c("DEP",varLab)
			first_part  <- rbind(c("moyenne",mean(tmp[,getIndexCol(varLab,tmp)])),first_part)
			data  <- rbind(first_part,second_part)

		}else{






			tmp  <- df %>% filter(nom_complet %in% available_EPCI$nom_complet & jurid1=="4-ESS" ) %>%
				select(-EPCI,-jurid1, -dep_epci)


			first_part  <-  df%>%filter(nom_complet ==input$select_scale_det & jurid1=="4-ESS")
			first_part  <- data.frame(first_part$nom_complet,first_part[,getIndexCol(varLab,first_part)])
			colnames(first_part)=c("nom_complet",varLab)


			second_part  <- df%>% filter(nom_complet %in% available_EPCI$nom_complet & jurid1=="4-ESS")%>% filter(nom_complet!=input$select_scale_det)

			second_part  <- data.frame(second_part$nom_complet,second_part[,getIndexCol(varLab,second_part)])
			colnames(second_part)  <- c("nom_complet",varLab)
			second_part  <- arrange(second_part,desc(second_part[,getIndexCol(varLab,second_part)]))
			second_part  <- second_part %>% head(3)
			first_part  <- rbind(c("moyenne",mean(tmp[,getIndexCol(varLab,tmp)])),first_part)
			data  <- rbind(first_part,second_part)
		}


		colnames(data)  <- c("country","value")
		return(data)


	})

	output$plot_dashboard_part_2  <- renderD3({
		dt  <- plot_dashboard_part_2()
		dt  <- jsonlite::toJSON(dt,dataframe = "rows",auto_unbox = FALSE,rownames=FALSE)
		r2d3(
			 data=dt,
			 script="www/assets/barplot_classic.js",
			 options=list(
						  background_color ='rgb(215, 245, 255)',
						  title=paste("Nombre Etablissement",input$select_scale_det,sep=" "),
						  var_name="Nombre établissements",
						  short_var_name="étab",
						  year=input$select_year_nb_etab

			 )
		)
		
	})







	observe({
		if(input$select_scale_nb_etab=="REG"){
			choice=c(overAll_filter_REG)
		}else if(input$select_scale_nb_etab=="DEP"){
			choice=available_DEP$nom

		}else{
			choice=available_EPCI$nom_complet
		}
		updateSelectInput(session,"select_scale_det",choices=choice)
	})
	#----------------------------------------
	# TAB2 - PART 1
	#----------------------------------------


	data_tabs2_part1  <- reactive({
		year  <- input$select_year_tabs2_part1
		scale  <- input$select_scale_tabs2_part1
		print(scale)
		id_table  <- ""
		if(scale=="REG"){
			id_table  <- "TC13.csv"
		}else if(scale=="DEP"){
			id_table  <- "TC1.csv"
		}else{
			id_table  <- "EPCI_T2.csv"

		}
		PATH  <- paste("outputs/",year,"/",id_table,sep="") 
		df <- tables[[PATH]]

		if(scale=="REG"){

			pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
			colnames(pattern)  <-  c("ESS","famille")
			patterns  <- paste(pattern$ESS,pattern$famille,sep="")
			first_part  <-df%>% filter(REG=="France entière" & typo_B_det=="Ensemble")
			first_part  <- first_part %>%filter(paste(ESS,famille,sep="") %in% patterns)  
			first_part[which(first_part$famille=="Ensemble"),]$famille   <-  "ESS"
			for (i in 1:nrow(first_part)){
				splitted  <- strsplit(first_part$typo_B[i],"[(]")[[1]]
				first_part$typo_B[i]  <- splitted[1]
			}


			first_part  <- data.frame(first_part$REG,first_part$famille,first_part$typo_B,first_part[,getIndexCol("nb_etab",first_part)])

			colnames(first_part)  <- c("REG","famille","typo_B","nb_etab")


			first_part_bis  <-  data.frame("REG"=character(),"famille"=character(),"typo_B"=character(),"var"=numeric())
			for(fam in first_part$famille%>%unique ){
				tmp  <- first_part%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_B=="Ensemble des secteurs d'activité"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
				first_part_bis  <- rbind(first_part_bis,tmp)
			}
			first_part_bis  <- first_part_bis %>% filter(typo_B!="Ensemble des secteurs d'activité")

			df  <- df %>% filter(REG=="Grand-Est" &  typo_B_det=="Ensemble" )
			df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns)  
			df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
			df  <-df%>% select(-typo_B_det,-ESS)
			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$typo_B[i],"[(]")[[1]]
				df$typo_B[i]  <- splitted[1]
			}


			df  <- data.frame(df$REG,df$famille,df$typo_B,df[,getIndexCol("nb_etab",df)])
			colnames(df)  <- c("REG","famille","typo_B","nb_etab")
			new_dat  <-  data.frame("REG"=character(),"famille"=character(),"typo_B"=character(),"var"=numeric())
			for(fam in df$famille%>%unique ){
				tmp  <- df%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_B=="Ensemble des secteurs d'activité"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
			new_dat  <- rbind(new_dat,tmp)
			}

			new_dat  <-new_dat %>% filter(typo_B !="Ensemble des secteurs d'activité")
			data <- rbind(new_dat,first_part)
	
		}else if(scale=="DEP"){


			df  <- tables[["outputs/2017/TC1.csv"]]

			nom_DEP  <- input$select_scale_det_tabs2_part1
			code  <-available_DEP%>%filter(nom==nom_DEP)%>% select(code) 
			code  <- code[[1]]
			pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
			colnames(pattern)  <-  c("ESS","famille")
			patterns  <- paste(pattern$ESS,pattern$famille,sep="")

			first_part  <-df%>% filter(DEP=="France entière" & typo_A_det=="Ensemble")
			first_part  <- first_part %>%filter(paste(ESS,famille,sep="") %in% patterns)  
			first_part[which(first_part$famille=="Ensemble"),]$famille   <-  "ESS"
			for (i in 1:nrow(first_part)){
				splitted  <- strsplit(first_part$typo_A[i],"[(]")[[1]]
				first_part$typo_A[i]  <- splitted[1]
			}


			first_part  <- data.frame(first_part$DEP,first_part$famille,first_part$typo_A,first_part[,getIndexCol("nb_etab",first_part)])

			colnames(first_part)  <- c("DEP","famille","typo_A","nb_etab")


			first_part_bis  <-  data.frame("DEP"=character(),"famille"=character(),"typo_A"=character(),"var"=numeric())
			for(fam in first_part$famille%>%unique ){
				tmp  <- first_part%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_A=="Ensemble des secteurs d'activité"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
				first_part_bis  <- rbind(first_part_bis,tmp)
			}
			first_part_bis <-  first_part_bis %>% filter(typo_A !="Ensemble des secteurs d'activité")

			df  <- df %>% filter(as.numeric(DEP)==as.numeric(code) &  typo_A_det=="Ensemble")
			df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns)  
			df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
			df  <-df%>% select(-typo_A_det,-ESS)
			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$typo_A[i],"[(]")[[1]]
				df$typo_A[i]  <- splitted[1]
			}
			df$DEP =rep(nom_DEP,nrow(df))
			df  <- data.frame(df$DEP,df$famille,df$typo_A,df[,getIndexCol("nb_etab",df)])
			colnames(df)  <- c("DEP","famille","typo_A","nb_etab")
			new_dat  <-  data.frame("DEP"=character(),"famille"=character(),"typo_A"=character(),"var"=numeric())
			for(fam in df$famille%>%unique ){
				tmp  <- df%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_A=="Ensemble des secteurs d'activité"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
				new_dat  <- rbind(new_dat,tmp)
			}

			new_dat  <-new_dat %>% filter(typo_A !="Ensemble des secteurs d'activité")

			data  <- rbind(new_dat,first_part_bis)

		}else{
			

			df  <- tables[["outputs/2017/EPCI_T2.csv"]]

			nom_EPCI  <- input$select_scale_det_tabs2_part1
			df  <- df %>%filter(nom_complet==nom_EPCI )
			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$jurid2[i],"TOTAL-")[[1]]
				df$jurid2[i]  <- splitted[2]
			}
			df$jurid2 %>%unique
			df  <-df %>% filter(jurid2 != "ASSOCIATIONS")
			# Seulement 2 modalité : TOTAL-ESS / TOTAL HORS ESS
			df  <- df %>% select(-EPCI)

			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$typo1[i],"-")[[1]]
				if(splitted%>% length >1){
	
					df$typo1[i]  <- splitted[2]
				}else{

					df$typo1[i]  <- splitted[1]
				}
			}
			df[which(df$typo1=="NON"),]$typo1  <- "NON CLASSES"


			df  <- data.frame(df$nom_complet,df$jurid2,df$typo1,df[,getIndexCol("nb_etab",df)])
			colnames(df)  <- c("EPCI","famille","typo1","nb_etab")
			new_dat  <-  data.frame("EPCI"=character(),"famille"=character(),"typo1"=character(),"var"=numeric())

			for(fam in df$famille%>%unique ){
				tmp  <- df%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo1=="TOUS SECTEURS"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
				new_dat  <- rbind(new_dat,tmp)
			}
			new_dat  <- new_dat%>%filter(typo1!="TOUS SECTEURS")
			data  <- new_dat
		}
		return(data)
			
	})


	output$plot_tabs2_part1_1  <- renderD3({

		df  <- data_tabs2_part1()
		print("hello there")
		if( input$select_scale_tabs2_part1!="REG"){
			df  <- df %>% filter(REG==input$select_scale_det_tabs2_part1 &  famille==input$select_ESS_tabs2_part1) %>%select(-REG,-famille)
		}else if(input$select_scale_tabs2_part1=="DEP"){
			df  <- df %>% filter(DEP==input$select_scale_det_tabs2_part1 & famille==input$select_ESS_tabs2_part1)%>% select(-DEP,-famille)


		}else{
			# HORS ESS et ESS
			df  <-  df%>% filter(famille==input$select_ESS_tabs2_part1)%>% select(-EPCI,-famille)

		}
		print(df)

		
		df  <- jsonlite::toJSON(dd,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
		print("plot done")
		r2d3(
			 data=df,

			 script="www/assets/barplot_classic.js",
			 options=list(
						  background_color ='rgb(215, 245, 255)',
						  title=paste("Nombre Etablissement",input$select_year_tabs2_part1,sep=" "),
						  var_name="Nombre établissements",
						  short_var_name="étab",
						  year=input$select_year_nb_etab

			 )
		)
		
	})
	observe({
		if(input$select_scale_tabs2_part1!="EPCI"){
			choice=c("ESS","Privée Hors ESS","Public")
		}else{
			choice=c("ESS","HORS ESS")
		}
		updateSelectInput(session,"select_ESS_tabs2_part1",choices=choice)
	})






	observe({
		if(input$select_scale_tabs2_part1=="REG"){
			choice=c(overAll_filter_REG)
		}else if(input$select_scale_tabs2_part1=="DEP"){
			choice=available_DEP$nom

		}else{
			choice=available_EPCI$nom_complet
		}
		updateSelectInput(session,"select_scale_det_tabs2_part1",choices=choice)
	})







    #---------------------------------
    #   Dashboard part.2
    # output$barPlot_famille_etab <- renderD3({
    #     tmp_data <- TC13_dashboard_bis() %>% select(famille, nb_etab)
    #     tmp_data<-tmp_data %>% filter(tolower(famille)!="ensemble")
    #     tmp_data$nb_etab<-as.numeric(tmp_data$nb_etab)
    #     colnames(tmp_data)<-c("country","value")
    #     tmp_data <-jsonlite::toJSON(tmp_data,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
    #     r2d3(
    #         data = tmp_data,
    #         script ="www/assets/barplot_classic.js",
    #         options = list(
    #             background_color = 'rgb(215, 245, 255)',
    #             title=paste("Nombre établissements par forme juridique (",input$dashboard_year_select_part2,")",sep=""),
    #             var_name="Nombre établissements",
    #             short_var_name="étab",
    #             year=input$dashboard_year_select_part2
    #         )
    #     )
    # })



}


shinyApp(
    ui = htmlTemplate(
        "www/index_bis.html",
        pieChart_etab = pieChart_etab,
        pieChart_rem = pieChart_rem,
        pieChart_emploi = pieChart_emploi,
		pieChart_effEQTP = pieChart_effEQTP,
		pieChart_ent = pieChart_ent,
        dashboard_select_Input_YEARS_key=dashboard_select_Input_YEARS_key,
		#---------------------------------
		# DAHSBOARD PART 2
		select_year_nb_etab=select_year_nb_etab,
		select_scale_det=select_scale_det_REG,
		plot_dashboard_part_2=plot_dashboard_part_2,
		#----------------------------------
		# TABS2 - PART 1
		select_year_tabs2_part1=select_year_tabs2_part1,
		select_ESS_tabs2_part1=construct_select_box(c("ESS","Privé Hors ESS","Public"),"select_ESS_tabs2_part1"),
		select_scale_det_tabs2_part1=construct_select_box(c(overAll_filter_REG),"select_scale_det_tabs2_part1"),
		plot_tabs2_part1_1=plot_tabs2_part1_1
        ),
    server = server
)
