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
var_to_title  <- list(
					  "nb_etab"="Nombre d'établissements",
					  "eff_31"="Les effectifs aux 31/12/",
					  "eff_EQTP"="Les effectifs equivalents temps plein",
					  "nb_ent"="Nombre d'entreprises",
					  "rem_brut"="Les rémunérations brutes"
)

var_to_short  <-  list(
					  "nb_etab"="etb",
					  "eff_31"="eff",
					  "eff_EQTP"="eff EQTP",
					  "nb_ent"="ent",
					  "rem_brut"="€ rem"
)



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
plot_tabs2_part1_1 =d3Output("plot_tabs2_part1_1",height = "100%",width="90%")
plot_tabs2_part1_2 =d3Output("plot_tabs2_part1_2",height = "100%",width="90%")
plot_tabs2_part2_1 =d3Output("plot_tabs2_part2_1",height = "100%",width="90%")
plot_tabs2_part2_2 =d3Output("plot_tabs2_part2_2",height = "100%",width="90%")
plot_tabs2_part3_1 =d3Output("plot_tabs2_part3_1",height = "100%",width="90%")
plot_tabs2_part3_2 =d3Output("plot_tabs2_part3_2",height = "100%",width="90%")


selectbox_HTML=""
for (choice in list.dirs("./outputs/", recursive = FALSE, full.names = FALSE) ){
    selectbox_HTML=paste(selectbox_HTML,'<option value="',choice,'">',choice,'</option>',sep="")
}

dashboard_select_Input_YEARS_key=HTML(paste('<select id="dashboard_year_select_key">',selectbox_HTML,'</select>',sep=""))

select_year_tabs2_head=HTML(paste('<select id="select_year_tabs2_head" class="select_box_custom">',selectbox_HTML,'</select>',sep=""))



select_year_tabs2_part1=HTML(paste('<select id="select_year_tabs2_part1" class="select_box_custom">',selectbox_HTML,'</select>',sep=""))

select_year_tabs2_part2=HTML(paste('<select id="select_year_tabs2_part2" class="select_box_custom">',selectbox_HTML,'</select>',sep=""))


select_year_tabs2_part3=HTML(paste('<select id="select_year_tabs2_part3" class="select_box_custom">',selectbox_HTML,'</select>',sep=""))

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






	data_tabs2_head	  <- reactive({

		nameTable = ""
		year  <- input$select_year_tabs2_head
		scale  <- input$select_scale_tabs2_head
		varLab  <- input$select_var_tabs2_head

		if(scale=="REG"){
			nameTable="TC13.csv"
		}else if(scale=="DEP"){
			nameTable="TC1.csv"
		}else{
			nameTable="EPCI_T1.csv"
		}

        PATH = paste("outputs/",year,"/",nameTable,sep="")
		print(PATH)
		
		df  <- tables[[PATH]]

		print(scale)
		if(scale=="REG"){
			
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
			first_part <-  rbind(c("moyenne",round(mean(tmp[,getIndexCol(varLab,tmp)])),3),first_part)
			data  <- rbind(first_part,second_part)

		}else if(scale=="DEP"){

			tmp  <-  df %>%filter( DEP !="France entière" & famille=="Ensemble"  & ESS=="ESS" & typo_A=="Ensemble des secteurs d'activité" & typo_A_det=="Ensemble") %>% select(-typo_A,-typo_A_det,-famille,-ESS) %>%filter(as.numeric(DEP) %in% as.numeric(available_DEP$code)) 

			tmp  <- data.frame(tmp$DEP,tmp[,getIndexCol(varLab,tmp)])
			colnames(tmp)  <- c("DEP",varLab)

			code  <-available_DEP%>%filter(nom==input$select_scale_det_tabs2_head)%>% select(code) 
			code  <- code[[1]]
			first_part  <- df %>% filter(as.numeric(DEP)==as.numeric(code) &  famille=="Ensemble" & ESS=="ESS" & typo_A=="Ensemble des secteurs d'activité" & typo_A_det=="Ensemble")%>% select(-typo_A,-typo_A_det,-famille,-ESS)

			first_part  <- data.frame(first_part$DEP,first_part[,getIndexCol(varLab,first_part)])
			colnames(first_part)  <- c("DEP",varLab)

			first_part[1,1] =input$select_scale_det_tabs2_head
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
			first_part  <- rbind(c("moyenne",round(mean(tmp[,getIndexCol(varLab,tmp)])),3),first_part)
			data  <- rbind(first_part,second_part)

		}else{






			tmp  <- df %>% filter(nom_complet %in% available_EPCI$nom_complet & jurid1=="4-ESS" ) %>%
				select(-EPCI,-jurid1, -dep_epci)


			first_part  <-  df%>%filter(nom_complet ==input$select_scale_det_tabs2_head & jurid1=="4-ESS")
			first_part  <- data.frame(first_part$nom_complet,first_part[,getIndexCol(varLab,first_part)])
			colnames(first_part)=c("nom_complet",varLab)


			second_part  <- df%>% filter(nom_complet %in% available_EPCI$nom_complet & jurid1=="4-ESS")%>% filter(nom_complet!=input$select_scale_det_tabs2_head)

			second_part  <- data.frame(second_part$nom_complet,second_part[,getIndexCol(varLab,second_part)])
			colnames(second_part)  <- c("nom_complet",varLab)
			second_part  <- arrange(second_part,desc(second_part[,getIndexCol(varLab,second_part)]))
			second_part  <- second_part %>% head(3)
			first_part  <- rbind(c("moyenne",round(mean(tmp[,getIndexCol(varLab,tmp)])),3),first_part)
			data  <- rbind(first_part,second_part)
		}


		return(data)


	})

	output$plot_tabs2_head  <- renderPlot({
		df  <- data_tabs2_head()
		colnames(df)  <-  c("country","value")
		print(df)
		varLab  <- input$select_var_tabs2_head
		
		g  <- ggplot(df,aes(country,value,fill=country))
		g  <- g+ geom_col()+
			xlab(" Régions/Moyenne")+
			ylab(var_to_short[[varLab]])+
			ggtitle(paste(var_to_title[[varLab]],"en",input$select_scale_det_tabs2_head,"(",input$select_year,")",sep=" "))+
			guides(fill="none")+
			scale_fill_hue()+ theme (axis.text.x=element_text(face="bold",size=10,angle=90))

		return(g)
	})







	observe({
		if(input$select_scale_tabs2_head=="REG"){
			choice=c(overAll_filter_REG)
		}else if(input$select_scale_tabs2_head=="DEP"){
			choice=available_DEP$nom

		}else{
			choice=available_EPCI$nom_complet
		}
		updateSelectInput(session,"select_scale_det_tabs2_head",choices=choice)
	})







	output$export_csv_dashboard_part2  <- downloadHandler(
													 filename=function(){
														 paste(input$select_var_tabs2_head,input$select_year_tabs2_head,"par_secteurs_scale=",input$select_scale_det_tabs2_head,".csv",sep="")

													 },
													 content = function(file){
														 write.csv(data_tabs2_head(),file,row.names= FALSE,fileEncoding="latin1")
													 }
	)














	#----------------------------------------
	# TAB2 - PART 1
	#----------------------------------------


	data_tabs2_part1  <- reactive({
		year  <- input$select_year_tabs2_part1
		scale  <- input$select_scale_tabs2_part1
		varLab  <- input$select_var_tabs2_part1
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


			first_part  <- data.frame(first_part$REG,first_part$famille,first_part$typo_B,first_part[,getIndexCol(varLab,first_part)])

			colnames(first_part)  <- c("REG","famille","typo_B",varLab)


			first_part_bis  <-  data.frame("REG"=character(),"famille"=character(),"typo_B"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in first_part$famille%>%unique ){
				tmp  <- first_part%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_B=="Ensemble des secteurs d'activité"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
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


			df  <- data.frame(df$REG,df$famille,df$typo_B,df[,getIndexCol(varLab,df)])
			colnames(df)  <- c("REG","famille","typo_B",varLab)
			new_dat  <-  data.frame("REG"=character(),"famille"=character(),"typo_B"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in df$famille%>%unique ){
				tmp  <- df%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_B=="Ensemble des secteurs d'activité"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
				}
			new_dat  <- rbind(new_dat,tmp)
			}

			new_dat  <-new_dat %>% filter(typo_B !="Ensemble des secteurs d'activité")
			data <- rbind(new_dat,first_part_bis)
	
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


			first_part  <- data.frame(first_part$DEP,first_part$famille,first_part$typo_A,first_part[,getIndexCol(varLab,first_part)])

			colnames(first_part)  <- c("DEP","famille","typo_A",varLab)


			first_part_bis  <-  data.frame("DEP"=character(),"famille"=character(),"typo_A"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in first_part$famille%>%unique ){
				tmp  <- first_part%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_A=="Ensemble des secteurs d'activité"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
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
			df  <- data.frame(df$DEP,df$famille,df$typo_A,df[,getIndexCol(varLab,df)])
			colnames(df)  <- c("DEP","famille","typo_A",varLab)
			new_dat  <-  data.frame("DEP"=character(),"famille"=character(),"typo_A"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in df$famille%>%unique ){
				tmp  <- df%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_A=="Ensemble des secteurs d'activité"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
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


			df  <- data.frame(df$nom_complet,df$jurid2,df$typo1,df[,getIndexCol(varLab,df)])
			colnames(df)  <- c("EPCI","famille","typo1",varLab)
			new_dat  <-  data.frame("EPCI"=character(),"famille"=character(),"typo1"=character(),"var"=numeric(),"prct"=numeric())

			for(fam in df$famille%>%unique ){
				tmp  <- df%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo1=="TOUS SECTEURS"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
				}
				new_dat  <- rbind(new_dat,tmp)
			}
			new_dat  <- new_dat%>%filter(typo1!="TOUS SECTEURS")
			data  <- new_dat
		}
		return(data)
			
	})


	output$plot_tabs2_part1_1  <- renderD3({
		varLab  <- input$select_var_tabs2_part1

		df  <- data_tabs2_part1()
		if( input$select_scale_tabs2_part1=="REG"){
			df  <- df %>% filter(REG==input$select_scale_det_tabs2_part1 &  famille==input$select_ESS_tabs2_part1) %>%select(-REG,-famille)
		}else if(input$select_scale_tabs2_part1=="DEP"){
			df  <- df %>% filter(DEP==input$select_scale_det_tabs2_part1 & famille==input$select_ESS_tabs2_part1)%>% select(-DEP,-famille)


		}else{
			# HORS ESS et ESS
			df  <-  df%>% filter(famille==input$select_ESS_tabs2_part1)%>% select(-EPCI,-famille)

		}
		if(input$prct_part1){
			df  <-  df[,-getIndexCol(varLab,df)]
		}else{
			df  <-  df %>% select(-prct)
		}


		colnames(df) <- c("country","value")
		
		df  <- jsonlite::toJSON(df,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
		r2d3(
			 data=df,

			 script="www/assets/barplot_classic.js",
			 options=list(
						  background_color ='rgb(215, 245, 255)',
						  title=paste(var_to_title[[varLab]],"par secteur d'activité","en",input$select_scale_det_tabs2_part1,"(",input$select_year_tabs2_part1,")",sep=" "),
						  var_name=var_to_short[[varLab]],
						  short_var_name=var_to_short[[varLab]],
						  year=input$select_year_tabs2_part1

			 )
		)
		
	})











	output$plot_tabs2_part1_2  <- renderD3({
		varLab  <- input$select_var_tabs2_part1

		df  <- data_tabs2_part1()
		if( input$select_scale_tabs2_part1=="REG"){
			df  <- df %>% filter(REG=="France entière" &  famille==input$select_ESS_tabs2_part1) %>%select(-REG,-famille)
		}else if(input$select_scale_tabs2_part1=="DEP"){
			df  <- df %>% filter(DEP=="France entière" & famille==input$select_ESS_tabs2_part1)%>% select(-DEP,-famille)


		}else{
			# HORS ESS et ESS
			PATH  <- paste("outputs/",input$select_year_tabs2_part1,"/TC13.csv",sep="")
			df  <- tables[[PATH]]


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


			first_part  <- data.frame(first_part$REG,first_part$famille,first_part$typo_B,first_part[,getIndexCol(varLab,first_part)])

			colnames(first_part)  <- c("REG","famille","typo_B",varLab)


			first_part_bis  <-  data.frame("REG"=character(),"famille"=character(),"typo_B"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in first_part$famille%>%unique ){
				tmp  <- first_part%>% filter(famille==fam)
				tot  <- tmp[which(tmp$typo_B=="Ensemble des secteurs d'activité"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
				}
				first_part_bis  <- rbind(first_part_bis,tmp)
			}
			ESS_filtre  <- ""
			if(input$select_ESS_tabs2_part1=="ESS"){
				ESS_filtre  <- "ESS"
			}else{
				ESS_filtre  <- "Public"
			}
			
			first_part_bis  <- first_part_bis %>% filter(typo_B!="Ensemble des secteurs d'activité")
			first_part_bis  <- 	first_part_bis %>% filter(famille==ESS_filtre)
			df  <- data.frame(first_part_bis$typo_B,first_part_bis[,getIndexCol(varLab,first_part_bis)],first_part_bis$prct)
			colnames(df)  <- c("typo_B",varLab,"prct")

		}
		if(input$prct_part1){
			df  <-  df[,-getIndexCol(varLab,df)]
		}else{
			df  <-  df %>% select(-prct)
		}

		colnames(df) <- c("country","value")
		
		df  <- jsonlite::toJSON(df,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
		r2d3(
			 data=df,

			 script="www/assets/barplot_classic.js",
			 options=list(
						  background_color ='rgb(215, 245, 255)',
						  title=paste(var_to_title[[varLab]],"par secteur d'activité","en France",input$select_scale_det_tabs2_part1,"(",input$select_year_tabs2_part1,")",sep=" "),
						  var_name=var_to_short[[varLab]],
						  short_var_name=var_to_short[[varLab]],
						  year=input$select_year_tabs2_part1

			 )
		)
		
	})




	observe({
		if(input$select_scale_tabs2_part1=="REG"){
			choice=c("ESS","Privé Hors ESS","Public")
		}else if(input$select_scale_tabs2_part1=="EPCI"){
			choice=c("ESS","HORS ESS")
		}
		else{
			choice=c("ESS","Public","Privé Hors ESS")
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

	
	

	output$export_csv_tab2_part1  <- downloadHandler(
													 filename=function(){
														 paste("Repartition","nb_etab",input$select_year_tabs2_part2,"par_secteurs_scale=",input$select_scale_det_tabs2_part2,".csv",sep="")

													 },
													 content = function(file){
														 write.csv(data_tabs2_part2(),file,row.names= FALSE,fileEncoding="latin1")
													 }
	)




















	#----------------------------------------
	# TAB2 - PART 2
	#----------------------------------------

	data_tabs2_part2  <- reactive({
		year <-  input$select_year_tabs2_part2
		scale  <-  input$select_scale_tabs2_part2
		varLab  <- input$select_var_tabs2_part2
		id_table  <- ""
		if(scale=="REG"){
			id_table  <- "TC13.csv"

		}else if(scale=="DEP"){

			id_table  <- "TC1.csv"
		}else{
			id_table  <- "EPCI_T1.csv"
		}

		PATH  <- paste("outputs/",year,"/",id_table,sep="") 
		df <- tables[[PATH]]


		if(scale=="REG"){


			first_part  <- df %>% filter(REG=="France entière" & typo_B_det=="Ensemble" & typo_B=="Ensemble des secteurs d'activité") %>% select(-typo_B,-typo_B_det)

			first_part  <- first_part %>% filter(ESS %in% c("ESS","ESS + Hors ESS")  )




			for (i in 1:nrow(first_part)){
					splitted  <- strsplit(first_part$famille[i],". ")[[1]]
					if (splitted %>% length==1){

						first_part$famille[i]  <- splitted[1]
					}else if(splitted%>%length ==2){
						first_part$famille[i]  <- splitted[2]
					}else if(splitted %>%length ==3){
						first_part$famille[i]  <- "Privé Hors ESS"
					}
			}



			first_part  <- data.frame(first_part$REG,first_part$ESS,first_part$famille,first_part[,getIndexCol(varLab,first_part)])

			colnames(first_part)  <- c("REG","ESS","famille",varLab)


			first_part_bis  <-  data.frame("REG"=character(),"ESS"=character(),"famille"=character(),"var"=numeric(),"prct"=numeric())
			for(ESS_val in first_part$ESS%>%unique ){
					tmp  <- first_part%>% filter(ESS==ESS_val)
					tot  <- tmp[which(tmp$famille=="Ensemble"),varLab]
					for (row in 1:nrow(tmp)){
						tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
					}
					first_part_bis  <- rbind(first_part_bis,tmp)
			}



			df  <- df %>% filter(REG=="Grand-Est" & typo_B_det=="Ensemble" & typo_B=="Ensemble des secteurs d'activité") %>% select(-typo_B,-typo_B_det)

			df  <- df %>% filter(ESS %in% c("ESS","ESS + Hors ESS") )


			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$famille[i],". ")[[1]]
				if (splitted %>% length==1){

					df$famille[i]  <- splitted[1]
				}else if(splitted%>%length ==2){
					df$famille[i]  <- splitted[2]
				}else if(splitted %>%length ==3){
					df$famille[i]  <- "Privé Hors ESS"
				}
			}

			df  <- data.frame(df$REG,df$ESS,df$famille,df[,getIndexCol(varLab,df)])

			colnames(df)  <- c("REG","ESS","famille",varLab)

			new_data  <-  data.frame("REG"=character(),"ESS"=character(),"famille"=character(),"var"=numeric(),"prct"=numeric())
			for(ESS_val in df$ESS%>%unique ){
				tmp  <- df%>% filter(ESS==ESS_val)
				tot  <- tmp[which(tmp$famille=="Ensemble"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
				}
				new_data  <- rbind(new_data,tmp)
			}
			data  <- rbind(first_part_bis,new_data)


		}else if(scale=="DEP"){
			nom_DEP  <-  input$select_scale_det_tabs2_part2
			
			code  <-available_DEP%>%filter(nom==nom_DEP)%>% select(code) 
			code  <- code[[1]]



			first_part  <- df %>% filter(DEP=="France entière" & typo_A_det=="Ensemble" & typo_A=="Ensemble des secteurs d'activité") %>% select(-typo_A,-typo_A_det)

			first_part  <- first_part %>% filter(ESS %in% c("ESS","ESS + Hors ESS")  )

			for (i in 1:nrow(first_part)){
				splitted  <- strsplit(first_part$famille[i],". ")[[1]]
				if (splitted %>% length==1){

					first_part$famille[i]  <- splitted[1]
				}else if(splitted%>%length ==2){
					first_part$famille[i]  <- splitted[2]
				}else if(splitted %>%length ==3){
					first_part$famille[i]  <- "Privé Hors ESS"
				}
			}
			first_part  <- data.frame(first_part$DEP,first_part$ESS,first_part$famille,first_part[,getIndexCol(varLab,first_part)])

			colnames(first_part)  <- c("DEP","ESS","famille",varLab)

			first_part_bis  <-  data.frame("DEP"=character(),"ESS"=character(),"famille"=character(),"var"=numeric(),"prct"=numeric())
			for(ESS_val in first_part$ESS%>%unique ){
					tmp  <- first_part%>% filter(ESS==ESS_val)
					tot  <- tmp[which(tmp$famille=="Ensemble"),varLab]
					for (row in 1:nrow(tmp)){
						tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
					}
					first_part_bis  <- rbind(first_part_bis,tmp)
			}



			df  <- df %>% filter(as.numeric(DEP)==as.numeric(code) & typo_A_det=="Ensemble" & typo_A=="Ensemble des secteurs d'activité") %>% select(-typo_A,-typo_A_det)
			df  <- df %>% filter(ESS %in% c("ESS","ESS + Hors ESS") )
			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$famille[i],". ")[[1]]
				if (splitted %>% length==1){

					df$famille[i]  <- splitted[1]
				}else if(splitted%>%length ==2){
					df$famille[i]  <- splitted[2]
				}else if(splitted %>%length ==3){
					df$famille[i]  <- "Privé Hors ESS"
				}
			}
			df  <- data.frame(df$DEP,df$ESS,df$famille,df[,getIndexCol(varLab,df)])

			colnames(df)  <- c("DEP","ESS","famille",varLab)
			df$DEP  <- rep(nom_DEP,nrow(df))

			new_data  <-  data.frame("DEP"=character(),"ESS"=character(),"famille"=character(),"var"=numeric(),"prct"=numeric())
			for(ESS_val in df$ESS%>%unique ){
					tmp  <- df%>% filter(ESS==ESS_val)
					tot  <- tmp[which(tmp$famille=="Ensemble"),varLab]
					for (row in 1:nrow(tmp)){
						tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
					}
					new_data  <- rbind(new_data,tmp)
			}
			data  <- rbind(first_part_bis,new_data)



		}else{

			nom_EPCI  <- input$select_scale_det_tabs2_part2
			df  <- df %>% filter(nom_complet==nom_EPCI & jurid1 %in%c("1-ASSO+FOND","2-COOPERATIVES","3-MUTUELLES","5A-PUBLIC","5B-PRIVE","6-TOTAL"))



			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$jurid1[i],"-")[[1]]
	
				df$jurid1[i]  <- splitted[2]
			}

			prct=rep(NA,nrow(df))
			df  <-  data.frame(df$nom_complet,df$jurid1,df[,getIndexCol(varLab,df)],prct)

			colnames(df) <- c("EPCI","famille",varLab,"prct")


			tot  <- df[which(df$famille=="TOTAL"),varLab]
			for (row in 1:nrow(df)){
				df[row,"prct"] <- round(df[row,getIndexCol(varLab,df)] /tot,2)*100
			}
			data  <- df

		}
		return (data)


	}) 





	output$plot_tabs2_part2_1  <-  renderD3({

		varLab  <- input$select_var_tabs2_part2

		df  <-  data_tabs2_part2()

		if(input$select_scale_tabs2_part2 =="REG"){
			

			df  <- df %>% filter(REG==input$select_scale_det_tabs2_part2 & ESS==input$select_ESS_tabs2_part2 & famille !="Ensemble")%>% select(-REG,-ESS)
		
		}else if(input$select_scale_tabs2_part2 =="DEP"){

			df  <- df %>%filter(DEP==input$select_scale_det_tabs2_part2 & ESS==input$select_ESS_tabs2_part2 & famille !="Ensemble")%>% select(-DEP,-ESS)
		}else{

			df  <- df %>% filter(famille!="TOTAL")%>% select(-EPCI)
		}
		if(input$prct_part2){
			df  <- df[,-getIndexCol(varLab,df)]
		}else{
			df  <- df%>%select(-prct)
		}
		colnames(df) <- c("country","value")
		
		df  <- jsonlite::toJSON(df,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
		r2d3(
			 data=df,

			 script="www/assets/barplot_classic.js",
			 options=list(
						  background_color ='rgb(215, 245, 255)',
						  title=paste("Nombre Etablissement",input$select_year_tabs2_part2,sep=" "),
						  var_name="Nombre établissements",
						  short_var_name="étab",
						  year=input$select_year_tabs2_part2

			 )
		)



	})





	output$plot_tabs2_part2_2  <- renderD3({
		varLab  <- input$select_var_tabs2_part2
		year  <- input$select_year_tabs2_part2
		PATH  <- paste("outputs/",year,"/TC13.csv",sep="") 
		df <- tables[[PATH]]




			first_part  <- df %>% filter(REG=="France entière" & typo_B_det=="Ensemble" & typo_B=="Ensemble des secteurs d'activité") %>% select(-typo_B,-typo_B_det)

			first_part  <- first_part %>% filter(ESS %in% c("ESS","ESS + Hors ESS")  )




			for (i in 1:nrow(first_part)){
					splitted  <- strsplit(first_part$famille[i],". ")[[1]]
					if (splitted %>% length==1){

						first_part$famille[i]  <- splitted[1]
					}else if(splitted%>%length ==2){
						first_part$famille[i]  <- splitted[2]
					}else if(splitted %>%length ==3){
						first_part$famille[i]  <- "Privé Hors ESS"
					}
			}



			first_part  <- data.frame(first_part$REG,first_part$ESS,first_part$famille,first_part[,getIndexCol(varLab,first_part)])

			colnames(first_part)  <- c("REG","ESS","famille",varLab)


			first_part_bis  <-  data.frame("REG"=character(),"ESS"=character(),"famille"=character(),"var"=numeric(),"prct"=numeric())
			for(ESS_val in first_part$ESS%>%unique ){
					tmp  <- first_part%>% filter(ESS==ESS_val)
					tot  <- tmp[which(tmp$famille=="Ensemble"),varLab]
					for (row in 1:nrow(tmp)){
						tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
					}
					first_part_bis  <- rbind(first_part_bis,tmp)
			}
			if (input$select_scale_tabs2_part2!="EPCI"){
				filter_ESS =input$select_ESS_tabs2_part2
			}else{
				filter_ESS="ESS"

			}


			first_part_bis  <- first_part_bis %>% filter (ESS==filter_ESS)
			df  <- first_part_bis %>% select(-REG,-ESS) %>% filter(famille !="Ensemble")
			
			if(input$prct_part2){
				df  <- df[,-getIndexCol(varLab,df)]
			}else{
				df  <-  df %>% select(-prct)
			}
			colnames(df)  <-  c("country","value")

			df  <- jsonlite::toJSON(df,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
			r2d3(
				 data=df,

				script="www/assets/barplot_classic.js",
				options=list(
						  background_color ='rgb(215, 245, 255)',
						  title=paste("Nombre Etablissement",input$select_year_tabs2_part2,sep=" "),
						  var_name="Nombre établissements",
						  short_var_name="étab",
						  year=input$select_year_tabs2_part2

			 )
		)

	})





	observe({
		if(input$select_scale_tabs2_part2=="REG"){
			choice=c("ESS","ESS + Hors ESS")
		}else if(input$select_scale_tabs2_part2=="DEP"){
			choice=c("ESS","ESS + Hors ESS")
		}
		else{
			choice=c("-")
		}
		updateSelectInput(session,"select_ESS_tabs2_part2",choices=choice)
	})






	observe({
		if(input$select_scale_tabs2_part2=="REG"){
			choice=c(overAll_filter_REG)
		}else if(input$select_scale_tabs2_part2=="DEP"){
			choice=available_DEP$nom

		}else{
			choice=available_EPCI$nom_complet
		}
		updateSelectInput(session,"select_scale_det_tabs2_part2",choices=choice)
	})



	output$export_csv_tab2_part2  <- downloadHandler(
													 filename=function(){
														 paste("Repartition",input$select_var_tabs2_part2,input$select_year_tabs2_part2,"par_famille_juridique_scale=",input$select_scale_det_tabs2_part2,".csv",sep="")

													 },
													 content = function(file){
														 write.csv(data_tabs2_part2(),file,row.names= FALSE,fileEncoding="latin1")
													 }
	)











	#-------------------------------------------
	#
	#    TABS2 - PART3
	#
	#-------------------------------------------

	data_tabs2_part3  <- reactive({
		year  <- input$select_year_tabs2_part3
		scale  <-  input$select_scale_tabs2_part3
		varLab  <-  input$select_var_tabs2_part3
		id_table  <- ""
		if(scale=="REG"){
			id_table  <- "TC14.csv"

		}else if(scale=="DEP"){

			id_table  <- "TC2.csv"
		}
		PATH  <- paste("outputs/",year,"/",id_table,sep="") 
		df <- tables[[PATH]]
		if(scale =="REG"){

			pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
			colnames(pattern)  <-  c("ESS","famille")
			patterns  <- paste(pattern$ESS,pattern$famille,sep="")


			first_part  <-df%>% filter(REG=="France entière")

			first_part  <- first_part %>%filter(paste(ESS,famille,sep="") %in% patterns)  

			first_part[which(first_part$famille=="Ensemble"),]$famille   <-  "ESS"
			for (i in 1:nrow(first_part)){
				splitted  <- strsplit(first_part$taille_etab[i],"- ")[[1]]
				if(length(splitted)==1){
					first_part$taille_etab[i]  <- splitted[1]
				}else{
					first_part$taille_etab[i]  <- splitted[2]
				}
			}


			first_part  <- data.frame(first_part$REG,first_part$famille,first_part$taille_etab,first_part[,getIndexCol(varLab,first_part)])

			colnames(first_part)  <- c("REG","famille","taille_etab",varLab)


			first_part_bis  <-  data.frame("REG"=character(),"famille"=character(),"taille_etab"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in first_part$famille%>%unique ){
				tmp  <- first_part%>% filter(famille==fam)
				tot  <- tmp[which(tmp$taille_etab=="Toutes entreprises"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
				}
				first_part_bis  <- rbind(first_part_bis,tmp)
			}




			df  <- df %>% filter(REG=="Grand-Est"  )
			df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns)  
			df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$taille_etab[i],"- ")[[1]]
				if(length(splitted)==1){
					df$taille_etab[i]  <- splitted[1]
				}else{
					df$taille_etab[i]  <- splitted[2]
				}
			}


			df  <- data.frame(df$REG,df$famille,df$taille_etab,df[,getIndexCol(varLab,df)])
			colnames(df)  <- c("REG","famille","taille_etab",varLab)
			new_dat  <-  data.frame("REG"=character(),"famille"=character(),"taille_etab"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in df$famille%>%unique ){
				tmp  <- df%>% filter(famille==fam)
				tot  <- tmp[which(tmp$taille_etab=="Toutes entreprises"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
				}
				new_dat  <- rbind(new_dat,tmp)
			}

			data  <- rbind(new_dat,first_part_bis)
		}else if(scale=="DEP"){
			nom_DEP  <-  input$select_scale_det_tabs2_part3


			code  <-available_DEP%>%filter(nom==nom_DEP)%>% select(code) 
			code  <- code[[1]]


			pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
			colnames(pattern)  <-  c("ESS","famille")
			patterns  <- paste(pattern$ESS,pattern$famille,sep="")


			first_part  <-df%>% filter(DEP=="France entière")

			first_part  <- first_part %>%filter(paste(ESS,famille,sep="") %in% patterns)  

			first_part[which(first_part$famille=="Ensemble"),]$famille   <-  "ESS"
			for (i in 1:nrow(first_part)){
				splitted  <- strsplit(first_part$taille_etab[i],"- ")[[1]]
				if(length(splitted)==1){
					first_part$taille_etab[i]  <- splitted[1]
				}else{
					first_part$taille_etab[i]  <- splitted[2]
				}
			}


			first_part  <- data.frame(first_part$DEP,first_part$famille,first_part$taille_etab,first_part[,getIndexCol(varLab,first_part)])

			colnames(first_part)  <- c("DEP","famille","taille_etab",varLab)


			first_part_bis  <-  data.frame("DEP"=character(),"famille"=character(),"taille_etab"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in first_part$famille%>%unique ){
				tmp  <- first_part%>% filter(famille==fam)
				tot  <- tmp[which(tmp$taille_etab=="Toutes entreprises"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
				}
				first_part_bis  <- rbind(first_part_bis,tmp)
			}




			df  <- df %>% filter(as.numeric(DEP)==as.numeric(code)  )
			df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns)  
			df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
			for (i in 1:nrow(df)){
				splitted  <- strsplit(df$taille_etab[i],"- ")[[1]]
				if(length(splitted)==1){
					df$taille_etab[i]  <- splitted[1]
				}else{
					df$taille_etab[i]  <- splitted[2]
				}
			}
			df$DEP  <- rep(nom_DEP,nrow(df))

			df  <- data.frame(df$DEP,df$famille,df$taille_etab,df[,getIndexCol(varLab,df)])
			colnames(df)  <- c("DEP","famille","taille_etab",varLab)
			new_dat  <-  data.frame("DEP"=character(),"famille"=character(),"taille_etab"=character(),"var"=numeric(),"prct"=numeric())
			for(fam in df$famille%>%unique ){
				tmp  <- df%>% filter(famille==fam)
				tot  <- tmp[which(tmp$taille_etab=="Toutes entreprises"),varLab]
				for (row in 1:nrow(tmp)){
					tmp[row,"prct"] <- round(tmp[row,getIndexCol(varLab,tmp)] /tot,2)*100
				}
				new_dat  <- rbind(new_dat,tmp)
			}
			data  <- rbind(new_dat,first_part_bis)
		}
		return(data)


	})




	output$plot_tabs2_part3_1  <- renderD3({
		varLab  <- input$select_var_tabs2_part3
		df  <- data_tabs2_part3()

		if(input$select_scale_tabs2_part3 =="REG"){
			

			df  <- df %>% filter(REG==input$select_scale_det_tabs2_part3 & famille==input$select_ESS_tabs2_part3 & taille_etab !="Toutes entreprises")%>% select(-REG,-famille)

		
		}else if(input$select_scale_tabs2_part3 =="DEP"){

			df  <- df %>%filter(DEP==input$select_scale_det_tabs2_part3 & famille==input$select_ESS_tabs2_part3 & taille_etab !="Toutes entreprises")%>% select(-DEP,-famille)
		}

		if(input$prct_part3){

			df  <- data.frame(df$taille_etab,df$prct)
		}else{
			df  <- data.frame(df$taille_etab,df[,getIndexCol(varLab,df)])
		}
		colnames(df) <- c("country","value")
		df  <- jsonlite::toJSON(df,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
		r2d3(
			 data=df,

			 script="www/assets/barplot_classic.js",
			 options=list(
						  background_color ='rgb(215, 245, 255)',
						  title=paste("Nombre Etablissement",input$select_year_tabs2_part2,sep=" "),
						  var_name="Nombre établissements",
						  short_var_name="étab",
						  year=input$select_year_tabs2_part3

			 )
		)

	})




	output$plot_tabs2_part3_2  <- renderD3({

		varLab  <- input$select_var_tabs2_part3
		df  <- data_tabs2_part3()

		if(input$select_scale_tabs2_part3 =="REG"){
			

			df  <- df %>% filter(REG=="France entière" & famille==input$select_ESS_tabs2_part3 & taille_etab !="Toutes entreprises")%>% select(-REG,-famille)

		
		}else if(input$select_scale_tabs2_part3 =="DEP"){

			df  <- df %>%filter(DEP=="France entière" & famille==input$select_ESS_tabs2_part3 & taille_etab !="Toutes entreprises")%>% select(-DEP,-famille)
		}

		if(input$prct_part3){

			df  <- data.frame(df$taille_etab,df$prct)
		}else{
			df  <- data.frame(df$taille_etab,df[,getIndexCol(varLab,df)])
		}
		colnames(df) <- c("country","value")
		df  <- jsonlite::toJSON(df,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
		r2d3(
			 data=df,

			 script="www/assets/barplot_classic.js",
			 options=list(
						  background_color ='rgb(215, 245, 255)',
						  title=paste("Nombre Etablissement",input$select_year_tabs2_part2,sep=" "),
						  var_name="Nombre établissements",
						  short_var_name="étab",
						  year=input$select_year_tabs2_part3

			 )
		)
	})








	observe({
		if(input$select_scale_tabs2_part3=="REG"){
			choice=c("ESS","Privé Hors ESS","Public")
		}else{
			choice=c("ESS","Public","Privé Hors ESS")
		}
		updateSelectInput(session,"select_ESS_tabs2_part3",choices=choice)
	})


	observe({
		if(input$select_scale_tabs2_part3=="REG"){
			choice=c(overAll_filter_REG)
		}else if(input$select_scale_tabs2_part3=="DEP"){
			choice=available_DEP$nom

		}
		updateSelectInput(session,"select_scale_det_tabs2_part3",choices=choice)
	})



	output$export_csv_tab2_part3  <- downloadHandler(
													 filename=function(){
														 paste("Repartition","nb_etab",input$select_year_tabs2_part3,"par_taille_etablissements_scale=",input$select_scale_det_tabs2_part3,".csv",sep="")

													 },
													 content = function(file){
														 write.csv(data_tabs2_part3(),file,row.names= FALSE,fileEncoding="latin1")
													 }
	)

	




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
		select_year_tabs2_head=select_year_tabs2_head,
		select_scale_det_tabs2_head=construct_select_box(c(overAll_filter_REG),"select_scale_det_tabs2_head"),
		export_csv_dashboard_part2 = downloadButton("export_csv_dashboard_part2","Export data"),
		#----------------------------------
		# TABS2 - PART 1
		select_year_tabs2_part1=select_year_tabs2_part1,
		select_ESS_tabs2_part1=construct_select_box(c("ESS","Privé Hors ESS","Public"),"select_ESS_tabs2_part1"),
		select_scale_det_tabs2_part1=construct_select_box(c(overAll_filter_REG),"select_scale_det_tabs2_part1"),
		plot_tabs2_part1_1=plot_tabs2_part1_1,
		plot_tabs2_part1_2 =plot_tabs2_part1_2,
		export_csv_tab2_part1=downloadButton("export_csv_tab2_part1","Export data"),
		#----------------------------
		#TABS2 - PART 2	
		select_year_tabs2_part2=select_year_tabs2_part2,
		select_ESS_tabs2_part2=construct_select_box(c("ESS","ESS + Hors ESS"),"select_ESS_tabs2_part2"),
		select_scale_det_tabs2_part2=construct_select_box(c(overAll_filter_REG),"select_scale_det_tabs2_part2"),
		plot_tabs2_part2_1= plot_tabs2_part2_1,
		export_csv_tab2_part2=downloadButton("export_csv_tab2_part2","Export data"),
		plot_tabs2_part2_2 = plot_tabs2_part2_2,
		#--------------------------------
		# TABS2 - PART3

		select_ESS_tabs2_part3=construct_select_box(c("ESS","Privé Hors ESS","Public"),"select_ESS_tabs2_part3"),
		select_year_tabs2_part3=select_year_tabs2_part3,
		select_scale_det_tabs2_part3=construct_select_box(c(overAll_filter_REG),"select_scale_det_tabs2_part3"),
		plot_tabs2_part3_1=plot_tabs2_part3_1,
		export_csv_tab2_part3=downloadButton("export_csv_tab2_part3","Export data"),
		plot_tabs2_part3_2=plot_tabs2_part3_2
        ),
    server = server
)
