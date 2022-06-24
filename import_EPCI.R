rm(list=ls())
library("readxl")# Can read xls and xlsx files with the same function read_excel
library(tidyverse)
library(dplyr)
library(readODS)

#--------------------------------

# Let work only on one dir for the moment

directories <- list.dirs("./inputs/", recursive = FALSE, full.names = FALSE)
dir <- directories[1]



#----------------------------------------
# Step 1 : Read every EPCI file + decoupage_geo.
# Make a list containing each colnames


geo_file <- list.files(
        paste("./inputs/", dir, "/decoupage_geo", sep = ""),
        all.files = FALSE
)
PATH <- paste("./inputs/", dir, "/decoupage_geo/", geo_file, sep = "")
geo_file <- read_excel(PATH)
geo_file<-geo_file %>% select(dep_epci,siren_epci,nom_complet) %>% unique
colnames(geo_file)[which(colnames(geo_file) == "siren_epci")] <- "EPCI"

EPCI_files <- list.files(
    paste("./inputs/", dir, "/infra_dep", sep = ""),
    all.files = FALSE
)

tables <- list()
labels <-c()
cols <-list()
for (file in EPCI_files) {
    PATH <- paste("./inputs/",dir, "/infra_dep/",file,sep= "")
    nb_sheet <- get_num_sheets_in_ods(PATH)
    for (sheet in 1: nb_sheet) {
        tmp_table <- read_ods(path = PATH, sheet = sheet)
        labels <- c(labels,paste(file, sheet,sep = "/"))
        tables <- append(tables, list(tmp_table))
        cols <- append(cols, list(colnames(tmp_table)))
    }
}
names(tables) <- labels
names(cols) <- labels

checkpoint_cols <-cols
checkpoint_tables <-tables

#cols=NULL
#tables=NULL
#cols=checkpoint_cols
#tables= checkpoint_tables

#-------------------------------
# Step 2 : Detect EPCI var

counter=1
del_index <- c()
for (col in cols){
    ind_EPCI <- which(str_detect(col,"EPCI|epci"))
    
    if(length(ind_EPCI) == 0){
        del_index <- c(del_index,counter)
    }else{
        col[ind_EPCI] <- "EPCI"
        if(ind_EPCI != 1 ){
            tmp_table<-tables[[counter]]
            EPCI_col <- tmp_table[,ind_EPCI]
            tmp_table<-cbind(EPCI_col,tmp_table[, -ind_EPCI]) %>% as.data.frame
            tables[[counter]]<-tmp_table
            cols[[counter]]<-c("EPCI",col[-ind_EPCI])
        }
    }
    counter= counter+1
}
cols[del_index] <-rep(NULL,length(del_index))
tables[del_index] <-rep(NULL,length(del_index))


#--------------------------------------
# Step 3 : Detect Interest variables and rename them

counter=1
for (col in cols) {
    index_nb_etab <- wich( str_detect(col,"nb_etab"))
    index_eff31 <- wich( str_detect(col,"EFF31"))
    index_effEQTP <- wich( str_detect(col,"effEQTP"))
    index_rembrut <- wich(str_detect(col,"Rembrut"))
    index_non_annexe <- wich( str_detect(col,"NON_Annexe"))
    indexes <- c(index_nb_etab,
                    index_eff_31,
                    index_effEQTP,
                    index_rembrut,
                    index_non_annexe
    )
    indexes_list=list(
        "nb_etab"=index_nb_etab
        "eff_31"=index_eff31
        "eff_EQTP"=index_effEQTP
        "rem_brut"=index_rembrut
        "non_annexe"=index_non_annexe
    )

    for (i in 1: length(indexes_list)){
        if(indexes_list[[i]] %>% length >0){
            col[indexes_list[[i]]] <-names(indexes_list)[i]
        }
    }

    
    index_min <- min(indexes)
    del_ind<-c()
    for ( i in index_min: length(col)){
        if( !(i %in% indexes)){
            del_ind<-c(del_ind,i)
        }
    }
    
    col=col[-del_ind]
    cols[[counter]] = col
    counter=counter+1
}


#----------------------------------------
# Step 4 : Detect jurid variables



#-----------------------------------------
#Read EPCI 1 et EPCI 2 de la dir

files <- list.files(
        paste("./inputs/", dir, "/infra_dep", sep = ""),
        all.files = FALSE
)

file <- files[2]
PATH <- paste("./inputs/", dir, "/infra_dep/", file, sep = "")

#// TO DO : Plusieurs formats possible pour les fichiers EPCI


nb_sheet <-get_num_sheets_in_ods(PATH)
# Parcourir les differentes feuilles 
list_table <- c()
list_names <-c()
for (sheet in 1:nb_sheet){
    tmp_table <- read_ods(path=PATH,sheet = sheet)
    list_table <-append(list_table,list(colnames(tmp_table)))
    list_names <- c(list_names,paste(file,sheet,sep="/"))
}
names(list_table) =list_names

list_table
#Liste table contient la localisation de la table + le nom des variables présentes


#-----------------------
# Detect I and EPCI vars
res <-rep(NA,length(list_table))

counter=1
for (el in list_table){
    ind_EPCI <- which(str_detect(el,"EPCI|epci"))
    print(ind_EPCI)
    print(length(ind_EPCI))
    print("--------------")
    if(length(ind_EPCI)==0){
        res[counter]="out"
    }
    counter=counter+1
}







tmp_table=read_ods(PATH)
tmp_table = left_join(tmp_table,data_decoup_geo,by="EPCI")
write.csv(tmp_table,paste("./outputs/",filename,sep=""))

#On load tous les fichier présents!


#---------------------------------------------------------------------
detect_var(data){
    vars=colnames(data)
    ind_EPCI_pot<- which(
        str_detect(vars,"EPCI|epci")
    )
    ind_jurid_2 <- which(
        str_detect(vars,"jurid2|juridr")
    )
    ind_jurid_1<- which(
        str_detect(vars,"jurid")
    )

}

pattern=c("juridr","jurid2",colnames(tmp_table))
str_detect(pattern,"jurid$")#ne detect que juri
str_detect(pattern,"jurid")