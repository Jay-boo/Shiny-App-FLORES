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
# Step 3 : Detect Interest variables and rename them  + remove useless vars



cols<-append(cols,list(
    "test"=c("EPCI",)
))
sapply(tables,FUN=function(i){return(colnames(i))})

counter=1
for (col in cols) {
    index_nb_etab <- which(
         str_detect(col,"Etab|etb|étab|ETAB")
    )
    index_eff31 <- which(
         str_detect(col,"eff|EFF|Eff") & str_detect(col,"31")
    )

    index_effEQTP <- which(
        str_detect(col,"eff|EFF|Eff") & !str_detect(col,"31") & str_detect(col,"EQTP|Eqtp|eqtp")
    )

    index_rembrut <- which(
        str_detect(col,"REMBRUT|rembrut|REMBrut|RemBrut|remBrut")
    )
    index_non_annexe <- which(
        str_detect(col,"NON_ANNEXE|non_annexe|Non_Annexe|Non_annexe|non_annex|Non_annex")
    )

    indexes <- c(index_nb_etab,
                    index_eff31,
                    index_effEQTP,
                    index_rembrut,
                    index_non_annexe
    )
    indexes_list=list(
        "nb_etab"=index_nb_etab,
        "eff_31"=index_eff31,
        "eff_EQTP"=index_effEQTP,
        "rem_brut"=index_rembrut,
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
    
    if(!is.null(del_ind)){
        col=col[-del_ind]
        #Needed to delete the column in the table too
        table=tables[[counter]]
        table <-table[,-del_ind]
        tables[[counter]] = table

    }
    cols[[counter]] = col
    
    counter=counter+1
}



#----------------------------------------
# Step 4 : Detect jurid variables +rename jurid1 et jurid2

jurid_vars<-list()
jurid_vars_names<-sapply(cols,FUN=function(i){
    return(i[str_detect(i,"jur|Jur|JUR")])
}) %>% unique

while(length(jurid_vars) < length(jurid_vars_names)){
    jurid_vars <-append(jurid_vars,list(c()))
}
names(jurid_vars)=jurid_vars_names

rename<-c()
counter=1
while(length(rename) < length(jurid_vars)){
    rename <- c(rename,paste("jurid",counter,sep=""))
    counter= counter+1
}

del_index <- c()
counter=1
for (col in cols){
    ind_jurid_var <- which(str_detect(col,"jurid"))
    
    if (ind_jurid_var %>% length ==1){
        lab <-col[ind_jurid_var]
        col[ind_jurid_var] <-rename[which(names(jurid_vars)==lab)]
        cols[[counter]]<-col
        jurid_vars[[lab]] <- c(jurid_vars[[lab]],names(cols)[counter])
    }else{
        #On enleve la table
        del_index <-c(del_index,counter)
    }
    counter=counter+1
}
cols[del_index] <- rep(NULL,length(del_index))
tables[del_index] <- rep(NULL,length(del_index))
names(jurid_vars) <- rename

#------------------------------------------------------
# Step 5 : Detect typo vars +  Rename typo1 and typo2

jurid_vars_names<-sapply(cols,FUN=function(i){
    return(i[str_detect(i,"jur|Jur|JUR")])
}) %>% unique









#---------------------------------------------------------
# Step 6 : Detect EPCI info missing and fix it + Merge the good tables

# Regarder juste sur il existe un trio de variable LIBGEO, DEP, nom
# Si non on enleve toute les variables en trop comme on a rename toutes les autres variables importantes 
# il sera facile de les identifiées
 

# Rmrq : Pas prevu d'avoir des variables d'interets differentes
# entre les tables d'une meme directory

vars_interet_length <-sum(cols[[1]] %in% c("nb_etab","eff_31","eff_EQTP","rem_brut","non_annexe"))

sizes_list <-list()
while( length(sizes_list)< length(jurid_vars_names)){
    sizes_list<-append(sizes_list,list(c()))
}
names(sizes_list) <- names(jurid_vars)

for ( i in 1: length(jurid_vars)){
    lab_paire <-names(jurid_vars)[i]
    sizes<-c()
    lab_tabs <- jurid_vars[[i]]
    for (tab in lab_tabs){
        col <- cols[[tab]]
        size <- length(col) -2 - vars_interet_length
        sizes <- c(sizes,size)
    }
    sizes_list[[lab_paire]] <- sizes

}
sizes_list


col=cols[[1]]

#-----------------------------------------------------------
# Step 6 : write CSV 





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


pattern=c("juridr","jurid2",colnames(tmp_table))
str_detect(pattern,"jurid$")#ne detect que juri
str_detect(pattern,"jurid")