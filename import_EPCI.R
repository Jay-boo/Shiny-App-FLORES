rm(list=ls())
library("readxl")# Can read xls and xlsx files with the same function read_excel
library(tidyverse)
library(dplyr)
library(readODS)

#--------------------------------

# Let work only on one dir for the moment

directories <- list.dirs("./inputs/", recursive = FALSE, full.names = FALSE)
dir <- directories[2]



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
        # Needed to delete the column in the table too
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

rename_jurid<-c()
counter=1
while(length(rename_jurid) < length(jurid_vars)){
    rename_jurid <- c(rename_jurid,paste("jurid",counter,sep=""))
    counter= counter+1
}

del_index <- c()
counter=1
for (col in cols){
    ind_jurid_var <- which(str_detect(col,"jurid"))
    
    if (ind_jurid_var %>% length ==1){
        lab <-col[ind_jurid_var]
        col[ind_jurid_var] <-rename_jurid[which(names(jurid_vars)==lab)]
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
names(jurid_vars) <- rename_jurid

#------------------------------------------------------
# Step 5 : Detect typo vars +  Rename typo1 and typo2

cols
jurid_vars

typo_vars_names<-sapply(cols,FUN=function(i){
    detect <-which(str_detect(i,"typ|Typ|TYP"))
    if(detect %>% length ==1){
        return(i[detect])
    }
})

vars_names<-c()
for (el in typo_vars_names){
    if( !is.null(el)){vars_names<- c(vars_names,el)}
} 
typo_vars_names<-vars_names %>% unique


length(typo_vars_names)
rename_typo <-c()
counter=1
while(length(rename_typo) < length(typo_vars_names)){
    rename_typo <- c(rename_typo,paste("typo",counter, sep=""))
    counter= counter+1
}
rename_typo

#Rename cols
counter=1
for (col in cols){
    ind_typo <- str_detect(col,"typ|Typ|TYP")
    if(ind_typo %>% length ==1){
        lab <-col[ind_typo]
        col[ind_typo] <- rename_typo[which(typo_vars_names == lab)]
        cols[[counter]] =col

    }
    counter= counter + 1
}
cols

#---------------------------------------------------------
# Step 6 : Detect EPCI info missing and fix it + Merge the good tables

# Regarder juste sur il existe un trio de variable LIBGEO, DEP, nom
# Si non on enleve toute les variables en trop comme on a rename toutes les autres variables importantes 
# il sera facile de les identifiées


# On supprime les autres variables
counter=1
for(col in cols){
    I <- c("nb_etab","eff_31","eff_EQTP","rem_brut","non_annexe")
    incompressible_vars<-c(I,rename_typo,rename_jurid,"EPCI")
    del_index=which(!(col %in% incompressible_vars))
    
    if (del_index%>% length >0){
        cols[[counter]]= col[- del_index]
        table=tables[[counter]]
        tables[[counter]]=table[,-del_index]
    }
    counter= counter+1
}

# On join les bonne paire de table
for(i in 1:length(tables)){
    table<-tables[[i]]
    colnames(table)<- cols[[i]]
    tables[[i]]=table
}


for (col_moda in unique(cols)){
    merge_table<-c()
    counter=1
    for (col in cols){
        
        if(length(col)==length(col_moda)){
            if(all(col==col_moda)){
                merge_table <- c(merge_table,counter)
            }
        }
        counter <- counter+1
    }

    
    # Add LIBGEO +Dep INFO
    table <-tables[[merge_table[1]]]
    
    if(length(merge_table)>1){
        for (i in 2: length(merge_table)){
            table <-rbind(table,tables[[merge_table[i]]])
        }
    }
    table <-table%>% left_join(geo_file,by="EPCI")
    print(colnames(table))
    # Write csv with the good name

    if(intersect(col_moda,rename_typo)%>% length ==0){
        filename="EPCI_T1"
    }else if(rename_typo[1] %in% col_moda){
       filename="EPCI_T2"
    }else{
        filename="EPCI_T3"
    }
    write.csv(table,
        paste("./outputs/",dir,"/",filename,".csv",sep="")
    )
}

# To Add : 
# Prendre en compte que T1 et T3 ne doivent pas avoir des numeros d'EPCI similaire : Utilisation de interesct et setdiff
