#------------------------------------------------------------------------------------
#   Importation des tables
# 
#       1. Detect ods tables and other formats
#       2. Detect variables and Rename them 
#       3. Detect which TC is refered 
#       4. write all tables in CSV with the good filename
#------------------------------------------------------------------------------------


rm(list = ls())
library(readODS)
library(dplyr)
library(skimr)
library(tidyverse)

#------------------------
# 1. detect format

# There is only 2 formats: ods or csv
directories  <-list.dirs("./inputs/",recursive = FALSE,full.names = FALSE)

ods_tables <- list.files("./inputs/2017",pattern=".ods",all.files = TRUE)
csv_tables <-list.files("./inputs/2017",pattern=".csv",all.files= TRUE)
#------------------------
# 2. Detect variables and Rename them

skip_term <- function(data) {
    skip <- 0
    while (TRUE) {
        if(any(is.na(data[skip + 1, ])) ) {
            skip <- skip + 1
        }
        else{
            break
        }
    }
    return(skip)
}

skip_term_csv <- function(data){
    skip <- 0
    while (TRUE) {
        if (any((data[skip + 1, ] == ""))) {
            skip <- skip + 1
        }
        else{
            break
        }
    }
    return(skip)
}


rename_var_ods_tables <- function(data) {
    #' Rename variables off data table
    #'
    #' @param data  
    #'
    #' @return data
    #'
    #' @examples
    list_index=list()

    #-----------------
    # General Variables detection
    ind_ent_pot <- which(
        (str_detect(colnames(data),"Nombre|nombre|nb|NB|NOMBRE")) &  (str_detect(colnames(data),"entr|Entr"))
    )
    list_index <- append(list_index, list("nb_ent" = ind_ent_pot))

    ind_etab_pot <-which(
        (str_detect(colnames(data),"Nombre|nombre|nb|NB|NOMBRE")) &  (str_detect(colnames(data),"etb|etab|Etab|étab"))
    )
    list_index <- append(list_index, list("nb_etab" = ind_etab_pot))

    ind_eff31_pot<-which(
        (str_detect(colnames(data),"eff|Eff|EFF") & str_detect(colnames(data),"31"))
    )
    list_index <- append(list_index, list("eff_31" = ind_eff31_pot))

    ind_eff_EQTP_pot<-which(
        (str_detect(colnames(data),"eff|Eff|EFF") & str_detect(colnames(data),"EQTP|eqtp"))
    )
    list_index <- append(list_index, list("eff_EQTP" = ind_eff_EQTP_pot))

    ind_remBrute_pot<-which(
        (str_detect(colnames(data),"Rem|rem|Rém|rém|REM") & str_detect(colnames(data),"brut|brt|BRT|Brt|BRUT"))
    )
    list_index <- append(list_index, list("rem_brut" = ind_remBrute_pot))

    ind_reg_pot <- which(
        str_detect(colnames(data), "REG|reg|rég|Rég")
    )
    list_index <- append(list_index, list("REG" = ind_reg_pot))

    ind_dep_pot <- which(
        str_detect(colnames(data), "DEP|dep|Dép|Dep")
    )
    list_index <- append(list_index, list("DEP" = ind_dep_pot))

    ind_ESS_pot <- which(
        str_detect(colnames(data), "ESS|ess") &  !str_detect(colnames(data), "simp|SIMP")
    )
    list_index <- append(list_index, list("ESS" = ind_ESS_pot))

    ind_ESS_simp_pot <- which(
        str_detect(colnames(data), "ESS|ess") & str_detect(colnames(data), "simp|SIMP") 
    )
    list_index <- append(list_index, list("ESS_simp" = ind_ESS_simp_pot))

    ind_famille_pot <- which(
        str_detect(colnames(data), "FAMILLE|famille|Famille")
    )
    list_index <- append(list_index, list("famille" = ind_famille_pot))

    ind_sexe_pot <- which(
        str_detect(colnames(data), "sexe|Sexe|Sex|SEX|SEXE")
    )
    list_index <- append(list_index, list("sexe" = ind_sexe_pot))

    ind_type_emploi_pot <- which(
        str_detect(colnames(data),"type|TYPE|Type") & str_detect(colnames(data),"emp|EMP|Emp")
    )
    list_index <- append(list_index, list("type_emploi" = ind_type_emploi_pot))

    ind_nb_poste_pot<-which(
        str_detect(colnames(data),"Nombre|nombre|nb|NB|NOMBRE") & str_detect(colnames(data),"poste|POSTE|Poste")
    )
    list_index <- append(list_index, list("nb_poste" = ind_nb_poste_pot))

    ind_taille_etab_pot<-which(
        str_detect(colnames(data), "taille|Taille|TAILLE") & str_detect(colnames(data), "etb|etab|Etab|étab")
    )
    list_index <- append(list_index, list("taille_etab" = ind_taille_etab_pot))

    ind_CSP_pot <- which(
        str_detect(colnames(data), "CSP|csp")
    )
    list_index <- append(list_index, list("CSP" = ind_CSP_pot))

    ind_IDF_pot <-which(
        str_detect(colnames(data),"IDF|idf")
    )
    list_index<-append(list_index,list("IDF" = ind_IDF_pot))

    
    #------------------------
    # Typo A, B, C variables detection
    if (length(list_index[["DEP"]])> 0) {

        ind_typo_A_pot <- which(
            str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"A") & !str_detect(colnames(data),"taill|TAILL") & !str_detect(colnames(data),"Asso|asso")
        )
        list_index <- append(list_index, list("typo_A" = ind_typo_A_pot))

        ind_typo_A_det_pot <- which(
            str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"A") & str_detect(colnames(data),"taill|TAILL") & !str_detect(colnames(data),"Asso|asso")
        )
        list_index <- append(list_index, list("typo_A_det" = ind_typo_A_det_pot))
    
    }else if (length(list_index[["REG"]]) > 0) {

        ind_typo_B_pot <- which(
            str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"B") & !str_detect(colnames(data),"taill|TAILL") 
        )
        list_index <- append(list_index, list("typo_B" = ind_typo_B_pot))

        ind_typo_B_det_pot <- which(
            str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"B") & str_detect(colnames(data),"taill|TAILL") 
        )
        list_index <- append(list_index, list("typo_B_det" = ind_typo_B_det_pot))
    
    }else {
        ind_typo_C_pot <- which(
            str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"C") & !str_detect(colnames(data),"taill|TAILL") 
        )
        list_index <- append(list_index, list("typo_C" = ind_typo_C_pot))

        ind_typo_C_det_pot <- which(
            str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"C") & str_detect(colnames(data),"taill|TAILL") 
        )
        list_index <- append(list_index, list("typo_C_det" = ind_typo_C_det_pot))
    }


    #------------------------
    # Typo Associations, Muts, Coop variables detection
    ind_typo_Asso_pot <- which(
        str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"Asso|asso") & !str_detect(colnames(data),"taill|TAILL") 
    )
    list_index <- append(list_index, list("typo_Asso" = ind_typo_Asso_pot))

    ind_typo_Asso_det_pot <- which(
        str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"Asso|asso") & str_detect(colnames(data),"taill|TAILL") 
    )
    list_index <- append(list_index, list("typo_Asso_det" = ind_typo_Asso_det_pot))

    ind_typo_Mut_pot <- which(
        str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"Mut|mut") & !str_detect(colnames(data),"taill|TAILL") 
    )
    list_index <- append(list_index, list("typo_Mut" = ind_typo_Mut_pot))
    
    ind_typo_Mut_det_pot <- which(
        str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"Mut|mut") & str_detect(colnames(data),"taill|TAILL") 
    )
    list_index <- append(list_index, list("typo_Mut_det" = ind_typo_Mut_det_pot))


    ind_typo_Coop_pot <- which(
        str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"Coop|coop|COOP") & !str_detect(colnames(data),"taill|TAILL") 
    )
    list_index <- append(list_index, list("typo_Coop" = ind_typo_Coop_pot))
    
    ind_typo_Coop_det_pot <- which(
        str_detect(colnames(data), "Typo|typo|TYPO") & str_detect(colnames(data),"Coop|coop|COOP") & str_detect(colnames(data),"taill|TAILL") 
    )
    list_index <- append(list_index, list("typo_Coop_det" = ind_typo_Coop_det_pot))


    #------------------------
    # Nomenclature juridique variables detection
    ind_Nom_juridique_coop_pot <- which(
        str_detect(colnames(data), "Nomen|NOMEN|nomen") & str_detect(colnames(data),"Juri|jurid|JUR") & str_detect(colnames(data),"COOP|coop|Coop") 
    )
    list_index <- append(list_index, list("nom_jurid_coop" = ind_Nom_juridique_coop_pot))
    
    ind_Nom_juridique_2_pot <- which(
        str_detect(colnames(data), "Nomen|NOMEN|nomen") & str_detect(colnames(data),"Juri|jurid|JUR") & str_detect(colnames(data),"2") & !str_detect(colnames(data),"COOP|coop|Coop") 
    )
    list_index <- append(list_index, list("nom_jurid_2" = ind_Nom_juridique_2_pot))
    
    
    #--------------------------
    #Check  the list_index
    
    if (ncol(data)!=sapply(list_index,length) %>% sum){break}
    
    #----------------------------
    # Rename all columns
    col_detected_bool <- sapply(list_index, length)
    noms <- names(col_detected_bool[which(col_detected_bool != 0)])
    final_liste <- sapply(noms,function(i){list_index[[i]]})
    for (i in 1 : length(final_liste)) {
        label <- names(final_liste)[i]
        index <- final_liste[i]
        colnames(data)[index] <- label
    }
    return(data)
}

#------------------------
# 3. Detect which TC is refered

# Plus tard pour associer un num a chaque table on utilisera set_diff entre colnames et chaque possibilité  Car RAF de l'ordre 

I=c("nb_etab","nb_ent","eff_31","eff_EQTP","rem_brut")
I_bis=c("nb_etab","eff_31","eff_EQTP","rem_brut")
var2name = list(   
    "TC1"=c(c("DEP","ESS","famille","typo_A","typo_A_det"),I),
    "TC2"=c(c("DEP","ESS","famille","taille_etab"),I),
    "TC3"=c(c("DEP","ESS_simp","typo_A","typo_A_det","taille_etab"),I),
    "TC4"=c("DEP","ESS","famille","sexe","type_emploi","nb_poste"),
    "TC5"=c("DEP","ESS_simp","typo_A","typo_A_det","sexe","CSP","nb_poste"),
    "TC6"=c(c("DEP","typo_Asso","typo_Asso_det"), I_bis),
    "TC7"=c("DEP","typo_Asso","typo_Asso_det","type_emploi","nb_poste"),
    "TC8"=c(c("DEP","typo_Coop","typo_Coop_det"),I_bis),
    "TC9"=c(c("DEP","nom_jurid_coop"),I_bis),
    "TC10"=c("DEP","typo_Coop","typo_Coop_det","type_emploi","nb_poste"),
    "TC11"=c(c("DEP","typo_Mut","typo_Mut_det"),I_bis),
    "TC12"=c("DEP","typo_Mut","typo_Mut_det","type_emploi","nb_poste"),
    "TC13"=c(c("REG","ESS","famille","typo_B","typo_B_det"),I),
    "TC14"=c(c("REG","ESS","famille","taille_etab"),I),
    "TC15"=c(c("REG","ESS_simp","typo_B","typo_B_det","taille_etab"),I),
    "TC16"=c("REG","ESS","famille","sexe","type_emploi","nb_poste"),
    "TC17"=c("REG","ESS_simp","typo_B","typo_B_det","sexe","CSP","nb_poste"),
    "TC18"=c(c("REG","typo_Asso","typo_Asso_det"),I_bis),
    "TC19"=c("REG","typo_Asso","typo_Asso_det","sexe","type_emploi","nb_poste"),
    "TC20"=c(c("REG","typo_Coop","typo_Coop_det"),I_bis),
    "TC21"=c("REG","typo_Coop","typo_Coop_det","sexe","type_emploi","nb_poste"),
    "TC22"=c("REG","nom_jurid_coop","nb_poste"),
    "TC23"=c(c("REG","typo_Mut","typo_Mut_det"),I_bis),
    "TC24"=c("REG","typo_Mut","typo_Mut_det","sexe","type_emploi","nb_poste"),
    "TC25"=c(c("ESS","famille","typo_C","typo_C_det","taille_etab","IDF"),I),
    "TC25_bis"=c(c("ESS","famille","typo_C","typo_C_det","taille_etab"),I),
    "TC26"=c(c("nom_jurid_2","taille_etab"),I),
    "TC27"=c("ESS","famille","typo_C","typo_C_det","sexe","type_emploi","CSP","nb_poste")    
)

detect_TC_number <- function(data) {
    counter <- 1
    data_colnames <- colnames(data)
    for (i in var2name) {
        if (setdiff(i,data_colnames)%>% length == 0) {
            break
        }
        counter <- counter + 1
    }
    return(names(var2name)[counter])
}


#------------------------
# 4. write all tables in CSV with the good filename


ods_tables <-ods_tables[ods_tables !="TC17_VF.ods" & ods_tables !="TC27_VF.ods" & ods_tables !="TC25_VF.ods"]

# For ods tables
for (ods_file in ods_tables) {
    print(paste("creating ...",ods_file," csv file"))
    tmp_table <- read_ods(path = paste("inputs/2017/", ods_file, sep = ""))
    if(skip_term(tmp_table)>0) {
        data <- tmp_table[-1: -skip_term(tmp_table),]
    }else{
        data <- tmp_table
    }
    colnames(data) <- data[1,]
    data <- data[-1,]
    
    # 2.Rename var step
    data <- rename_var_ods_tables(data)
    
    # 3.Get TC number refering
    filename <- detect_TC_number(data)

    # 4. write csv file
    write.csv(
        data,
        paste("./outputs/", filename, ".csv", sep = ""),
        row.names = FALSE
    )
}
#--------------------------------------------
# For csv files
for (csv_file in csv_tables) {
    print(paste("creating ...",csv_file," csv file"))
    tmp_table <- read.csv(paste("inputs/2017/",csv_file,sep=""),sep=";",row.names=NULL)
    if(skip_term_csv(tmp_table) > 0) {
        data <- tmp_table[-1: -skip_term_csv(tmp_table),]
    }else{
        data <- tmp_table
    }
    colnames(data) <- data[1,]
    data <- data[-1,]
    # 2.Rename var step
    data <- rename_var_ods_tables(data)
    
    # 3.Get TC number refering
    filename <- detect_TC_number(data)

    # 4. write csv file
    write.csv(
        data,
        paste("./outputs/", filename, ".csv", sep = ""),
        row.names = FALSE
    )
}



#-------------------------------------------------------------------------------------------------------------------------
# Types:
# ON va parcourir uniquement les elements qui ont une lenght ==1
# et l'on va aller chopper l'index dans la liste puis renomer cette variable selon la clé de la liste

#types<-list(
#"numeric"=c("nb_ent","nb_etab","eff_31","eff_EQTP","rem_brut","nb_poste"),
#"factor"=c("REG","DEP","ESS","ESS_simp","famille","sexe","type_emploi","taille_etab","CSP",
#            "typo_A","typo_A_det","typo_B_det","typo_C","typo_C_det","typo_Asso","typo_Asso_det",
#            "typo_Mut","typo_Mut_det","typo_Coop","typo_Coop_det","nom_jurid_coop","nom_jurid_2","IDF")
#)

#if (label %in% types[["numeric"]]){type_change = as.numeric}else{type_change=as.factor}
#        data[,index]=type_change(data[,index])
        