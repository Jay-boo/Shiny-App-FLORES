#-------------------------------------
# update.R 
# Mission : Detect new files to update
# Return missing_imports :
#        A list containing entire direcotry missing and files missing 
#------------------------------------

#-------------------------
# Importations
rm(list=ls())
require(dplyr)
require(tidyverse)

#--------------------------------
#   Initializations
report_imports <- read.csv("./inputs/report_imports.csv", sep = ",")
missing_imports <- list(
    "directories" = c(),
    "files" = list("reg_dep" = c(),
                    "infra_dep" = c()
                ) 
)

directories <- list.dirs("./inputs/", recursive = FALSE, full.names = FALSE)

#--------------------
# First step : look for global directory missings
directories_imported <- unique(report_imports$directory)
#directories_imported <- directories_imported[-1]
if (setdiff(directories, directories_imported) %>% length != 0) {
    missing_directories <- setdiff(directories, directories_imported)
    common_directories <- directories[which(
        !directories %in% missing_directories
        )]
}else{
    missing_directories <- c()
    common_directories <- directories
}

missing_imports$directories <- c(missing_directories,missing_imports$directories)

#-------------------------------------------
# Second step for common directories , let's loops on the various files

for (dir in common_directories) {
    files_reg_dep <- list.files(
        paste("./inputs/", dir, "/reg_dep", sep = ""),
        all.files = FALSE
    )
    files_infra_dep <- list.files(
        paste("./inputs/", dir, "/infra_dep", sep = ""),
        all.files = FALSE
    )
    tmp <- report_imports %>%
            filter(as.character(report_imports$dir) == dir) %>%
             select(scale,filename) %>%
              unique

    tmp_reg_dep <- tmp %>%
              filter(scale == "reg_dep") %>% select(filename)
    tmp_infra_dep <- tmp %>%
              filter(scale == "infra_dep") %>% select(filename)
    
    missing_files_reg_dep <-setdiff(files_reg_dep,tmp_reg_dep[[1]])
    missing_files_infra_dep <-setdiff(files_infra_dep,tmp_infra_dep[[1]])
    missing_files_reg_dep <-sapply(missing_files_reg_dep,
                            FUN = function(filename){
                                paste("./inputs/",dir,"/reg_dep/",filename,sep="")
                            })
    
    missing_files_infra_dep <-sapply(missing_files_infra_dep,
                            FUN = function(filename){
                                paste("./inputs/",dir,"/infra_dep/",filename,sep="")
                            })

    if(missing_files_reg_dep%>% length >0){
        missing_imports[["files"]][["reg_dep"]] <-c(missing_imports[["files"]][["reg_dep"]],missing_files_reg_dep)
    }
    if(missing_files_infra_dep%>% length >0){
        missing_imports[["files"]][["infra_dep"]] <-c(missing_imports[["files"]][["infra_dep"]],missing_files_infra_dep)
    }

}



#---------------------------------------------------
#
#    Start importing tables
#
#-----------------------------------------------------
print(missing_imports)
source("import_VF.R")
source("import_EPCI.R")


if(!is.null(missing_imports$directories)){
    for (dir in missing_imports$directories){
        # Import reg_dep scale of the directory
        import_directory(dir)
    }
    import_EPCI(missing_directories$directories)
}


for (file in missing_imports$files$reg_dep){
    import_files(file)
}


#-------------------------------------------------------------------
#EPCI imports are special : Indeed you can't just import a part of the EPCI file
# So for missing_files in infra_dep part you need to check if all the directory is missing
#And if not you need to remove the imported files and remove there lines on the report

dir_EPCI<-c()
for (file in missing_imports$files$infra_dep){
    tmp=str_split(file,"/")[[1]][-1]
    directory=tmp[2]
    if( !(directory %in% dir_EPCI)){
        dir_EPCI <-c(dir_EPCI,directory)
    }
}
list_EPCI<-list()
while(length(list_EPCI) <length(dir_EPCI)){
    list_EPCI <-append(list_EPCI,list(c()))
}
names(list_EPCI)<-dir_EPCI
for(file in missing_imports$file$infra_dep){
    dir=str_split(file,"/")[[1]][-1] [2]
    vec=list_EPCI[[dir]]
    list_EPCI[[dir]]=c(vec,file)
}
# list_EPCI contient les fichier etant absent pour chaque direcotry
for(dir in dir_EPCI){
    all_EPCI_in_dir <- list.files(
            paste("./inputs/", dir, "/infra_dep", sep = ""),
            all.files = FALSE
    )
    if(setdiff(all_EPCI_in_dir,list_EPCI[[dir]]) %>%length != 0){
        #Certain fichier EPCI on d'ores et deja ete importer

        # On supprime les output deja importe déjà importé

        all_EPCI_in_output <-list.files(
            paste("./outputs/",dir,"/",sep=""),
            pattern = "EPCI",
            all.files = FALSE
        )
        
        file.remove(paste("./outputs/",dir,"/",all_EPCI_in_output,sep=""))


        # + On supprime leur ligne d'import dans le report
        report_imports <- read.csv("./inputs/report_imports.csv", sep = ",")
        report_imports <-report_imports %>% filter( directory != dir & scale!="infra_dep" & !(filename %in% all_EPCI_in_dir))
    }
    # Si aucun fichier EPCI n'a déjà ete importé alors pas besoin e faire la manoeuvre

}
import_EPCI(dir_EPCI)
