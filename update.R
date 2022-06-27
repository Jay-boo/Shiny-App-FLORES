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
                    "infra_dep" = c()) 
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
    tmp <- tmp_reg_dep <- report_imports %>%
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


    missing_imports[["files"]][["reg_dep"]] <-c(missing_imports[["files"]][["reg_dep"]],missing_files_reg_dep)
    missing_imports[["files"]][["infra_dep"]] <-c(missing_imports[["files"]][["infra_dep"]],missing_files_infra_dep)

}



#--------------------------
# Throw useless variables

rm(list=ls()[ls() != "missing_imports"])
missing_imports[["files"]]
print(missing_imports)
