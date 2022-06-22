#-------------------------------------
# update.R 
# Mission : Detect new files to update
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
    "files" = c()
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
    files <- list.files(
        paste("./inputs/", dir, "/reg_dep", sep = ""),
        all.files = FALSE
    )

    tmp <- report_imports %>%
            filter(as.character(report_imports$dir) == dir) %>%
             select(scale,filename) %>%
              unique %>%
               select(filename)
    missing_files <- setdiff(files, tmp[[1]])
    missing_files <- sapply(missing_files,
                            FUN=function(filename) {
                                paste("./inputs/", dir, "/reg_dep/",filename, sep = "")
                            })

    missing_imports[["files"]] <- c(missing_files,missing_imports[["files"]])

}
#--------------------------
# Throw useless variables
rm(list=ls()[ls() != "missing_imports"])
missing_imports[["files"]]
print(missing_imports)
