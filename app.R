# Start with update
# Be sure RAM is empty at start
rm(list=ls())

#source("update.R")
# Create  missing_imports list + Import the missing_files 

require(shiny)

#-----------------------------------
# Import cmd files
#require(readxl)
#PATH = "./inputs/2017/cmd_files/fichier_commande_general.xlsx"
#excel_sheets(PATH)
#read_excel(PATH,sheet = "Typologie_A")
#---------------------------------------------

server <-function(input,output){

}


shinyApp(
    ui=htmlTemplate("www/index.html"),
    server=server
)


