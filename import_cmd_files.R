require(readxl)
PATH = "./inputs/2017/cmd_files/fichier_commande_general.xlsx"
excel_sheets(PATH)
read_excel(PATH,sheet = "Typologie_A")