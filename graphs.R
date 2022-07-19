
#------------------------------------------------------------
#      _           _     _                         _ 
#   __| | __ _ ___| |__ | |__   ___   __ _ _ __ __| |
# / _` |/ _` / __| '_ \| '_ \ / _ \ / _` | '__/ _` |
#| (_| | (_| \__ \ | | | |_) | (_) | (_| | | | (_| |
# \__,_|\__,_|___/_| |_|_.__/ \___/ \__,_|_|  \__,_|
#
#-------------------------------------------------------------------




#--------------------------------------------------------
# Pie Chart Repartition etablissements / entreprises / eff_EQTP/ eff_31 / Rem brute  par ESS/ Privée Hors ESS /Public

df  <- tables[["outputs/2017/TC13.csv"]]
# ESS - famille
# ESS+Hors ESS - Ensemble => Total
# Hors ESS - Public => Total Public
#  Hors ESS - Prive Hors ESS=> Total Privé hors ESS
# ESS + Ensemble
pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
colnames(pattern)  <-  c("ESS","famille")
patterns  <- paste(pattern$ESS,pattern$famille,sep="")

df  <- df %>% filter(REG=="Grand-Est" & typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble")
df[which( c(df$ESS,df$famille) %in% pattern )]
df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns) %>% select(-REG, -ESS, -typo_B,-typo_B_det)
df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"



#---------------------------------------------------------
# Bar plot REg-DEP-EPCI des variables d'interets classique comparer aux restes

#------------------------
# REG
df  <- tables[["outputs/2017/TC13.csv"]]

first_part  <- df %>%filter( REG== "Grand-Est" & famille=="Ensemble"  & ESS=="ESS" & typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble") %>% select(-typo_B,-typo_B_det,-famille,-ESS) 
first_part  <- data.frame(first_part$REG,first_part[,getIndexCol("nb_etab",first_part)])
colnames(first_part)=c("REG","nb_etab")

tmp  <-  df %>%filter( REG !="France entière" & famille=="Ensemble"  & ESS=="ESS" & typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble") %>% select(-typo_B,-typo_B_det,-famille,-ESS)

tmp  <- data.frame(tmp$REG,tmp[,getIndexCol("nb_etab",tmp)])
colnames(tmp)=c("REG","nb_etab")

second_part  <- df %>%filter( !(REG %in% c("France entière","Grand-Est")) & famille=="Ensemble"  & ESS=="ESS" & typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble") %>% select(-typo_B,-typo_B_det,-famille,-ESS) 
second_part  <- data.frame(second_part$REG,second_part[,getIndexCol("nb_etab",second_part)])
colnames(second_part)=c("REG","nb_etab")
second_part  <- arrange(second_part,desc(second_part[,getIndexCol("nb_etab",second_part)]))
second_part  <- second_part%>%head(3)
first_part <-  rbind(c("moyenne",mean(tmp$nb_etab)),first_part)
rbind(first_part,second_part)

#----------------------
#	DEP

df  <-  tables[["outputs/2017/TC1.csv"]]
nom_complet <- "Marne"
tmp  <-  df %>%filter( DEP !="France entière" & famille=="Ensemble"  & ESS=="ESS" & typo_A=="Ensemble des secteurs d'activité" & typo_A_det=="Ensemble") %>% select(-typo_A,-typo_A_det,-famille,-ESS) %>%filter(as.numeric(DEP) %in% as.numeric(available_DEP$code)) 

tmp  <- data.frame(tmp$DEP,tmp[,getIndexCol("nb_etab",tmp)])
colnames(tmp)  <- c("DEP","nb_etab")

code  <-available_DEP%>%filter(nom==nom_complet)%>% select(code) 
code  <- code[[1]]
first_part  <- df %>% filter(as.numeric(DEP)==as.numeric(code) &  famille=="Ensemble" & ESS=="ESS" & typo_A=="Ensemble des secteurs d'activité" & typo_A_det=="Ensemble")%>% select(-typo_A,-typo_A_det,-famille,-ESS)%>% select(DEP,nb_etab)

first_part  <- data.frame(first_part$DEP,first_part[,getIndexCol("nb_etab",first_part)])
colnames(first_part)  <- c("DEP","nb_etab")
first_part[1,1] =nom_complet

second_part  <- df %>% filter (as.numeric(DEP) %in% as.numeric(available_DEP$code) & famille=="Ensemble" & ESS=="ESS" & typo_A=="Ensemble des secteurs d'activité" & typo_A_det=="Ensemble")%>%select(-typo_A,-typo_A_det,-famille,-ESS)

second_part  <- data.frame(second_part$DEP ,second_part[,getIndexCol("nb_etab",second_part)])
colnames(second_part) <- c("DEP","nb_etab")
second_part  <- second_part %>% filter(as.numeric(DEP) !=as.numeric(code))
second_part  <-  arrange(second_part,desc(second_part[,getIndexCol("nb_etab",second_part)]))
second_part  <- second_part %>%head(3)
available_DEP_bis <- available_DEP
available_DEP_bis$code  <- as.numeric(available_DEP_bis$code) 
colnames(second_part)  <- c("code","nb_etab")
second_part$code  <- as.numeric(second_part$code)



second_part  <- left_join(second_part,available_DEP_bis)%>% select(nom,nb_etab)
colnames(second_part) = c("DEP","nb_etab")
first_part  <- rbind(c("moyenne",mean(tmp$nb_etab)),first_part)

rbind(first_part,second_part)


#-------------------------
#	EPCI

df  <- tables[["outputs/2017/EPCI_T1.csv"]]

nom_compl<- "CC Faucigny-Glières"

tmp  <- df %>% filter(nom_complet %in% available_EPCI$nom_complet & jurid1=="4-ESS" ) %>%
	select(-EPCI,-jurid1, -dep_epci)

first_part  <-  df%>%filter(nom_complet ==nom_compl & jurid1=="4-ESS")%>% select(nom_complet,nb_etab)

second_part  <- df%>% filter(nom_complet %in% available_EPCI$nom_complet & jurid1=="4-ESS")%>% filter(nom_complet!=nom_compl)%>% select(nom_complet,nb_etab)%>%arrange(desc(nb_etab))%>%head(3)

first_part  <- rbind(c("moyenne",mean(tmp$nb_etab)),first_part)


rbind(first_part,second_part)
