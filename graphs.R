
#------------------------------------------------------------
#      _           _     _                         _ 
#   __| | __ _ ___| |__ | |__   ___   __ _ _ __ __| |
# / _` |/ _` / __| '_ \| '_ \ / _ \ / _` | '__/ _` |
#| (_| | (_| \__ \ | | | |_) | (_) | (_| | | | (_| |
# \__,_|\__,_|___/_| |_|_.__/ \___/ \__,_|_|  \__,_|
#
#-------------------------------------------------------------------

#------------------------------------------------
# Dashboard key numbers
TC13 <-read.csv("./outputs/2017/TC13.csv")
df<-TC13 %>% filter(typo_B=="Ensemble des secteurs d'activité" & typo_B_det=="Ensemble" & famille=="Ensemble" & ESS=="ESS")%>% select(REG,nb_etab,rem_brut)
df<-df%>% filter(REG %in% c("France entière","Bourgogne-Franche-Comté"))
df$nb_etab<-as.numeric(df$nb_etab)
df$rem_brut<-as.numeric(df$rem_brut)
df$REG<- as.factor(df$REG)


pie(df$nb_etab)
pie(df$rem_brut)
#----------------------------------------------------------
#   Dashboard : Scaled part 