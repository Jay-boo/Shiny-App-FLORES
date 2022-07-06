
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
df<-df%>% filter(REG %in% c("France entière","Grand-Est"))
df$nb_etab<-as.numeric(df$nb_etab)
df$rem_brut<-as.numeric(df$rem_brut)
df$REG<- as.factor(df$REG)

unique(df$REG)
pie(df$nb_etab)
pie(df$rem_brut)


#----------------------------------------------------------
#   Dashboard : Scaled part 


DEP_GE=c("08","10","51","52","54","55","57","67","68","88")
TC1 <-read.csv("./outputs/2017/TC1.csv")

#---------
#   Reformat DEP NUM
#       01 needed instead of 1
# +  Retyping famille column

TC1_dep_num=TC1$DEP %>% as.numeric 
for (i in 1: length(TC1_dep_num)){
    if (!is.na(TC1_dep_num[i]) & TC1_dep_num[i]<10){
        TC1$DEP[i]<-paste("0",TC1$DEP[i],sep="")
    } 
}

TC1_famille_tmp<-strsplit(TC1$famille,"[.]")
TC1$famille<-sapply(TC1_famille_tmp,FUN = function(i){
    if(length(i)>1){
        return(i[2])
    }else{
        return(i[1])
    }
})


#---------------------
# Scale : REG

TC13 <-read.csv("./outputs/2017/TC13.csv")
TC13_famille_tmp<-strsplit(TC13$famille,"[.]")
TC13$famille<-sapply(TC13_famille_tmp,FUN = function(i){
    if(length(i)>1){
        return(i[2])
    }else{
        return(i[1])
    }
})
TC16 <-read.csv("./outputs/2017/TC16.csv")
TC16_famille_tmp<-strsplit(TC16$famille,"[.]")
TC16$famille<-sapply(TC16_famille_tmp,FUN = function(i){
    if(length(i)>1){
        return(i[2])
    }else{
        return(i[1])
    }
})


barplot_1<-TC13%>% filter(typo_B_det=="Ensemble" & ESS=="ESS" & REG=="Grand-Est" & typo_B=="Ensemble des secteurs d'activité")%>%
    select(famille,nb_etab)

barplot_2<-TC13%>% filter(typo_B_det=="Ensemble" & ESS=="ESS" & REG=="Grand-Est" & typo_B=="Ensemble des secteurs d'activité")
    select(famille,eff_31)

barplot_3<-TC16%>% filter(sexe=="Ensemble" & ESS=="ESS" & REG=="Grand-Est" & type_emploi=="Ensemble")%>%
    select(famille,nb_poste)

#-----------------------------------
# MAP

# MAP /secteur d'activité
data_map=TC1 %>% filter( typo_A_det=="Ensemble" & famille=="Ensemble" & ESS=="ESS" & DEP %in% DEP_GE & typo_A!="Ensemble des secteurs d'activité")%>%
 select(DEP,typo_A,nb_etab) %>% arrange(DEP)
 
# Map / forme juridique
data_map_bis=TC1%>% filter(typo_A_det=="Ensemble" & ESS=="ESS" & DEP %in% DEP_GE & typo_A=="Ensemble des secteurs d'activité")%>%
    select(DEP,famille,nb_etab)%>% arrange(DEP)




# Sending data to r2d3
#jsonlite::toJSON(data_map,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)




#----------------------
# Scale Dep


#----------------------
# Scale EPCI