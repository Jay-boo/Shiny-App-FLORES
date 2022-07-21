
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




#--------------------------------------


# TABS2 - PART 1 : Nombre d'etbs nombre d'emploi par secteurs d'activité selon ESS/ Public / Privé Hors ESS

#-----------------------



#-------------
# REG

df  <- tables[["outputs/2018/TC13.csv"]]


pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
colnames(pattern)  <-  c("ESS","famille")
patterns  <- paste(pattern$ESS,pattern$famille,sep="")


first_part  <-df%>% filter(REG=="France entière" & typo_B_det=="Ensemble")

first_part  <- first_part %>%filter(paste(ESS,famille,sep="") %in% patterns)  
first_part[which(first_part$famille=="Ensemble"),]$famille   <-  "ESS"
for (i in 1:nrow(first_part)){
	splitted  <- strsplit(first_part$typo_B[i],"[(]")[[1]]
	first_part$typo_B[i]  <- splitted[1]
}


first_part  <- data.frame(first_part$REG,first_part$famille,first_part$typo_B,first_part[,getIndexCol("nb_etab",first_part)])

colnames(first_part)  <- c("REG","famille","typo_B","nb_etab")


first_part_bis  <-  data.frame("REG"=character(),"famille"=character(),"typo_B"=character(),"var"=numeric())
for(fam in first_part$famille%>%unique ){
	tmp  <- first_part%>% filter(famille==fam)
	tot  <- tmp[which(tmp$typo_B=="Ensemble des secteurs d'activité"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	first_part_bis  <- rbind(first_part_bis,tmp)
}
first_part_bis  <- first_part_bis %>% filter(typo_B!="Ensemble des secteurs d'activité")

df  <- df %>% filter(REG=="Grand-Est" &  typo_B_det=="Ensemble" )
df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns)  
df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
df  <-df%>% select(-typo_B_det,-ESS)
for (i in 1:nrow(df)){
	splitted  <- strsplit(df$typo_B[i],"[(]")[[1]]
	df$typo_B[i]  <- splitted[1]
}


df  <- data.frame(df$REG,df$famille,df$typo_B,df[,getIndexCol("nb_etab",df)])
colnames(df)  <- c("REG","famille","typo_B","nb_etab")
new_dat  <-  data.frame("REG"=character(),"famille"=character(),"typo_B"=character(),"var"=numeric())
for(fam in df$famille%>%unique ){
	tmp  <- df%>% filter(famille==fam)
	tot  <- tmp[which(tmp$typo_B=="Ensemble des secteurs d'activité"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	new_dat  <- rbind(new_dat,tmp)
}

new_dat  <-new_dat %>% filter(typo_B !="Ensemble des secteurs d'activité")
rbind(new_dat,first_part)
#------------------
# DEP

df  <- tables[["outputs/2017/TC1.csv"]]

nom_DEP  <- "Marne"
code  <-available_DEP%>%filter(nom==nom_DEP)%>% select(code) 
code  <- code[[1]]
pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
colnames(pattern)  <-  c("ESS","famille")
patterns  <- paste(pattern$ESS,pattern$famille,sep="")

first_part  <-df%>% filter(DEP=="France entière" & typo_A_det=="Ensemble")
first_part  <- first_part %>%filter(paste(ESS,famille,sep="") %in% patterns)  
first_part[which(first_part$famille=="Ensemble"),]$famille   <-  "ESS"
for (i in 1:nrow(first_part)){
	splitted  <- strsplit(first_part$typo_A[i],"[(]")[[1]]
	first_part$typo_A[i]  <- splitted[1]
}


first_part  <- data.frame(first_part$DEP,first_part$famille,first_part$typo_A,first_part[,getIndexCol("nb_etab",first_part)])

colnames(first_part)  <- c("DEP","famille","typo_A","nb_etab")


first_part_bis  <-  data.frame("DEP"=character(),"famille"=character(),"typo_A"=character(),"var"=numeric())
for(fam in first_part$famille%>%unique ){
	tmp  <- first_part%>% filter(famille==fam)
	tot  <- tmp[which(tmp$typo_A=="Ensemble des secteurs d'activité"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	first_part_bis  <- rbind(first_part_bis,tmp)
}
first_part_bis <-  first_part_bis %>% filter(typo_A !="Ensemble des secteurs d'activité")

df  <- df %>% filter(as.numeric(DEP)==as.numeric(code) &  typo_A_det=="Ensemble")
df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns)  
df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
df  <-df%>% select(-typo_A_det,-ESS)
for (i in 1:nrow(df)){
	splitted  <- strsplit(df$typo_A[i],"[(]")[[1]]
	df$typo_A[i]  <- splitted[1]
}
df$DEP =rep(nom_DEP,nrow(df))
df  <- data.frame(df$DEP,df$famille,df$typo_A,df[,getIndexCol("nb_etab",df)])
colnames(df)  <- c("DEP","famille","typo_A","nb_etab")
new_dat  <-  data.frame("DEP"=character(),"famille"=character(),"typo_A"=character(),"var"=numeric())
for(fam in df$famille%>%unique ){
	tmp  <- df%>% filter(famille==fam)
	tot  <- tmp[which(tmp$typo_A=="Ensemble des secteurs d'activité"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	new_dat  <- rbind(new_dat,tmp)
}

new_dat  <-new_dat %>% filter(typo_A !="Ensemble des secteurs d'activité")

rbind(new_dat,first_part_bis)
#-----------------
# EPCI 


df  <- tables[["outputs/2017/EPCI_T2.csv"]]

nom_EPCI  <- "CC Faucigny-Glières"
df  <- df %>%filter(nom_complet==nom_EPCI )
for (i in 1:nrow(df)){
	splitted  <- strsplit(df$jurid2[i],"TOTAL-")[[1]]
	print(splitted)
	df$jurid2[i]  <- splitted[2]
}
df$jurid2 %>%unique
df  <-df %>% filter(jurid2 != "ASSOCIATIONS")
# Seulement 2 modalité : TOTAL-ESS / TOTAL HORS ESS
df  <- df %>% select(-EPCI)

for (i in 1:nrow(df)){
	splitted  <- strsplit(df$typo1[i],"-")[[1]]
	print(splitted)
	if(splitted%>% length >1){

		df$typo1[i]  <- splitted[2]
	}else{

		df$typo1[i]  <- splitted[1]
	}
}
df[which(df$typo1=="NON"),]$typo1  <- "NON CLASSES"


df  <- data.frame(df$nom_complet,df$jurid2,df$typo1,df[,getIndexCol("nb_etab",df)])
colnames(df)  <- c("EPCI","famille","typo1","nb_etab")
new_dat  <-  data.frame("EPCI"=character(),"famille"=character(),"typo1"=character(),"var"=numeric())

for(fam in df$famille%>%unique ){
	tmp  <- df%>% filter(famille==fam)
	tot  <- tmp[which(tmp$typo1=="TOUS SECTEURS"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	new_dat  <- rbind(new_dat,tmp)
}
new_dat  <- new_dat%>%filter(typo1!="TOUS SECTEURS")
new_dat$famille %>%unique






#----------------------------------------
#
# TABS 2 - PART 2 : Nb etblissmeents /emplois par famille juridique
#
#----------------------------------------



#------------------------
# REG
df  <- tables[["outputs/2018/TC13.csv"]]

first_part  <- df %>% filter(REG=="France entière" & typo_B_det=="Ensemble" & typo_B=="Ensemble des secteurs d'activité") %>% select(-typo_B,-typo_B_det)

first_part  <- first_part %>% filter(ESS %in% c("ESS","ESS + Hors ESS")  )




for (i in 1:nrow(first_part)){
		splitted  <- strsplit(first_part$famille[i],". ")[[1]]
		if (splitted %>% length==1){

			first_part$famille[i]  <- splitted[1]
		}else if(splitted%>%length ==2){
			first_part$famille[i]  <- splitted[2]
		}else if(splitted %>%length ==3){
			first_part$famille[i]  <- "Privé Hors ESS"
		}
}



first_part  <- data.frame(first_part$REG,first_part$ESS,first_part$famille,first_part[,getIndexCol("nb_etab",first_part)])

colnames(first_part)  <- c("REG","ESS","famille","nb_etab")


first_part_bis  <-  data.frame("REG"=character(),"ESS"=character(),"famille"=character(),"var"=numeric())
for(ESS_val in first_part$ESS%>%unique ){
				tmp  <- first_part%>% filter(ESS==ESS_val)
				tot  <- tmp[which(tmp$famille=="Ensemble"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
				first_part_bis  <- rbind(first_part_bis,tmp)
}



df  <- df %>% filter(REG=="Grand-Est" & typo_B_det=="Ensemble" & typo_B=="Ensemble des secteurs d'activité") %>% select(-typo_B,-typo_B_det)

df  <- df %>% filter(ESS %in% c("ESS","ESS + Hors ESS") )


for (i in 1:nrow(df)){
		splitted  <- strsplit(df$famille[i],". ")[[1]]
		if (splitted %>% length==1){

			df$famille[i]  <- splitted[1]
		}else if(splitted%>%length ==2){
			df$famille[i]  <- splitted[2]
		}else if(splitted %>%length ==3){
			df$famille[i]  <- "Privé Hors ESS"
		}
}

df  <- data.frame(df$REG,df$ESS,df$famille,df[,getIndexCol("nb_etab",df)])

colnames(df)  <- c("REG","ESS","famille","nb_etab")


new_data  <-  data.frame("REG"=character(),"ESS"=character(),"famille"=character(),"var"=numeric())
for(ESS_val in df$ESS%>%unique ){
				tmp  <- df%>% filter(ESS==ESS_val)
				tot  <- tmp[which(tmp$famille=="Ensemble"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
				new_data  <- rbind(new_data,tmp)
}
rbind(first_part_bis,new_data)

#-------------------
# DEP 
nom_DEP  <- "Marne"

code  <-available_DEP%>%filter(nom==nom_DEP)%>% select(code) 
code  <- code[[1]]

df  <- tables[["outputs/2018/TC1.csv"]]


first_part  <- df %>% filter(DEP=="France entière" & typo_A_det=="Ensemble" & typo_A=="Ensemble des secteurs d'activité") %>% select(-typo_A,-typo_A_det)

first_part  <- first_part %>% filter(ESS %in% c("ESS","ESS + Hors ESS")  )




for (i in 1:nrow(first_part)){
		splitted  <- strsplit(first_part$famille[i],". ")[[1]]
		if (splitted %>% length==1){

			first_part$famille[i]  <- splitted[1]
		}else if(splitted%>%length ==2){
			first_part$famille[i]  <- splitted[2]
		}else if(splitted %>%length ==3){
			first_part$famille[i]  <- "Privé Hors ESS"
		}
}



first_part  <- data.frame(first_part$DEP,first_part$ESS,first_part$famille,first_part[,getIndexCol("nb_etab",first_part)])

colnames(first_part)  <- c("DEP","ESS","famille","nb_etab")


first_part_bis  <-  data.frame("DEP"=character(),"ESS"=character(),"famille"=character(),"var"=numeric())
for(ESS_val in first_part$ESS%>%unique ){
				tmp  <- first_part%>% filter(ESS==ESS_val)
				tot  <- tmp[which(tmp$famille=="Ensemble"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
				first_part_bis  <- rbind(first_part_bis,tmp)
}



df  <- df %>% filter(as.numeric(DEP)==as.numeric(code) & typo_A_det=="Ensemble" & typo_A=="Ensemble des secteurs d'activité") %>% select(-typo_A,-typo_A_det)

df  <- df %>% filter(ESS %in% c("ESS","ESS + Hors ESS") )


for (i in 1:nrow(df)){
		splitted  <- strsplit(df$famille[i],". ")[[1]]
		if (splitted %>% length==1){

			df$famille[i]  <- splitted[1]
		}else if(splitted%>%length ==2){
			df$famille[i]  <- splitted[2]
		}else if(splitted %>%length ==3){
			df$famille[i]  <- "Privé Hors ESS"
		}
}

df  <- data.frame(df$DEP,df$ESS,df$famille,df[,getIndexCol("nb_etab",df)])

colnames(df)  <- c("DEP","ESS","famille","nb_etab")
df$DEP  <- rep(nom_DEP,nrow(df))

new_data  <-  data.frame("DEP"=character(),"ESS"=character(),"famille"=character(),"var"=numeric())
for(ESS_val in df$ESS%>%unique ){
				tmp  <- df%>% filter(ESS==ESS_val)
				tot  <- tmp[which(tmp$famille=="Ensemble"),"nb_etab"]
				for (row in 1:nrow(tmp)){
					tmp[row,getIndexCol("nb_etab",tmp)] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
				}
				new_data  <- rbind(new_data,tmp)
}
rbind(first_part_bis,new_data)




#-------------------
# EPCI
nom_EPCI  <- "CC Faucigny-Glières" 
df  <- tables[["outputs/2018/EPCI_T1.csv"]]
df  <- df %>% filter(nom_complet==nom_EPCI & jurid1 %in%c("1-ASSO+FOND","2-COOPERATIVES","3-MUTUELLES","5A-PUBLIC","5B-PRIVE","6-TOTAL"))



for (i in 1:nrow(df)){
	splitted  <- strsplit(df$jurid1[i],"-")[[1]]
	
	df$jurid1[i]  <- splitted[2]
}

df  <-  data.frame(df$nom_complet,df$jurid1,df[,getIndexCol("nb_etab",df)])

colnames(df) <- c("EPCI","famille","nb_etab")

new_data  <-  data.frame("EPCI"=character(),"famille"=character(),"var"=numeric())

tot  <- df[which(df$famille=="TOTAL"),"nb_etab"]
for (row in 1:nrow(df)){
			df[row,getIndexCol("nb_etab",df)] <- round(df[row,getIndexCol("nb_etab",df)] /tot,2)*100
}








#--------------------------------------


# TABS2 - PART 3 : Nombre d'etab par Taille des etablissements

#-----------------------


#------------------
# REG
df  <- tables[["outputs/2018/TC14.csv"]]



pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
colnames(pattern)  <-  c("ESS","famille")
patterns  <- paste(pattern$ESS,pattern$famille,sep="")


first_part  <-df%>% filter(REG=="France entière")

first_part  <- first_part %>%filter(paste(ESS,famille,sep="") %in% patterns)  

first_part[which(first_part$famille=="Ensemble"),]$famille   <-  "ESS"
for (i in 1:nrow(first_part)){
	splitted  <- strsplit(first_part$taille_etab[i],"- ")[[1]]
	if(length(splitted)==1){
		first_part$taille_etab[i]  <- splitted[1]
	}else{
		first_part$taille_etab[i]  <- splitted[2]
	}
}


first_part  <- data.frame(first_part$REG,first_part$famille,first_part$taille_etab,first_part[,getIndexCol("nb_etab",first_part)])

colnames(first_part)  <- c("REG","famille","taille_etab","nb_etab")


first_part_bis  <-  data.frame("REG"=character(),"famille"=character(),"taille_etab"=character(),"var"=numeric(),"prct"=numeric())
for(fam in first_part$famille%>%unique ){
	tmp  <- first_part%>% filter(famille==fam)
	tot  <- tmp[which(tmp$taille_etab=="Toutes entreprises"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,"prct"] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	first_part_bis  <- rbind(first_part_bis,tmp)
}




df  <- df %>% filter(REG=="Grand-Est"  )
df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns)  
df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
for (i in 1:nrow(df)){
	splitted  <- strsplit(df$taille_etab[i],"- ")[[1]]
	if(length(splitted)==1){
		df$taille_etab[i]  <- splitted[1]
	}else{
		df$taille_etab[i]  <- splitted[2]
	}
}


df  <- data.frame(df$REG,df$famille,df$taille_etab,df[,getIndexCol("nb_etab",df)])
colnames(df)  <- c("REG","famille","taille_etab","nb_etab")
new_dat  <-  data.frame("REG"=character(),"famille"=character(),"taille_etab"=character(),"var"=numeric(),"prct"=numeric())
for(fam in df$famille%>%unique ){
	tmp  <- df%>% filter(famille==fam)
	tot  <- tmp[which(tmp$taille_etab=="Toutes entreprises"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,"prct"] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	new_dat  <- rbind(new_dat,tmp)
}

rbind(new_dat,first_part_bis)
#------------------
# DEP

df  <- tables[["outputs/2018/TC2.csv"]]


nom_DEP  <- "Marne"

code  <-available_DEP%>%filter(nom==nom_DEP)%>% select(code) 
code  <- code[[1]]


pattern  <- rbind(c("ESS","Ensemble"),c("Hors ESS","Public"),c("Hors ESS","Privé Hors ESS"))%>% data.frame
colnames(pattern)  <-  c("ESS","famille")
patterns  <- paste(pattern$ESS,pattern$famille,sep="")


first_part  <-df%>% filter(DEP=="France entière")

first_part  <- first_part %>%filter(paste(ESS,famille,sep="") %in% patterns)  

first_part[which(first_part$famille=="Ensemble"),]$famille   <-  "ESS"
for (i in 1:nrow(first_part)){
	splitted  <- strsplit(first_part$taille_etab[i],"- ")[[1]]
	if(length(splitted)==1){
		first_part$taille_etab[i]  <- splitted[1]
	}else{
		first_part$taille_etab[i]  <- splitted[2]
	}
}


first_part  <- data.frame(first_part$DEP,first_part$famille,first_part$taille_etab,first_part[,getIndexCol("nb_etab",first_part)])

colnames(first_part)  <- c("DEP","famille","taille_etab","nb_etab")


first_part_bis  <-  data.frame("DEP"=character(),"famille"=character(),"taille_etab"=character(),"var"=numeric(),"prct"=numeric())
for(fam in first_part$famille%>%unique ){
	tmp  <- first_part%>% filter(famille==fam)
	tot  <- tmp[which(tmp$taille_etab=="Toutes entreprises"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,"prct"] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	first_part_bis  <- rbind(first_part_bis,tmp)
}




df  <- df %>% filter(as.numeric(DEP)==as.numeric(code)  )
df  <- df %>%filter(paste(ESS,famille,sep="") %in% patterns)  
df[which(df$famille=="Ensemble"),]$famille   <-  "ESS"
for (i in 1:nrow(df)){
	splitted  <- strsplit(df$taille_etab[i],"- ")[[1]]
	if(length(splitted)==1){
		df$taille_etab[i]  <- splitted[1]
	}else{
		df$taille_etab[i]  <- splitted[2]
	}
}
df$DEP  <- rep(nom_DEP,nrow(df))

df  <- data.frame(df$DEP,df$famille,df$taille_etab,df[,getIndexCol("nb_etab",df)])
colnames(df)  <- c("DEP","famille","taille_etab","nb_etab")
new_dat  <-  data.frame("DEP"=character(),"famille"=character(),"taille_etab"=character(),"var"=numeric(),"prct"=numeric())
for(fam in df$famille%>%unique ){
	tmp  <- df%>% filter(famille==fam)
	tot  <- tmp[which(tmp$taille_etab=="Toutes entreprises"),"nb_etab"]
	for (row in 1:nrow(tmp)){
		tmp[row,"prct"] <- round(tmp[row,getIndexCol("nb_etab",tmp)] /tot,2)*100
	}
	new_dat  <- rbind(new_dat,tmp)
}

tmp  <- rbind(new_dat,first_part_bis)
# TC14.csv
tmp  <- tmp %>% filter(DEP==nom_DEP)%>% select(-DEP,-prct)

 jsonlite::toJSON(tmp,dataframe="rows",auto_unbox = FALSE,rownames=FALSE)
#-----------------
# EPCI

# Pas d'information


