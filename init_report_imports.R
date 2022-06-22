n_2018=length(list.files("./outputs/2018",all.files=FALSE)[list.files("./outputs/2018",all.files=FALSE)!="nomenclatures"])
n_2017=length(list.files("./outputs/2017",all.files=FALSE)[list.files("./outputs/2017",all.files=FALSE)!="nomenclatures"])

directory<-c(rep("2017",n_2017),rep("2018",n_2018))


scale<-rep("reg_dep",n_2017+n_2018)
filename<-c(
    list.files("./inputs/2017/reg_dep",all.files=FALSE),
    list.files("./inputs/2018/reg_dep",all.files=FALSE)[-length(list.files("./inputs/2018/reg_dep",all.files=FALSE)):-24]
    )

initialize_report=data.frame(directory=directory,
            scale=scale,
            filename=filename
)

write.csv(initialize_report,"./inputs/report_imports.csv",row.names=FALSE)
