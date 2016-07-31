packages.available = installed.packages()
if (!"haven" %in% row.names(packages.available)) install.packages("haven")
if (!"tcltk2" %in% row.names(packages.available)) install.packages("tcltk2")
if (!"Hmisc" %in% row.names(packages.available)) install.packages("Hmisc")

library(haven)
library(tcltk2)
library(Hmisc)


default_path = getwd()

#### First you need to download the data

windows.select_data = function(){
  name_folder = tk_choose.dir(default = getwd(),"Choose the folder where the data are stored")
  if (name_folder == "") {}
  else {
    import.hfcs_data(name_folder)
    windows.save_data(list_tab.to.store)
        }
    }


windows.save_data = function(list_file){
  data_name = tclvalue(tkgetSaveFile(initialfile="hfcs.RData",filetypes="{{RData files} {.RData}} {{rda files} {.rda}}"))
  save(list=list_file,file=data_name)
}

import.hfcs_data = function(path_folder) {
  list_files_SAS = list.files(path=path_folder,pattern=".sas7bdat")
  list_files_ASCII = list.files(path=path_folder,pattern=".csv")
  list_files_Stata = list.files(path=path_folder,pattern=".dta")
  list_files_zip = list.files(path=path_folder,pattern=".zip")
  
  if (length(list_files_ASCII)>0) {
    list_tab = gsub(".csv","",list_files_ASCII)
    #assign("list_tab",list_tab,envir=.GlobalEnv)
    setwd(path_folder)
    list_tab.to.store = c()
    for (f in 1:length(list_files_ASCII)) {
      print(paste0("Importing table ",list_tab[f]),quote=FALSE)
      txt = paste0(list_tab[f],"=read.table('",list_files_ASCII[f],"',header=TRUE,sep=',',na.strings='')")
      eval(parse(text=txt))
      #assign(list_tab[f],eval(parse(text=list_tab[f])),envir=.GlobalEnv)
    }
    ##### assign labels to the variables
    list_lab = list_tab[substr(list_tab,1,7) == "labels_"]
    list_tab_lab = gsub("labels_","",list_lab)
    for (t in 1:length(list_lab)) {
      #list_tab.to.label = list_tab[substr(list_tab,1,nchar(list_tab_lab[t]))==list_tab_lab[t]]
      if (list_tab_lab[t] == "W") list_tab.to.label = list_tab_lab[t]
      if (list_tab_lab[t] != "W") list_tab.to.label = paste0(list_tab_lab[t],1:5)
      var.labels = c("ID",as.character(eval(parse(text=list_lab[t]))[,2]))
      list_tab.to.store = c(list_tab.to.store,list_tab.to.label)
      for (k in 1:length(list_tab.to.label)) {
        txt = paste0("label(",list_tab.to.label[k],") = lapply(var.labels, function(x) label(",list_tab.to.label[k],") = x)")
        eval(parse(text=txt))
        assign(list_tab.to.label[k],eval(parse(text=list_tab.to.label[k])),envir=.GlobalEnv)
      }
    }
    assign("list_tab.to.store",list_tab.to.store,envir=.GlobalEnv)
  } 
}


windows.select_data()


