packages.available = installed.packages()
if (!"haven" %in% row.names(packages.available)) install.packages("haven")
if (!"tcltk2" %in% row.names(packages.available)) install.packages("tcltk2")
if (!"Hmisc" %in% row.names(packages.available)) install.packages("Hmisc")

library(haven)
library(tcltk2)
library(Hmisc)


default_path = getwd()

#### First you need to download the data - ASCII or Stata files, and store the data on your computer.
#### HFCS data come as a zip file - it gets automatically unzipped when downloading the file on a Mac, but has to be unzipped
#### on other OSs (Windows, Linux). So far the script does not allow for automatically unzip through R the archive, but the
#### feature should be implemented on a later version.


#### All you have to do is to remember the path of the folder in which you stored the data and run the scripts on R.
#### A nice way of launching the script with less of CTR+C, CTRL+V effort :
#### submit on any R GUI the following code:
#### source("https://raw.githubusercontent.com/pierre-lamarche/Household-Finance-and-Consumption-Survey/master/1.%20Import%20data.R")


### the opening window, to select the folder where the data are stored

windows.select_data = function(){
  if (.Platform$OS.type == "windows") {
    fk_win = tktoplevel()
    tcl("wm", "attributes", fk_win, topmost=TRUE)
    name_folder = tclvalue(tcl("tk_chooseDirectory",initialdir=getwd(),title="Choose the folder where the data are stored",parent=fk_win))
    tkdestroy(fk_win)
  } else {
    name_folder = tk_choose.dir(default = getwd(),"Choose the folder where the data are stored")
  }
  if (name_folder == "") {}
  else {
    import.hfcs_data(name_folder)
    windows.save_data(list_tab.to.store)
  }
}


### the saving window, to select where you want to store the data in R format

windows.save_data = function(list_file){
  if (.Platform$OS.type == "windows") {
    fk_win = tktoplevel()
    tcl("wm", "attributes", fk_win, topmost=TRUE)
    data_name = tclvalue(tkgetSaveFile(initialdir=getwd(),initialfile="hfcs",filetypes="{{RData files} {.RData}} {{rda files} {.rda}}",defaultextension=".RData",parent=fk_win))
    tkdestroy(fk_win)
  } else {
    data_name = tclvalue(tkgetSaveFile(initialdir=getwd(),initialfile="hfcs",filetypes="{{RData files} {.RData}} {{rda files} {.rda}}",defaultextension=".RData"))
  }
  save(list=list_file,file=data_name)
}


### the core function, dealing with the treatment of the data and their conversion into R format

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
  
  if (length(list_files_ASCII)==0 & length(list_files_Stata)>0) {
    list_tab = gsub(".dta","",list_files_Stata)
    assign("list_tab.to.store",list_tab,envir=.GlobalEnv)
    setwd(path_folder)
    for (f in 1:length(list_files_Stata)) {
      print(paste0("Importing table ",list_tab[f]),quote=FALSE)
      txt = paste0(list_tab[f],"=read_dta('",list_files_Stata[f],"')")
      eval(parse(text=txt))
      txt = paste0("names(",list_tab[f],") = toupper(names(",list_tab[f],"))")
      eval(parse(text=txt))
      txt = paste0("names(",list_tab[f],")[which(names(",list_tab[f],")=='SURVEY')] = 'Survey'")
      eval(parse(text=txt))
      assign(list_tab[f],eval(parse(text=list_tab[f])),envir=.GlobalEnv)
    }
  }
  
  if (length(list_files_ASCII) == 0 & length(list_files_Stata) == 0 & length(list_files_SAS) == 0 & length(list_files_zip) > 0) {
    if (length(list_files_zip) == 1) {
      setwd(path_folder)
      unzip(list_files_zip,exdir=paste0(path_folder,"/unzip"))
      import.hfcs_data(paste0(path_folder,"/unzip"))
      }
  }
}


windows.select_data()

