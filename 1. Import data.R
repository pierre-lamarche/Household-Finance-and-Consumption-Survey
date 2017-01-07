packages.available <- installed.packages()
if (!"haven" %in% row.names(packages.available)) install.packages("haven")
if (!"tcltk2" %in% row.names(packages.available)) install.packages("tcltk2")
if (!"Hmisc" %in% row.names(packages.available)) install.packages("Hmisc")

library(haven)
library(tcltk2)
library(Hmisc)


default_path <- getwd()

#### First you need to download the data - ASCII or Stata files, and store the data on your computer.
#### HFCS data come as a zip file - it gets automatically unzipped when downloading the file on a Mac, but has to be unzipped
#### on other OSs (Windows, Linux). So far the script does not allow for automatically unzip through R the archive, but the
#### feature should be implemented on a later version.


#### All you have to do is to remember the path of the folder in which you stored the data and run the scripts on R.
#### A nice way of launching the script with less of CTR+C, CTRL+V effort :
#### submit on any R GUI the following code:
#### source("https://raw.githubusercontent.com/pierre-lamarche/Household-Finance-and-Consumption-Survey/master/1.%20Import%20data.R")


### the opening window, to select the folder where the data are stored

windows.select_data <- function(){
  memorySaving <- (memory.size() < 2000)
  if (.Platform$OS.type == "windows") {
    fk_win <- tktoplevel()
    tcl("wm", "attributes", fk_win, topmost=TRUE)
    name_folder <- tclvalue(tcl("tk_chooseDirectory",initialdir=getwd(),title="Choose the folder where the data are stored",parent=fk_win))
    tkdestroy(fk_win)
  } else {
    name_folder <- tk_choose.dir(default = getwd(),"Choose the folder where the data are stored")
  }
  if (name_folder == "") {}
  else {
    import.hfcs_data(name_folder, memorySaving)
    if (!memorySaving) 
      windows.save_data(list_tab.to.store)
  }
}

### the saving window, to select where you want to store the data in R format

windows.save_data <- function(list_file){
  if (.Platform$OS.type == "windows") {
    fk_win = tktoplevel()
    tcl("wm", "attributes", fk_win, topmost=TRUE)
    data_name <- tclvalue(tkgetSaveFile(initialdir=getwd(),initialfile="hfcs",filetypes="{{RData files} {.RData}} {{rda files} {.rda}}",defaultextension=".RData",parent=fk_win))
    tkdestroy(fk_win)
  } else {
    data_name <- tclvalue(tkgetSaveFile(initialdir=getwd(),initialfile="hfcs",filetypes="{{RData files} {.RData}} {{rda files} {.rda}}",defaultextension=".RData"))
  }
  save(list=list_file,file=data_name)
}

### alternatively, to save the data one by one, the windows to select one for all where to store the R tables

windows.save_folder <- function() {
  if (.Platform$OS.type == "windows") {
    fk_win <- tktoplevel()
    tcl("wm", "attributes", fk_win, topmost=TRUE)
    name_folder <- tclvalue(tcl("tk_chooseDirectory",initialdir=getwd(),title="Choose the folder where the R tables will be saved",parent=fk_win))
    tkdestroy(fk_win)
  } else {
    name_folder <- tk_choose.dir(default = getwd(),"Choose the folder where the R tables will be saved")
  }
  return(name_folder)
}

### the core function, dealing with the treatment of the data and their conversion into R format

import.hfcs_data <- function(path_folder, saveMemory) {
  list_files_SAS <- list.files(path=path_folder,pattern=".sas7bdat")
  list_files_ASCII <- list.files(path=path_folder,pattern=".csv")
  list_files_Stata <- list.files(path=path_folder,pattern=".dta")
  list_files_zip <- list.files(path=path_folder,pattern=".zip")
  
  if (saveMemory)
    folder.to.save <- windows.save_folder()
  
  if (length(list_files_ASCII)>0) {
    list_tab <- gsub(".csv","",list_files_ASCII)
    setwd(path_folder)
    ##### assign labels to the variables
    list_lab <- list_tab[substr(list_tab,1,7) == "labels_"]
    list_tab.to.store <- list_tab[substr(list_tab,1,7) != "labels_" & substr(list_tab,1,12) != "valuelabels_"]
    
    for (f in 1:length(list_lab)) {
      txt <- paste0(list_lab[f], " <- read.table(\"", list_lab[f], ".csv\", header = FALSE, 
                    sep = \",\", na.strings = '', col.names = c(\"var\", \"label\"))")
      eval(parse(text = txt))
    }

    for (f in 1:length(list_tab.to.store)) {
      cat(paste0("Importing table ",list_tab.to.store[f]),"\n")
      txt <- paste0(list_tab.to.store[f],"<- read.table(\"", list_tab.to.store[f],
                    ".csv\", header=TRUE, sep=\",\", na.strings='')")
      eval(parse(text=txt))
      if (gsub("[1-5]","",list_tab.to.store[f]) %in% substr(list_lab, 8, 10)) {
        cat(paste0(" * Labelling table ", list_tab.to.store[f], "\n"))
        labels.table <- eval(parse(text=paste0("labels_",gsub("[1-5]","",list_tab.to.store[f]))))
        var.labels <- as.character(labels.table$label)
        var.names <- as.character(labels.table$var)
        names.table <- names(eval(parse(text = list_tab.to.store[f])))
        # filter the existing variables and remove duplicate - yes it happens sometimes
        var.labels <- var.labels[var.names %in% names.table]
        var.names <- var.names[var.names %in% names.table]
        var.labels <- var.labels[!duplicated(var.names)]
        var.names <- var.names[!duplicated(var.names)]
        # now sort the labels according to the order in the table
        orderNames <- match(names.table, var.names)
        var.labels <- var.labels[orderNames]
        # apply the label to each variable
        txt <- paste0("label(", list_tab.to.store[f], ") <- lapply(var.labels, function(x) label(",list_tab.to.store[f],") = x)")
        eval(parse(text = txt))
        assign(list_tab.to.store[f],eval(parse(text=list_tab.to.store[f])),envir=.GlobalEnv)
      }
      if (saveMemory) {
        cat(paste0(" * Saving table ", list_tab.to.store[f]))
        save(list = list_tab.to.store[f], file = paste0(folder.to.save, "/", 
                                                        list_tab.to.store[f], ".RData"))
        cat(paste0(" * Removing table and garbage collector..."))
        rm(list = list_tab.to.store[f])
        rm(list = list_tab.to.store[f], envir = .GlobalEnv)
        gc()
      }
    }
    if (!saveMemory) {
      assign("list_tab.to.store",list_tab.to.store,envir=.GlobalEnv)
    }
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

