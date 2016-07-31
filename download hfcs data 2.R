

packages.available = installed.packages()
if (!"httr" %in% row.names(packages.available)) install.packages("httr")
if (!"rvest" %in% row.names(packages.available)) install.packages("rvest")
if (!"haven" %in% row.names(packages.available)) install.packages("haven")
if (!"tcltk2" %in% row.names(packages.available)) install.packages("tcltk2")


library(httr)
library(rvest)
library(haven)
library(tcltk2)


default_path = getwd()
set_config( config( ssl_verifypeer = 0L ) )

# the function to download the data - once you have your logon id (the email address), your password and the CAPTCHA

download.hfcs = function(info,cookie) {
  # Log in
  url_intra = POST('https://www.ecb.europa.eu/secure/hfcs/login.html',body=info,set_cookies(.cookies=cookie),encode="multipart")
  
  url_intra_form = read_html(url_intra$content) %>% html_form()
  print(url_intra_form)
  cook2 = url_intra$cookies$value
  names(cook2) = url_intra$cookies$name
  
  #submit the form back
  url_final = POST(url_intra_form[[1]]$url, body = list(RelayState=url_intra_form[[1]]$fields$RelayState$value,
                                                        SAMLResponse=url_intra_form[[1]]$fields$SAMLResponse$value),
                                                        set_cookies(.cookies=cook2),
                                                        encode="form")
  #write.table(url_intra,file="page.html")
  #tf = tempfile(fileext=".zip")
  #download.file('https://www.ecb.europa.eu/secure/hfcs/download.html?src=HFCS_User_DataBase_1_1_SAS.zip&type=all&sid=057269199b920422b335cfaf9f73d650',destfile = tf)
  #  url_logged = getURL('https://www.ecb.europa.eu/secure/hfcs/page1.html')
  #windows.save_and_unzip_file(tf)
}


# the function to decide the path where to store the data + unzip the archive downloaded from the website

windows.save_and_unzip_file = function(file) {
  path_store = tk_choose.dir(getwd(),"Choose where you want to store HFCS data")
  setwd(path_store)
  file.copy(file,"HFCS_User_DataBase_1_1_SAS.zip")
  unzip("HFCS_User_DataBase_1_1_SAS.zip")
}


# Tcl/Tk code for creating the GUI

# The logon window - first window to collect logon information from the user

Logon_window = function() {
  
  
  # connect to the login page to download the CAPTCHA picture
  
  url = GET('https://www.ecb.europa.eu/secure/hfcs/login.html',verbose(info=TRUE))
  
  cook = url$cookies$value
  names(cook) = url$cookies$name
  
  tf = tempfile(fileext = ".png")
  # extract the `captcha` location string
  #url = gsub('"',"",url)
  captcha = as.character(sub(
    '.*src="([:alphanum:/+])"*' , 
    '\\1' , 
    url
  ))
  captcha = sub('"',"",captcha)
  x = gregexpr(" alt=",captcha)
  
  captcha = substr(captcha,1,x[[1]][1]-1)
  download.file(paste0('https://www.ecb.europa.eu/secure',captcha),destfile=tf)
  
  
  image = tclVar()
  tkimage.create("photo",image,file=tf)
  
  window_log = tktoplevel()
  tktitle(window_log) = "Identification elements for logging on the HFCS intranet"
  Email_address = tclVar("")
  Psswrd = tclVar("")
  Captcha = tclVar("")
  # Email section
  window_log$env$entName <-tk2entry(window_log, width = "60", textvariable = Email_address)
  tkgrid(tk2label(window_log, text = "Please enter your email address:", justify = "left"),
         padx = 10, pady = c(15, 5), sticky = "w")
  tkgrid(window_log$env$entName, padx = 10, pady = c(0, 15))
  # Password section
  window_log$env$entName <-tk2entry(window_log, width = "60", textvariable = Psswrd, show = "*")
  tkgrid(tk2label(window_log, text = "Please enter your password:", justify = "left"),
         padx = 10, pady = c(15, 5), sticky = "w")
  tkgrid(window_log$env$entName, padx = 10, pady = c(0, 15))
  # Captcha section
  tkgrid(tk2label(window_log, text = "Please enter code (see picture below):", justify = "left"),
         padx = 10, pady = c(15, 5), sticky = "w")
  window_log$env$label <- tk2label(window_log, image = image)
  tkgrid(window_log$env$label)
  window_log$env$entName <-tk2entry(window_log, width = "60", textvariable = Captcha)
  tkgrid(window_log$env$entName, padx = 10, pady = c(0, 15))
  
  # OK button
  onOK <- function() {
    tkdestroy(window_log)
    Email_address.Value = tclvalue(Email_address)
    Psswrd.Value = tclvalue(Psswrd)
    Captcha.Value = tclvalue(Captcha)
    
    info_logon = list('userid'=Email_address.Value,
                      'password'=Psswrd.Value,
                      'ct_captcha'=Captcha.Value,
                      'Login'="Login")
                      
    download.hfcs(info=info_logon,cookie=cook)
  }
  
  window_log$env$butOK <-tk2button(window_log, text = "OK", width = -6, command = onOK)
  tkgrid(window_log$env$butOK, padx = 10, pady = c(5, 15))
  tkbind(window_log$env$entName, "<Return>", onOK)
  
  tkfocus(window_log)
  
}

Logon_window()

#setwd(default_path)
#if (file.exists("cookies.txt")) file.remove("cookies.txt")
