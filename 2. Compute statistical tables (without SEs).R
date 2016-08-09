##### Compute Statistical Tables
##### https://www.ecb.europa.eu/home/pdf/research/hfcn/HFCS_Statistical_Tables_Wave1.pdf?475c5eaaa34668e727772c71eeb6497f

library(tcltk2)
library(dplyr)
##### define the folder where the HFCS data are stored

name.hfcs = tclvalue(tkgetOpenFile(filetypes="{{RData files} {.RData}} {{rda files} {.rda}}"))
load(name.hfcs)


##### combine the different implicates into one single dataframe

list_table = c("H","HN","D","P","PN")
list.to.remove = c()

for (k in 1:length(list_table)) {
  list = paste0(list_table[k],1:5)
  list.to.remove = c(list.to.remove,list)
  txt = paste0(list_table[k]," = rbind(")
  for (im in 1:4) {
    txt = paste0(txt,list[im],",")
  }
  txt = paste0(txt,list[5],")")
  eval(parse(text=txt))
}

rm(list=list.to.remove)
gc()


##### compute variables
D$DL1000i = as.numeric(!is.na(D$DL1000))

#vars.to.keep = names(D1)
vars.to.keep = c("ID","SA0100","SA0010","IM0100","DA3001","DL1000i","DL1000","DN3001")


##### compute variables in the H-table

H$tenure_status = NA
H[H$HB0300 %in% c(1,2,4),]$tenure_status = "Owner"
H[H$HB0300 == 3,]$tenure_status = "Tenant"


D = merge(D,H[,c("ID","HW0010","tenure_status")],by="ID")



#### TABLE A1
#### All households, median of total assets (DA3001)

median_DA3001_by_SA0100 = D %>%
  group_by(SA0100,IM0100) %>%
  summarize(median_DA3001_im = wtd.quantile(DA3001,weights=HW0010,probs=0.5)) %>%
  group_by(SA0100) %>%
  summarise(median_DA3001 = mean(median_DA3001_im))
median_DA3001 = D %>%
  group_by(IM0100) %>%
  summarize(median_DA3001_im = wtd.quantile(DA3001,weights=HW0010,probs=0.5)) %>%
  summarise(median_DA3001 = mean(median_DA3001_im))

mean_DL1000i_by_SA0100 = D %>%
  group_by(SA0100,IM0100) %>%
  summarize(mean_DL1000i_im = wtd.mean(DL1000i,weights=HW0010)) %>%
  group_by(SA0100) %>%
  summarise(mean_DL1000i = mean(mean_DL1000i_im))
mean_DL1000i = D %>%
  group_by(IM0100) %>%
  summarize(mean_DL1000i_im = wtd.mean(DL1000i,weights=HW0010)) %>%
  summarise(mean_DL1000i = mean(mean_DL1000i_im))