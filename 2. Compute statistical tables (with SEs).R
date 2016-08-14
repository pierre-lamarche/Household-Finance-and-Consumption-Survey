##### Compute Statistical Tables
##### https://www.ecb.europa.eu/home/pdf/research/hfcn/HFCS_Statistical_Tables_Wave1.pdf?475c5eaaa34668e727772c71eeb6497f

library(tcltk2)
library(survey)
library(mitools)

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

imp1 <- D[ D$IM0100 == 1, c(vars.to.keep,"HW0010","tenure_status") ]
imp2 <- D[ D$IM0100 == 2, c(vars.to.keep,"HW0010","tenure_status") ]
imp3 <- D[ D$IM0100 == 3, c(vars.to.keep,"HW0010","tenure_status") ]
imp4 <- D[ D$IM0100 == 4, c(vars.to.keep,"HW0010","tenure_status") ]
imp5 <- D[ D$IM0100 == 5, c(vars.to.keep,"HW0010","tenure_status") ]



hfcs.design = svrepdesign(weights=~HW0010,repweights=W[,-1:-4],
                          data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) ,
                          scale = 1, rscales = rep(1/999,1000), mse = FALSE, type = "bootstrap", 
                          combined.weights = TRUE)



#### TABLE A1
#### All households, median of total assets (DA3001)

v_median_DA3001 = MIcombine(with(hfcs.design,svyquantile(~DA3001,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DA3001_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DA3001,~SA0100,svyquantile,0.5,method="constant",interval.type="quantile")))

#### All households, proportion of indebted households (DL1000 not missing)

v_mean_DL1000i = MIcombine(with(hfcs.design,svymean(~DL1000i)))
v_mean_DL1000i_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000i,~SA0100,svymean)))

#### All households, median of total liabilities (DL1000)

v_median_DL1000 = MIcombine(with(hfcs.design,svyquantile(~DL1000,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DL1000_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000,~SA0100,svyquantile,0.5,method="constant",interval.type="quantile")))

#### All households, median of net wealth (DN3001)

v_median_DN3001 = MIcombine(with(hfcs.design,svyquantile(~DN3001,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DN3001_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DN3001,~SA0100,svyquantile,0.5,method="constant",interval.type="quantile")))




