##### Compute Statistical Tables
##### https://www.ecb.europa.eu/home/pdf/research/hfcn/HFCS_Statistical_Tables_Wave1.pdf?475c5eaaa34668e727772c71eeb6497f

library(tcltk2)
library(survey)
library(mitools)
library(dplyr)
library(reshape2)
library(sqldf)

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

H <- mutate(H, tenure_status = ifelse(HB0300 %in% levels(H$HB0300)[1:2], "Owner", "Non-owner"))

if ("HW0010" %in% names(D))
    D = merge(D,H[,c("ID","tenure_status")],by="ID") else 
      D = merge(D,H[,c("ID","HW0010","tenure_status")],by="ID")

imp1 <- D[ D$IM0100 == 1, c(vars.to.keep,"HW0010","tenure_status") ]
imp2 <- D[ D$IM0100 == 2, c(vars.to.keep,"HW0010","tenure_status") ]
imp3 <- D[ D$IM0100 == 3, c(vars.to.keep,"HW0010","tenure_status") ]
imp4 <- D[ D$IM0100 == 4, c(vars.to.keep,"HW0010","tenure_status") ]
imp5 <- D[ D$IM0100 == 5, c(vars.to.keep,"HW0010","tenure_status") ]

### solve issues with UDBs for waves 1-2
### missing 1000th replicate weight for FR + some missing values instead of 0s in wave 1
### misssing 750th replicate weight for IE in wave 2
### apply a generic solution consisting of replacing these empty data with the sampling weights HW0010
W <- merge(W, imp1[,c("ID","HW0010")], by = c("ID"))
W <- arrange(W, SA0100, SA0010)
misW <- W %>%
  select(SA0100, starts_with("WR")) %>%
  group_by(SA0100) %>%
  summarise_all(function(x) mean(is.na(x)))
misW <- melt(misW, id.vars = "SA0100", variable.name = "WR", value.name = "miss_WR")
tW <- melt(select(W, SA0100, SA0010, HW0010, starts_with("WR")), id.vars = c("SA0100","SA0010","HW0010"), variable.name = "WR", value.name = "value_WR")

tW <- sqldf("select a.*, b.miss_WR from tW as a, misW as b where a.SA0100 = b.SA0100 and a.WR = b.WR")
#tW <- merge(tW, misW, by = c("SA0100","WR"))
#tW <- tW[,-c(6,7)]

tW <- mutate(tW,
             value_WR = ifelse(miss_WR == 1, HW0010, value_WR))

W <- dcast(SA0100+SA0010~WR, data = tW, value.var = "value_WR")

#W <- mutate(W,
#            WR1000 = ifelse(is.na(WR1000), HW0010, WR1000))

w <- W[substr(names(W),1,2) == "WR"] 

w[is.na(w)] <- 0

hfcs.design = svrepdesign(weights=~HW0010,repweights=w,
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

#### Owners/Non-owners, mean of total assets (DA3001)

v_median_DA3001_by_ts = MIcombine(with(hfcs.design,svyby(~DA3001,~tenure_status,svyquantile,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DA3001_by_SA0100_ts = MIcombine(with(hfcs.design,svyby(~DA3001,~SA0100+tenure_status,svyquantile,quantiles=0.5,method="constant",interval.type="quantile")))

#### Owners/Non-owners, proportion of indebted households (DL1000 not missing)

v_mean_DL1000i = MIcombine(with(hfcs.design,svymean(~DL1000i)))
v_mean_DL1000i_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000i,~SA0100,svymean)))

#### Owners/Non-owners, median of total liabilities (DL1000)

v_median_DL1000 = MIcombine(with(hfcs.design,svyquantile(~DL1000,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DL1000_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000,~SA0100,svyquantile,quantiles=0.5,method="constant",interval.type="quantile")))

#### Owners/Non-owners, median of net wealth (DN3001)

v_median_DN3001 = MIcombine(with(hfcs.design,svyquantile(~DN3001,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DN3001_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DN3001,~SA0100,svyquantile,quantiles=0.5,method="constant",interval.type="quantile")))






#### TABLE A2
#### All households, mean of total assets (DA3001)

v_mean_DA3001 = MIcombine(with(hfcs.design,svymean(~DA3001)))
v_mean_DA3001_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DA3001,~SA0100,svymean)))

#### All households, proportion of indebted households (DL1000 not missing)

v_mean_DL1000i = MIcombine(with(hfcs.design,svymean(~DL1000i)))
v_mean_DL1000i_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000i,~SA0100,svymean)))

#### All households, mean of total liabilities (DL1000)

v_mean_DL1000 = MIcombine(with(hfcs.design,svymean(~DL1000)))
v_mean_DL1000_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000,~SA0100,svymean)))

#### All households, mean of net wealth (DN3001)

v_mean_DN3001 = MIcombine(with(hfcs.design,svymean(~DN3001)))
v_mean_DN3001_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DN3001,~SA0100,svymean)))



#### Owners/Non-owners, mean of total assets (DA3001)

v_mean_DA3001_by_ts = MIcombine(with(hfcs.design,svyby(~DA3001,~tenure_status,svymean)))
v_mean_DA3001_by_SA0100_ts = MIcombine(with(hfcs.design,svyby(~DA3001,~SA0100+tenure_status,svymean)))

#### Owners/Non-owners, proportion of indebted households (DL1000 not missing)

v_mean_DL1000i = MIcombine(with(hfcs.design,svymean(~DL1000i)))
v_mean_DL1000i_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000i,~SA0100,svymean)))

#### Owners/Non-owners, mean of total liabilities (DL1000)

v_mean_DL1000 = MIcombine(with(hfcs.design,svymean(~DL1000)))
v_mean_DL1000_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000,~SA0100,svymean)))

#### Owners/Non-owners, mean of net wealth (DN3001)

v_mean_DN3001 = MIcombine(with(hfcs.design,svymean(~DN3001)))
v_mean_DN3001_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DN3001,~SA0100,svymean)))

