##### Compute Statistical Tables
##### https://www.ecb.europa.eu/home/pdf/research/hfcn/HFCS_Statistical_Tables_Wave1.pdf?475c5eaaa34668e727772c71eeb6497f

library(tcltk2)
library(survey)
library(mitools)

##### define the folder where the HFCS data are stored

name.hfcs = tclvalue(tkgetOpenFile(filetypes="{{RData files} {.RData}} {{rda files} {.rda}}"))
load(name.hfcs)

##### compute variables
D1$DL1000i = as.numeric(!is.na(D1$DL1000))
D2$DL1000i = as.numeric(!is.na(D2$DL1000))
D3$DL1000i = as.numeric(!is.na(D3$DL1000))
D4$DL1000i = as.numeric(!is.na(D4$DL1000))
D5$DL1000i = as.numeric(!is.na(D5$DL1000))

#vars.to.keep = names(D1)
vars.to.keep = c("ID","SA0100","SA0010","IM0100","DA3001","DL1000i","DL1000","DN3001")


imp1 <- D1[ , vars.to.keep ]
imp2 <- D2[ , vars.to.keep ]
imp3 <- D3[ , vars.to.keep ]
imp4 <- D4[ , vars.to.keep ]
imp5 <- D5[ , vars.to.keep ]

imp1 = merge(imp1,H1[,c("ID","HW0010")],by="ID")
imp2 = merge(imp2,H2[,c("ID","HW0010")],by="ID")
imp3 = merge(imp3,H3[,c("ID","HW0010")],by="ID")
imp4 = merge(imp4,H4[,c("ID","HW0010")],by="ID")
imp5 = merge(imp5,H5[,c("ID","HW0010")],by="ID")



hfcs.design = svrepdesign(weights=~HW0010,repweights=W[,-1:-4],
                          data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) ,
                          scale = 1, rscales = rep(1/999,1000), mse = FALSE, type = "bootstrap", 
                          combined.weights = TRUE)



#### TABLE A1
#### All households, median of total assets (DA3001)

v_median_DA3001 = MIcombine(with(hfcs.design,svyquantile(~DA3001,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DA3001_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DA3001,~SA0100,svyquantile,0.5,method="constant",interval.type="quantile")))

v_mean_DL1000i = MIcombine(with(hfcs.design,svymean(~DL1000i)))
v_mean_DL1000i_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000i,~SA0100,svymean)))

v_median_DL1000 = MIcombine(with(hfcs.design,svyquantile(~DL1000,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DL1000_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DL1000,~SA0100,svyquantile,0.5,method="constant",interval.type="quantile")))

v_median_DN3001 = MIcombine(with(hfcs.design,svyquantile(~DN3001,quantiles=0.5,method="constant",interval.type="quantile")))
v_median_DN3001_by_SA0100 = MIcombine(with(hfcs.design,svyby(~DN3001,~SA0100,svyquantile,0.5,method="constant",interval.type="quantile")))




