# Household-Finance-and-Consumption-Survey
Use the micro data from the Eurosystem Household Finance and Consumption Survey with R.

This repository contains pieces of codes for:
* downloading the HFCS micro-data (provided that the user has requested the access to the data, 
following the procedure described [here](http://www.ecb.europa.eu/pub/economic-research/research-networks/html/researcher_hfcn.en.html)) - this feature needs to be improved, as it does not work
on every platform so far.
* transferring ASCII or Stata files in R format, in order to use the data with R.
* Computing the figures provided by the European Central Bank in the [statistical tables](https://www.ecb.europa.eu/home/pdf/research/hfcn/HFCS_Statistical_Tables_Wave1.pdf?475c5eaaa34668e727772c71eeb6497f)), with or without standard errors. Calculating standard errors requires a significant additional amount of time; therefore users interested in calculating only the point estimates may gain time.