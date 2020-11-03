## ---------------------------
##
## Script name: FY2019_DataCompiler.R
##
## Purpose of script: Brings together VTR, EM, and Dealer Data for FY2019 for
##    analytics and reporting
##
## Author: George A. Maynard
##
## Date Created: 2020-10-19
##
## Copyright (c) George Alphonse Maynard, 2020
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory

## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(RJSONIO)
## ---------------------------

## load up our functions into memory

## ---------------------------
## The three datasets that make up the basis of this analysis are the FY2019 VTR
## data (requested from GARFO), the FY2019 EM data (requested from Teem.Fish),
## and the FY2019 Dealer Data (requested from sector managers). The next step
## reads in each dataset individually. All data are housed in the Electronic 
## Monitoring directory of the CCCFA server. The default starting point for 
## file addresses is 
## "Electronic Monitoring/Georges Analyses/ElectronicMonitoring"
##
## VTR data is read in from a .csv file, which functions as a data frame 
VTR=read.csv(
  file="../../SMAST Science/Data/FY2019-eVTR-GARFO-20201007.csv"
)
## EM data is read in from a .json file which functions as a list of lists
EM=fromJSON(
  content="../ClosedAreaComparisons/FY19/RawData/NOAA_Submissions_2019.json"
)
## Dealer data is read in from a series of .xlsx and .xls files mailed
## from the sector managers. All of this data should be stored in the same 
## directory to enable gathering. the Sustainable Harvest Sector's manager sends
## both eVTR and dealer data in the same file, so it is important to load the 
## correct worksheet from that file
fileList=dir("../ClosedAreaComparisons/FY19/RawData/DealerData/")
Dealer=data.frame()
for(i in 1:length(fileList)){
  filename=paste0(
    "../ClosedAreaComparisons/FY19/RawData/DealerData/",
    fileList[i]
  )
  if(
    grepl(
      pattern="shs",
      x=filename
    )==FALSE
  ){
    partial=XLConnect::readWorksheetFromFile(
      file=filename,
      sheet=1
    )
  } else {
    partial=XLConnect::readWorksheetFromFile(
      file=filename,
      sheet="dealer"
    )
  }
  Dealer=rbind(Dealer,partial)
}


