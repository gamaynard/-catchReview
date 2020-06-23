## ---------------------------
##
## Script name: speciesNames
##
## Purpose of script: This script creates the list of potential
##    species names that other scripts use to standardize species names
##
## Author: George A. Maynard
##
## Date Created: 2020-05-26
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
## In this instance, the working directory should already be set to the 
## github repository https://github.com/gamaynard/-catchReview.git
## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
memory.limit(30000000)     # increase PC memory allowance
## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(XLConnect)
library(tabulizer)
library(dplyr)
library(naniar)
## ---------------------------

## load up our functions into memory

## ---------------------------

## Create a list of files to read in
fileList=dir(
  path="C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/CatchReview/Catch Reviews FY19-20200512T202011Z-001/Catch Reviews FY19/"
)

## Create an empty vector of species names to store the results
sn=vector(length=0)

## Loop over the files, reading each in and collecting species name
## variants from it
for(i in fileList){
  f=paste("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/CatchReview/Catch Reviews FY19-20200512T202011Z-001/Catch Reviews FY19/",i,sep="")
  x=unique(
    readWorksheetFromFile(
      file=f,
      sheet=1,
      startRow=1,
      endCol=17
    )$Species
  )
  y=unique(
    readWorksheetFromFile(
      file=f,
      sheet=2,
      startRow=1,
      endCol=17
    )$Species  
  )
  z=unique(c(x,y))
  sn=c(sn,z)
}
sn=unique(toupper(sn))

## Output the list as a .csv for inclusion in the master species.csv
## sheet
write.csv(
  x=sn,
  file="sn.csv",
  row.names=FALSE
)

