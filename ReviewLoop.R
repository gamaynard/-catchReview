## ---------------------------
##
## Script name: ReviewLoop
##
## Purpose of script: This script loops over all review comparison files,
##    executing ReadReview.R on each one and creating a master summary
##    file to analyze
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
## ---------------------------

## load up the packages we will need:  (uncomment as required)

## ---------------------------

## load up our functions into memory

## ---------------------------

## Create a list of files to loop through
fileList=dir(
  path="C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/CatchReview/Catch Reviews FY19-20200512T202011Z-001/Catch Reviews FY19/"
)

## Create an empty dataframe to store results
rSum=data.frame(
  DATE=character(),
  VESSEL=character(),
  SPECIES=character(),
  FSB_Count=integer(),
  FSB_Weight=double(),
  TF_Count=integer(),
  TF_Weight=double(),
  Delta_Count=integer(),
  Ind_Lengths=integer(),
  Exact_Length=integer(),
  W2_Length=integer(),
  Diff_eLength=integer(),
  Diff_wLength=integer(),
  Delta_Weight=double()
)

## Fire up the progress bar
pb=txtProgressBar(
  min=0,
  max=1,
  initial=0,
  char="*",
  style=3
)
for(file in fileList){
  fn=paste("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/CatchReview/Catch Reviews FY19-20200512T202011Z-001/Catch Reviews FY19/",file,sep="")
  source("ReadReview.R")
  s=unique(c(counts$SPECIES,exacts$SPECIES,weights$SPECIES))
  newdat=matrix(nrow=length(s),ncol=14)
  newdat[,3]=s
  newdat[,1]=as.character(floor_date(provider$Timestamp[1],unit="day"))
  newdat[,2]=as.character(provider$Vessel[1])
  newdat[,4]=ifelse(newdat[,3]%in%counts$SPECIES,counts$FSB[which(counts$SPECIES==newdat[,3])],NA)
  newdat[,5]=ifelse(newdat[,3]%in%weights$SPECIES,weights$FSB[which(weights$SPECIES==newdat[,3])],NA)
  newdat[,6]=ifelse(newdat[,3]%in%counts$SPECIES,counts$TEEM[which(counts$SPECIES==newdat[,3])],NA)
  newdat[,7]=ifelse(newdat[,3]%in%weights$SPECIES,weights$TEEM[which(weights$SPECIES==newdat[,3])],NA)
  newdat[,8]=ifelse(newdat[,3]%in%counts$SPECIES,counts$deltaFish[which(counts$SPECIES==newdat[,3])],NA)
  for(sp in newdat[,3]){
    if(sp%in%exacts$SPECIES){
      newdat[which(newdat[,3]==sp),9]=subset(exacts,exacts$SPECIES==sp)$Total
      newdat[which(newdat[,3]==sp),10]=subset(exacts,exacts$SPECIES==sp)$Match
    } else {
      newdat[which(newdat[,3]==sp),9]=NA
      newdat[which(newdat[,3]==sp),10]=NA
    }
  }
  newdat[,11]=ifelse(newdat[,3]%in%exacts$SPECIES,exacts$w2[which(exacts$SPECIES==newdat[,3])],NA)
  newdat[,12]=ifelse(newdat[,3]%in%exacts$SPECIES,exacts$diff[which(exacts$SPECIES==newdat[,3])],NA)
  newdat[,13]=ifelse(newdat[,3]%in%exacts$SPECIES,exacts$totalDiff[which(exacts$SPECIES==newdat[,3])],NA)
  newdat[,14]=ifelse(newdat[,3]%in%weights$SPECIES,weights$deltaWeights[which(weights$SPECIES==newdat[,3])],NA)
  colnames(newdat)=c('DATE','VESSEL','SPECIES','FSB_COUNT','FSB_WEIGHT','TF_COUNT',
    'TF_WEIGHT','DELTA_COUNT','IND_LENGTHS','EXACT_LENGTH','W2_LENGTH',
    'DIFF_ELENGTH','DIFF_WLENGTH','DELTA_WEIGHT')
  newdat=as.data.frame(newdat)
  rSum=as.data.frame(rbind(rSum,newdat))
  setTxtProgressBar(
    pb=pb,
    value=which(fileList==file)/length(fileList)
  )
}  