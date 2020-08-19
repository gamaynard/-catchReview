## ---------------------------
##
## Script name: weeklyVolumetric.R
##
## Purpose of script: This script will digest and analyze standardized volumetric
##    data as quickly as the data can be provided and provide informative 
##    summaries and figures to project partners
##
## Author: George A. Maynard
##
## Date Created: 2020-08-11
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
## The working directory is set outside of the GitHub repository so that no data
## files are uploaded to GitHub
setwd("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Volumetric/RealTime/")
## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(XLConnect)
library(lubridate)
library(stringdist)
library(samplingbook)
## ---------------------------

## load up our functions into memory

## ---------------------------
## Create a list of files to work through
fileList=dir()
## Create a blank dataframe to be populated by the files as we work through
master=data.frame(
  Trip_Slice_RevID=as.character(),
  ID=as.character(),
  Timestamp=as.character(),
  Latitude=as.numeric(),
  Longitued=as.numeric(),
  Description=as.character(),
  OptionalEvent=as.character(),
  Volumetrics=as.character(),
  EstimatedWeight=as.character(),
  INCLifeStatus=as.character(),
  BasketCount=as.character(),
  BasketLevel=as.character(),
  Quantity=as.character(),
  Count=as.numeric(),
  Species=as.character(),
  GearType=as.character(),
  Reviewer=as.character(),
  Vessel=as.character(),
  HaulNo=as.character()
)
## Combine files to create one master file
for(i in 1:length(fileList)){
  x=XLConnect::readWorksheetFromFile(
    file=fileList[i],
    sheet=1,
    startCol=2,
    colTypes=c(rep("character",3),
      rep("numeric",2),
      rep("character",8),
      "numeric",
      rep("character",5)
    )
  )
  colnames(x)=colnames(master)
  ## Correct the Excel-induced fractions --> dates error
  for(j in 1:nrow(x)){
    if(is.na(x$BasketLevel[j])==FALSE){
      if(nchar(x$BasketLevel[j])==19){
        a=ymd_hms(x$BasketLevel[j])
        b=month(a)/day(a)
        x$BasketLevel[j]=b
        rm(a,b)
      }
    }
  }
  ## Remove all baskets that are < 1/4 full or listed as NA
  x=subset(x,x$BasketLevel!="<1/4")
  x=subset(x,is.na(x$BasketLevel)==FALSE)
  ## Change all basket counts that are "full" to 1
  x$BasketLevel=ifelse(
    toupper(x$BasketLevel)=="FULL",
    1,
    x$BasketLevel
  )
  ## Remove any other baskets with "<" or ">" symbols
  x=subset(x,grepl("[<>]",x$BasketLevel)==FALSE)
  x$BasketLevel=as.numeric(as.character(x$BasketLevel))
  master=rbind(master,x)
}
## Read in standardization file for species names
species=read.csv("https://raw.githubusercontent.com/gamaynard/ElectronicMonitoring/master/species.csv")
## Remove any records that don't have a species name
master=subset(master,is.na(master$Species)==FALSE)
## Use fuzzy matching to convert each species name into a standardized value
for(i in 1:nrow(master)){
  ## calculate the minimum string distance from the existing value
  ## to commonly used, non-standardized values
  b=min(
    stringdist(
      a=toupper(master$Species[i]),
      b=toupper(species$PEBKAC)
    )
  )
  ## assign a standardized value that matches the closest non-standardized
  ## value
  sv=species$AFS[
    which(stringdist(
      a=toupper(master$Species[i]),
      b=toupper(species$PEBKAC)
      )==b
    )
  ]
  ## If there is more than one unique value this portion of
  ## the code will throw an error
  if(length(unique(sv))==1){
    master$SPECIES[i]=as.character(sv[1])
  } else {
    stop('ERROR: MULTIPLE SPECIES MATCHES')
  }
}
## Create a list of species of interest to analyze
species=unique(master$SPECIES)
## Standardize all estimates to one basket
master$Quantity=as.numeric(as.character(master$Quantity))
master$estBasket=master$Quantity/master$BasketLevel
## Remove any duplicate records from the data
master=subset(master,duplicated(master$ID)==FALSE)
## Set the working directory to store output plots
setwd("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Volumetric/Plots/")
## Analyze each species individually
for(s in species){
  a=subset(master,master$SPECIES==s&is.na(master$estBasket)==FALSE)
  if(nrow(a)>2){
    png(
      filename=paste0(s,"_BasketDistribution_",Sys.Date(),".png")
    )
    hist(
      a$estBasket,
      xlab="Estimate Fish per Whole Basket",
      ylab="Number of Baskets",
      main=s,
      col='gray',
      xlim=c(0,250),
      ylim=c(0,25),
      breaks=seq(0,250,5)
    )
    lines(
      dnorm(
        seq(0,250,1),
        mean=mean(a$estBasket),
        sd=sd(a$estBasket)
        )*300,
      lty=2
      )
    dev.off()
    precisions=seq(0.5,25,0.5)
    sn=vector(length=length(precisions))
    for(j in 1:length(precisions)){
      sn[j]=sample.size.mean(
        precisions[j],
        sd(a$estBasket)
      )$n
    }
    png(
      filename=paste0(s,"_PrecisionTarget_",Sys.Date(),".png")
    )
    plot(
      (precisions*2)~sn,
      type='l',
      xlab="Samples Needed",
      ylab="95% CI Width (# Fish)",
      lwd=2,
      main=s,
      xlim=c(0,1000),
      ylim=c(0,50)
      )
    abline(v=nrow(a),lty=2,col='blue')
    abline(h=precisions[which(abs(sn-nrow(a))==min(abs(sn-nrow(a))))[1]]*2,
      lty=2,
      col='blue')
    abline(h=5,lty=2,col='red')
    SN=sample.size.mean(
      2.5,
      sd(a$estBasket)
    )$n
    abline(v=SN,lty=2,col='red')
    text(
      x=400,
      y=40,
      labels=paste(
        "Est. ",
        SN-nrow(a),
        " more samples needed",
        sep=""),
      pos=4
      )
    text(
      x=400,
      y=38,
      labels="to reach a precision of 2.5 fish",
      pos=4
      )
    legend(
      "topright",
      legend=c("Target","Present"),
      lty=2,
      col=c('red','blue')
    )
    dev.off()
  }
}