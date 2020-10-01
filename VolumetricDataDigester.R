## ---------------------------
##
## Script name: VolumetricDataDigester.R
##
## Purpose of script: Imports and cleans data from Excel spreadsheets related
## to volumetric data files from Rhode Island vessels
##
## Author: George A. Maynard
##
## Date Created: 2020-09-30
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
setwd("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Volumetric/Windowpane")
## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(XLConnect)
library(lubridate)
library(stringdist)
library(samplingbook)
library(dunn.test)
## ---------------------------

## load up our functions into memory

## ---------------------------
## Create an empty data frame to store values
master=data.frame(
  Volumetric=as.character(),
  ID=as.numeric(),
  HaulID=as.numeric(),
  Start=as.character(),
  End=as.character(),
  Lat=as.numeric(),
  Lon=as.numeric(),
  Description=as.character(),
  Comment=as.character(),
  EstBy=as.character(),
  EstWeight=as.numeric(),
  Status=as.character(),
  DiscardCondition=as.character(),
  Length=as.numeric(),
  Quantity=as.numeric(),
  Retained=as.character(),
  Species=as.character(),
  GearType=as.character(),
  Reviewer=as.character(),
  Vessel=as.character()
)
## Create a list of files
fileList=dir()
## Import the individual trip records that contain timestamped fish lengths
for(i in fileList){
  x=XLConnect::loadWorkbook(i)
  XLConnect::onErrorCell(x,XLC$ERROR.WARN)
  y=XLConnect::readWorksheet(
    object=x,
    sheet=1,
    startCol=1,
    colTypes=c(
      "character",
      rep("numeric",2),
      rep("character",2),
      rep("numeric",2),
      rep("character",3),
      "numeric",
      "character",
      rep("numeric",3),
      rep("character",5)
    )
  )
  y=subset(y,y$Volumetric=="V")
  colnames(y)=colnames(master)
  master=rbind(master,y)
}
## Change directories and import the actual volumetric data that contains basket
## level information and timestamps
setwd("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Volumetric/RealTime/")
## Create a list of files to work through
fileList=dir()
## Create a blank dataframe to be populated by the files as we work through
key=data.frame(
  Trip_Slice_RevID=as.character(),
  ID=as.character(),
  Timestamp=as.character(),
  Latitude=as.numeric(),
  Longitude=as.numeric(),
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
  ## Remove any rows where the "Description" is NA (blank lines, errors in data
  ## entry, etc.)
  x=subset(x,is.na(x$Description)==FALSE)
  colnames(x)=colnames(key)
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
  ## Remove all baskets that are listed as NA
  x=subset(x,is.na(x$BasketLevel)==FALSE)
  ## Change all basket counts that are "<1/4" to 0
  x$BasketLevel=ifelse(
    x$BasketLevel=="<1/4",
    0,
    x$BasketLevel
  )
  ## Change all basket counts that are "full" to 1
  x$BasketLevel=ifelse(
    toupper(x$BasketLevel)=="FULL",
    1,
    x$BasketLevel
  )
  ## Remove any other baskets with "<" or ">" symbols
  x=subset(x,grepl("[<>]",x$BasketLevel)==FALSE)
  ## If multiple baskets are counted in one cell, combine them
  for(j in 1:nrow(x)){
    if(grepl("-",x$BasketLevel[j])==TRUE){
      x$BasketLevel[j]=sum(
        sapply(
          gsub(
            " ",
            "",
            strsplit(x$BasketLevel[j],"-")[[1]]
          ), 
          function(x) eval(parse(text=x))
        )
      )
    }
  }
  for(k in 1:nrow(x)){
    ## If there is a space in the record
    if(grepl(" ",x$BasketLevel[k])==TRUE){
      x$BasketLevel[k]=sum(
        sapply(
          strsplit(
            x$BasketLevel[k],
            " "
          )[[1]],
          function(x) eval(parse(text=x))
        )
      )
    }
  }
  ## If a line of data contains a fraction instead of a decimal, convert it
  for(k in 1:nrow(x)){
    if(grepl("/",x$BasketLevel[k])==TRUE){
      x$BasketLevel[k]=sum(
        sapply(
          x$BasketLevel[k],
          function(x) eval(parse(text=x))
        )
      )
    }
  }
  x$BasketLevel=as.numeric(as.character(x$BasketLevel))
  key=rbind(key,x)
}
## Current measurments cannot support a 1/3 basket, so eliminate these records
key=subset(
  key,
  round(key$BasketLevel,2)!=0.33
)
## Read in standardization file for species names
species=read.csv("https://raw.githubusercontent.com/gamaynard/ElectronicMonitoring/key/species.csv")
## Remove any records that don't have a species name
key=subset(key,is.na(key$Species)==FALSE)
## Use fuzzy matching to convert each species name into a standardized value
for(i in 1:nrow(key)){
  ## calculate the minimum string distance from the existing value
  ## to commonly used, non-standardized values
  b=min(
    stringdist(
      a=toupper(key$Species[i]),
      b=toupper(species$PEBKAC)
    )
  )
  ## assign a standardized value that matches the closest non-standardized
  ## value
  sv=species$AFS[
    which(stringdist(
      a=toupper(key$Species[i]),
      b=toupper(species$PEBKAC)
    )==b
    )
    ]
  ## If there is more than one unique value this portion of
  ## the code will throw an error
  if(length(unique(sv))==1){
    key$SPECIES[i]=as.character(sv[1])
  } else {
    stop('ERROR: MULTIPLE SPECIES MATCHES')
  }
}
## Remove all duplicate entries
key$dup=duplicated(key$ID)
key=subset(key,key$dup==FALSE)
## Create a list of species of interest to analyze
species=unique(key$SPECIES)
## Convert all basket levels to account for the shape of the basket
key$volume=0
## Calculate the volume of a full basket
D=9.5
r=6
R=7.25
V=(1/3)*pi*D*(r^2+r*R+R^2)
for(i in 1:nrow(key)){
  a=as.numeric(
    strsplit(
      as.character(
        key$BasketLevel[i]
      ),
      "\\."
    )[[1]][1]
  )
  b=as.numeric(
    strsplit(
      as.character(
        format(
          round(
            key$BasketLevel[i],
            2
          ),
          nsmall=2
        )
      ),
      "\\."
    )[[1]][2]
  )/100
  b=ifelse(
    is.na(b),
    0,
    b
  )
  v=ifelse(
    b==0.25,
    (1/3)*pi*2.5*(r^2+r*6.375+6.375^2),
    ifelse(
      b==0.5,
      (1/3)*pi*4.5*(r^2+r*6.75+6.75^2),
      ifelse(
        b==0.75,
        (1/3)*pi*6.5*(r^2+r*7+7^2),
        0
      )
    )
  )
  b=v/V
  key$volume[i]=a+b
}
## Assign each record in the master dataset to a basket in the key dataset
master$EVENT=0
key$ID=as.numeric(as.character(key$ID))
for(i in 1:nrow(key)){
  ## Define a time window around each event in minutes (1/2 tw before and 1/2
  ## after)
  tw=60
  a=ymd_hms(key$Timestamp[i])
  startTime=a-minutes(tw/2)
  endTime=a+minutes(tw/2)
  event=seq.POSIXt(
    from=startTime,
    to=endTime,
    by="sec"
  )
  master$EVENT=ifelse(
    ymd_hms(master$Start)%in%event,
    key$ID[i],
    master$EVENT
  )
}
## Remove all records that failed automatic matching for manual matching later
manual=subset(master,master$EVENT==0)
## Proceed with automatically matched records only
master=subset(master,master$EVENT!=0)
## Merge master and key records after renaming key columns to ALL CAPS
a=select(key,ID,BasketLevel,Species,Quantity,Vessel,Reviewer,Timestamp,volume)
a$EVENT=a$ID
a$ID=NULL
a$SPECIES=a$Species
a$Species=NULL
a$QUANTITY=a$Quantity
a$Quantity=NULL
a$VESSEL=a$Vessel
a$Vessel=NULL
a$REVIEWER=a$Reviewer
a$Reviewer=NULL
a$VOLUME=a$volume
a$volume=NULL
master=merge(master,a)
## Create a new empty data set to store summary information
new=data.frame(
  EVENT=as.numeric(),
  BasketLevel=as.numeric(),
  Volume=as.numeric(),
  rCount=as.numeric(),
  cCount=as.numeric(),
  SumWeight=as.numeric(),
  CountWeight=as.numeric(),
  Reviewer=as.character()
)
for(i in unique(master$EVENT)){
  a=subset(master,master$EVENT==i)
  bl=unique(a$BasketLevel)
  v=unique(a$VOLUME)
  rc=unique(a$QUANTITY)
  cc=nrow(a)
  sw=sum(a$EstWeight)
  cw=nrow(a)*0.7233
  r=unique(a$REVIEWER)
  b=as.data.frame(cbind(i,bl,v,rc,cc,sw,cw,r))
  colnames(b)=colnames(new)
  new=rbind(new,b)
}
new$EVENT=as.numeric(as.character(new$EVENT))
new$BasketLevel=as.numeric(as.character(new$BasketLevel))
new$Volume=as.numeric(as.character(new$Volume))
new$rCount=as.numeric(as.character(new$rCount))
new$cCount=as.numeric(as.character(new$cCount))
new$SumWeight=as.numeric(as.character(new$SumWeight))
new$CountWeight=as.numeric(as.character(new$CountWeight))
new$Reviewer=as.character(new$Reviewer)
## Remove records with counts that don't match
problems=subset(
  new,
  as.numeric(as.character(new$rCount))!=as.numeric(as.character(new$cCount))
)
new=subset(new,new$EVENT%in%problems$EVENT==FALSE)
## Counts not matching is a function of subsampling on board. 
## For these records, calculate an average weight per fish (from the measured
## individuals) and extrapolate it out to the total number of fish counted. 
for(i in 1:nrow(problems)){
  avg=problems$SumWeight[i]/problems$cCount[i]
  problems$SumWeight[i]=avg*problems$rCount[i]
  problems$CountWeight[i]=problems$rCount[i]*0.7233
}
## Merge the fixed records back in 
new=rbind(new,problems)
## Plot the results and check for differences between weights
par(mai=c(1.02,0.82,0.82,0.42))
boxplot(
  new$SumWeight~new$BasketLevel,
  xlab="Basket Level (as marked)",
  ylab="Estimated Weight (sum of individual weights, lbs)",
  main="Windowpane",
  las=2
  )
text(
  x=seq(1,length(unique(new$BasketLevel)),1),
  y=max(new$SumWeight),
  labels=table(new$BasketLevel)
)
dunn.test(new$SumWeight,new$BasketLevel)
## Standardize all records to one basket worth of fish
new$SBS=new$SumWeight/new$Volume
boxplot(
  subset(new,new$BasketLevel!=0)$SBS~subset(new,new$BasketLevel!=0)$BasketLevel,
  xlab="Basket Level (as marked)",
  ylab="Standardized Weight (single basket, lbs)",
  main="Windowpane",
  las=2
)
text(
  x=seq(1,length(unique(new$BasketLevel)),1),
  y=max(subset(new,new$BasketLevel!=0)$SBS),
  labels=table(subset(new,new$BasketLevel!=0)$BasketLevel)
)
dunn.test(
  subset(new,new$BasketLevel!=0)$SBS,
  subset(new,new$BasketLevel!=0)$BasketLevel
  )
