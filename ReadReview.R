## ---------------------------
##
## Script name: ReadReview
##
## Purpose of script: Reads in a single trip's catch review file from the EM
##    project. The files are produced by NOAA staff following NOAA's audit of
##    the EM service provider's review for the trip.
##
## Author: Dr. George A. Maynard
##
## Date Created: 2020-05-20
##
## Copyright (c) George Alphonse Maynard, 2020
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
##
## Notes: The difference between this file and the original (master) branch is
##   that this file includes the summary data worksheets
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
library(XLConnect)
library(lubridate)
library(stringdist)
## ---------------------------

## load up our functions into memory

## ---------------------------

## Set the filename to read in
filename="../Catch Reviews FY19-20200512T202011Z-001/Catch Reviews FY19/Catch_review_20190430_KathrynLeigh.xlsx"
## The first sheet of the file is the provider data
provider=readWorksheetFromFile(
  file=filename,
  sheet=1,
  startRow=1,
  endCol=17
)
## Format the data
provider$ID=as.numeric(as.character(provider$ID))
provider$Timestamp=ymd_hms(as.character(provider$Timestamp))
provider$Latitude=as.numeric(as.character(provider$Latitude))
provider$Longitude=as.numeric(as.character(provider$Longitude))
provider$Description=as.character(provider$Description)
provider$Comments=as.character(provider$Comments)
provider$Estimated.by=as.character(provider$Estimated.by)
provider$Estimated.weight=as.numeric(as.character(provider$Estimated.weight))
provider$INC.life.status=as.character(provider$INC.life.status)
provider$Discard.Condition=as.character(provider$Discard.Condition)
provider$Length=as.numeric(as.character(provider$Length))
provider$Quantity=as.numeric(as.character(provider$Quantity))
provider$Retained=as.character(provider$Retained)
provider$Species=as.character(provider$Species)
provider$Gear.Type=as.character(provider$Gear.Type)
provider$Reviewer=as.character(provider$Reviewer)
provider$Vessel=as.character(provider$Vessel)

## The second worksheet is the FSB (NOAA Fisheries Sampling Branch) review
fsb=readWorksheetFromFile(
  file=filename,
  sheet=2,
  startRow=1,
  endCol=17
)
## Format the data
fsb$ID=as.numeric(as.character(fsb$ID))
fsb$Timestamp=ymd_hms(as.character(fsb$Timestamp))
fsb$Latitude=as.numeric(as.character(fsb$Latitude))
fsb$Longitude=as.numeric(as.character(fsb$Longitude))
fsb$Description=as.character(fsb$Description)
fsb$Comments=as.character(fsb$Comments)
fsb$Estimated.by=as.character(fsb$Estimated.by)
fsb$Estimated.weight=as.numeric(as.character(fsb$Estimated.weight))
fsb$INC.life.status=as.character(fsb$INC.life.status)
fsb$Discard.Condition=as.character(fsb$Discard.Condition)
fsb$Length=as.numeric(as.character(fsb$Length))
fsb$Quantity=as.numeric(as.character(fsb$Quantity))
fsb$Retained=as.character(fsb$Retained)
fsb$Species=as.character(fsb$Species)
fsb$Gear.Type=as.character(fsb$Gear.Type)
fsb$Reviewer=as.character(fsb$Reviewer)
fsb$Vessel=as.character(fsb$Vessel)

## Standardize species names
species=read.csv("species.csv")
fsb$SPECIES=NA
## Use fuzzy matching to assign the most likely standardized species name based
## on the existing species value
for(i in 1:nrow(fsb)){
  ## if the Species value is not NA
  if(is.na(toupper(fsb$Species[i]))==FALSE){
    ## calculate the minimum string distance from the existing species value to 
    ## common non-standardized values
    b=min(
      stringdist(
        a=toupper(fsb$Species[i]),
        b=toupper(species$PEBKAC)
      )
    )
    ## assign the standardized value that matches the closest non-standardized
    ## value
    sv=species$AFS[
      which(
        stringdist(
          a=toupper(fsb$Species[i]),
          b=toupper(species$PEBKAC)
        )==b
      )
      ]
    if(length(unique(sv))==1){
      fsb$SPECIES[i]=as.character(sv[1])
    }
  } 
}
provider$SPECIES=NA
## Use fuzzy matching to assign the most likely standardized species name based
## on the existing species value
for(i in 1:nrow(provider)){
  ## if the Species value is not NA
  if(is.na(toupper(provider$Species[i]))==FALSE){
    ## calculate the minimum string distance from the existing species value to 
    ## common non-standardized values
    b=min(
      stringdist(
        a=toupper(provider$Species[i]),
        b=toupper(species$PEBKAC)
      )
    )
    ## assign the standardized value that matches the closest non-standardized
    ## value
    sv=species$AFS[
      which(
        stringdist(
          a=toupper(provider$Species[i]),
          b=toupper(species$PEBKAC)
        )==b
      )
    ]
    if(length(unique(sv))==1){
      provider$SPECIES[i]=as.character(sv[1])
    }
  } 
}
## The third through seventh sheets are generated by FSB and include some
## comparison metrics. Without knowing what their methodology is for matching
## data between the two raw sheets (provider and fsb), it's hard to say anything
## about the value of these sheets. In the interest of ballparking the
## comparison analysis, we will be using these sheets initially. The third sheet
## is still skipped because it contains matched raw data rather than summary
## data. So, starting with the fourth sheet...
## The fourth sheet contains the count of individual fish by species
counts=readWorksheetFromFile(
  file=filename,
  sheet=4,
  startRow=1,
  endCol=4
)
## Eliminate "Grand Total" and extraneous notes lines that don't fit in the table
counts=counts[1:which(counts$Species=="Grand Total")-1,]
## Standardize species names
counts$SPECIES=NA
## Use fuzzy matching to assign the most likely standardized species name based
## on the existing species value
for(i in 1:nrow(counts)){
  ## if the Species value is not NA
  if(is.na(toupper(counts$Species[i]))==FALSE){
    ## calculate the minimum string distance from the existing species value to 
    ## common non-standardized values
    b=min(
      stringdist(
        a=toupper(counts$Species[i]),
        b=toupper(species$PEBKAC)
      )
    )
    ## assign the standardized value that matches the closest non-standardized
    ## value
    sv=species$AFS[
      which(
        stringdist(
          a=toupper(counts$Species[i]),
          b=toupper(species$PEBKAC)
        )==b
      )
      ]
    if(length(unique(sv))==1){
      counts$SPECIES[i]=as.character(sv[1])
    }
  } 
}
## Calculate difference between FSB estimate and provider estimate
counts$deltaFish=abs(counts$FSB-counts$TEEM)

## The fifth sheet in the workbook contains weight comparisons between the FSB
## review and the third party provider review
weights=readWorksheetFromFile(
  file=filename,
  sheet=5,
  startRow=1,
  endCol=4
)
## Eliminate "Grand Total" and extraneous notes lines that don't fit in the table
weights=weights[1:which(weights$Species=="Grand Total")-1,]
weights$SPECIES=NA
## Use fuzzy matching to assign the most likely standardized species name based
## on the existing species value
for(i in 1:nrow(weights)){
  ## if the Species value is not NA
  if(is.na(toupper(weights$Species[i]))==FALSE){
    ## calculate the minimum string distance from the existing species value to 
    ## common non-standardized values
    b=min(
      stringdist(
        a=toupper(weights$Species[i]),
        b=toupper(species$PEBKAC)
      )
    )
    ## assign the standardized value that matches the closest non-standardized
    ## value
    sv=species$AFS[
      which(
        stringdist(
          a=toupper(weights$Species[i]),
          b=toupper(species$PEBKAC)
        )==b
      )
      ]
    if(length(unique(sv))==1){
      weights$SPECIES[i]=as.character(sv[1])
    }
  } 
}
weights$deltaWeights=abs(weights$FSB-weights$TEEM)

## The sixth and seventh sheets in the workbook describe how many length records
## match exactly and within 2 cm between the provider data and the FSB data. For
## our purposes, we'll combine these datasets into two columns in the same table
## "exact" will give the number of individual fish with exact length matches and
## "within2" will give the number of individual fish with length matches within 
## a tolerance of 2 cm. "total" is the total number of fish measured. 
exacts=readWorksheetFromFile(
  file=filename,
  sheet=6,
  startRow=1,
  endCol=4
)
## Eliminate "Grand Total" and extraneous notes lines that don't fit in the table
exacts=exacts[1:which(exacts$Species=="Grand Total")-1,]
exacts$SPECIES=NA
## Use fuzzy matching to assign the most likely standardized species name based
## on the existing species value
for(i in 1:nrow(exacts)){
  ## if the Species value is not NA
  if(is.na(toupper(exacts$Species[i]))==FALSE){
    ## calculate the minimum string distance from the existing species value to 
    ## common non-standardized values
    b=min(
      stringdist(
        a=toupper(exacts$Species[i]),
        b=toupper(species$PEBKAC)
      )
    )
    ## assign the standardized value that matches the closest non-standardized
    ## value
    sv=species$AFS[
      which(
        stringdist(
          a=toupper(exacts$Species[i]),
          b=toupper(species$PEBKAC)
        )==b
      )
      ]
    if(length(unique(sv))==1){
      exacts$SPECIES[i]=as.character(sv[1])
    }
  } 
}
exacts$diff=abs(exacts$Match-exacts$Total)
w2=readWorksheetFromFile(
  file=filename,
  sheet=7,
  startRow=1,
  endCol=4
)
## Eliminate "Grand Total" and extraneous notes lines that don't fit in the table
w2=w2[1:which(w2$Species=="Grand Total")-1,]
w2$SPECIES=NA
## Use fuzzy matching to assign the most likely standardized species name based
## on the existing species value
for(i in 1:nrow(w2)){
  ## if the Species value is not NA
  if(is.na(toupper(w2$Species[i]))==FALSE){
    ## calculate the minimum string distance from the existing species value to 
    ## common non-standardized values
    b=min(
      stringdist(
        a=toupper(w2$Species[i]),
        b=toupper(species$PEBKAC)
      )
    )
    ## assign the standardized value that matches the closest non-standardized
    ## value
    sv=species$AFS[
      which(
        stringdist(
          a=toupper(w2$Species[i]),
          b=toupper(species$PEBKAC)
        )==b
      )
      ]
    if(length(unique(sv))==1){
      w2$SPECIES[i]=as.character(sv[1])
    }
  } 
}
exacts$w2=w2$X..2
exacts$totalDiff=exacts$diff-exacts$w2
## The eighth sheet in the workbook is a summary file that is essentially a
## block of text, presumably produced by the FSB reviewer, describing any
## discrepencies between the outputs of the two reviews
fsbSummary=readWorksheetFromFile(
  file=filename,
  sheet=8,
  startRow=0
)
fsbSummary=colnames(fsbSummary)
## Make the text string human readable
fsbSummary=gsub(
  pattern="\\.\\.",
  replacement="\\! ",
  x=fsbSummary
)
fsbSummary=gsub(
  pattern="\\.",
  replacement=" ",
  x=fsbSummary
)
fsbSummary=gsub(
  pattern="\\!",
  replacement="\\.",
  x=fsbSummary
)
## ---------------------------

# ## Create a blank reconciliation table
# recon=data.frame(
#   p_Timestamp=character(),
#   f_Timestamp=character(),
#   p_Lat=double(),
#   f_Lat=double(),
#   p_Lon=double(),
#   f_Lon=double(),
#   p_Desc=character(),
#   f_Desc=character(),
#   p_Com=character(),
#   f_Com=character(),
#   p_Wgt=double(),
#   f_Wgt=double(),
#   p_Len=double(),
#   f_Len=double(),
#   p_Quan=integer(),
#   f_Quan=integer(),
#   SPECIES=character(),
#   p_Rev=character(),
#   f_Rev=character(),
#   notes=character(),
#   stringsAsFactors=FALSE
# )
# recon[0:nrow(provider),]=NA
# ## Set parameters for comparison
# ## Time Tolerance (seconds)
# timeTolerance=2
# ## Latitude Tolerance (decimal degrees)
# ## Longitude Tolerance (decimal degrees)
# for(i in 1:nrow(provider)){
#   p=provider[i,]
#   recon$p_Timestamp[i]=as.character(ymd_hms(p$Timestamp))
#   recon$p_Lat[i]=p$Latitude
#   recon$p_Lon[i]=p$Longitude
#   recon$p_Desc[i]=p$Description
#   recon$p_Com[i]=p$Comments
#   f=subset(fsb,fsb$Timestamp%in%seq(p$Timestamp-timeTolerance,p$Timestamp+timeTolerance,1))
#   
# }
## ---------------------------
## Create a summary file for the trip
tripSum=list()
tripSum$vessel=fsb$Vessel[1]
tripSum$fsbReviewers=unique(fsb$Reviewer)
tripSum$provReviewers=unique(provider$Reviewer)
tripSum$Timestamps=c(min(c(fsb$Timestamp,provider$Timestamp)),max(c(fsb$Timestamp,provider$Timestamp)))
s=unique(c(counts$SPECIES,exacts$SPECIES,weights$SPECIES))
x=matrix(nrow=length(s),ncol=10)
for(i in 1:length(s)){
  a=subset(counts,counts$SPECIES==s[i])
  b=subset(exacts,exacts$SPECIES==s[i])
  d=subset(weights,weights$SPECIES==s[i])
  x[i,1]=s[i]
  x[i,2]=ifelse(nrow(a)==1,a$FSB,0)
  x[i,3]=ifelse(nrow(a)==1,a$TEEM,0)
  x[i,4]=ifelse(nrow(a)==1,a$deltaFish)
  x[i,5]=ifelse(nrow(b)==1,b$Total,0)
  x[i,6]=ifelse(nrow(b)==1,b$Match,0)
  x[i,7]=ifelse(nrow(b)==1,b$w2,0)
  x[i,8]=ifelse(nrow(d)==1,d$FSB,0)
  x[i,9]=ifelse(nrow(d)==1,d$TEEM,0)
  x[i,10]=ifelse(nrow(d)==1,d$deltaWeights,0)
}
x=as.data.frame(x)
colnames(x)=c("SPECIES","fCount","pCount","diffCount","nFish","nMatch",
  "nW2Match","fWeight","pWeight","diffWeight")
tripSum$data=x