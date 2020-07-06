## ---------------------------
##
## Script name: HADDOCK_LLB.R
##
## Purpose of script:analysis of count only haddock longline discards
##
## Author: George A. Maynard
##
## Date Created: 2020-06-30
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

## ---------------------------

## load up our functions into memory

## ---------------------------
## Load necessary packages
library(RJSONIO)
library(lubridate)
library(dplyr)
library(SimDesign)
library(sm)
start=Sys.time()
## Select a geartype from the following list: Jig, Gillnet, OtterTrawl, Longline
g="LLB"
## Select a discard species from the following gear-specific lists
## Jig: HADDOCK, COD, POLLOCK, WITCH FLOUNDER, PLAICE
## Gillnet: HADDOCK, POLLOCK
## OtterTrawl: WINDOWPANE, WINTER FLOUNDER, YELLOWTAIL FLOUNDER, WITCH FLOUNDER, PLAICE
## Longline: HADDOCK, COD
s="HADDOCK"
## Read in real discard by length data
setwd("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Subsampling")
MasterLengths=read.csv("Lengths.csv")
## Subset out just the data of interest
data=subset(MasterLengths,MasterLengths$GEAR==g&MasterLengths$SPECIES==s)
lengths=subset(data,data$QUANTITY==1&data$LENGTH>0&is.na(data$LENGTH)==FALSE)$LENGTH
numTrips=length(unique(data$VTR))
## Create blank lists for storing trip information
discards=list()
extrapolations=list()
for(i in 1:numTrips){
  ## Subset out data for one trip
  trip=subset(data,data$VTR==unique(data$VTR)[i])
  ## Break data up into lines that contain single or multiple fish
  singles=subset(trip,trip$QUANTITY==1)
  multiples=subset(trip,trip$QUANTITY>1)
  ## separate singles into knowns and unknowns
  knowns=subset(singles,singles$LENGTH>0&is.na(singles$LENGTH)==FALSE)
  unknowns=subset(singles,singles$LENGTH==0|is.na(singles$LENGTH)==TRUE)
  ## If any lines contain unmeasured fish or multiple fish, extrapolation is
  ## necessary to estimate a discard weight
  extrapolated=(nrow(unknowns)+nrow(multiples))
  ## Populate all unmeasured fish with lengths using the distribution from
  ## this species / geartype combo
  simFish=sample(lengths,extrapolated,replace=TRUE)
  ## Record whether the discards contain extrapolations or not
  extrapolations[[i]]=extrapolated>0
  discards[[i]]=c(knowns$LENGTH,simFish)
}
## Read in the lw equations from Wigley et al.
lw=read.csv(file="LengthWeightEquations.csv")
## Convert all discard lengths to weights
for(i in 1:numTrips){
  discards[[i]]=exp(lw$lnAlpha[which(lw$Species==s)]+lw$Beta[which(lw$Species==s)]*log(discards[[i]]))*2.204623
}
## Calculate known weights for each trip
knowns=rep(NA,numTrips)
for(i in 1:numTrips){
  knowns[i]=sum(discards[[i]])
}
## Set bootstrap number
boot=100
## For each subsample number
for(subSample in c(1,5,10,15,20,25)){
  ## Estimate the weight boot times for each trip
  estimates=matrix(nrow=numTrips,ncol=boot)
  for(j in 1:boot){
    for(i in 1:numTrips){
      if(length(discards[[i]])>subSample){
        estimates[i,j]=(sum(sample(discards[[i]],subSample))/subSample)*length(discards[[i]])
      } else {
        estimates[i,j]=sum(discards[[i]])
      }
    }
  }
  est1=colSums(estimates)
  ## Plot CV for all estimates (including trips with <= subsample discards)
  if(diff(range(est1))<1000){
    est1=est1
    unit="(lbs)"
    k=sum(knowns)
  }
  if(diff(range(est1))>1000){
    est1=est1/10
    unit="(x10 lbs)"
    k=sum(knowns)/10
  }
  # hist(est1,
  #      breaks=seq(round(floor(min(est1)/10)*10,-1),round(ceiling(max(est1)/10)*10,-1),10),
  #      col='gray',
  #      main=paste(g,s,"SS =",subSample,"CV =",round(sd(est1)/mean(est1),3)*100,"%",sep=" "),
  #      xlab=paste("Estimated Discard Weight for", numTrips,"trips",unit,
  #       length(subset(extrapolations,extrapolations==TRUE)), "extrapolated",sep=" "),
  #      ylab="Number of Estimates"
  # )
  # abline(v=k,col='red',lwd=2)
  hist(
    (est1-k)/numTrips,
    col='gray',
    breaks=seq(-20,20,1),
    main=paste(
      "SS =",
      subSample,
      "CV =",
      round(sd(est1)/mean(est1),3)*100,
      "%",
      "MEAN = ",
      round(mean((est1-k)/numTrips),1),
      "lbs",
      sep=" "),
    ylab=paste("Estimates (out of) ",boot),
    xlab=paste(
      "Pounds per Trip Difference (Estimator - Known) | 95% CI = ",
      round(quantile((est1-k)/numTrips,c(0.025,0.975))[[1]],1),
      " to ",
      round(quantile((est1-k)/numTrips,c(0.025,0.975))[[2]],1),
      " lbs",
      sep="")
  )
  abline(v=c(-5,5),col='red',lty=2,lwd=1.5)
    # ## Plot CV for only estimates with > subsample discards
  # include=rep(NA,simTrips)
  # for(i in 1:simTrips){
  #   if(length(simTruth[[i]])>subSample){
  #     include[i]=TRUE
  #   } else {
  #     include[i]=FALSE
  #   }
  # }
  # est2=colSums(estimates[which(include==TRUE),])
  # knowns2=sum(knowns[which(include==TRUE)])
  # if(diff(range(est2))<1000){
  #   est2=est2
  #   unit="(lbs)"
  #   k=sum(knowns2)
  # }
  # if(diff(range(est2))>1000){
  #   est2=est2/10
  #   unit="(x10 lbs)"
  #   k=sum(knowns2)/10
  # }
  # hist(est2,
  #      breaks=seq(round(floor(min(est2)/10)*10,-1),round(ceiling(max(est2)/10)*10,-1),10),
  #      dens=15,
  #      main=paste(g,s,"SS =",subSample,"CV =",round(sd(est2)/mean(est2),3)*100,"%",sep=" "),
  #      xlab=paste("Estimated Discard Weight for", simTrips,"trips",unit,sep=" "),
  #      ylab="Number of Estimates"
  # )
  # abline(v=k,col='red',lwd=2)
}

## Read in Review Time data
tdat=read.csv("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/HaddockLongline/FY19_LL Review Times_Count Only Analysis.csv")
## Remove failed trips
tdat=subset(tdat,tdat$type!="failed")
## Add number of hauls
tdat$hauls=as.numeric(as.character(tdat$Fishing.Effort....hauls.sets.))
tdat=subset(tdat,is.na(tdat$hauls)==FALSE)
## Use a wilcox test to see if there are differences between the reviewed
## trips and the count only trips
wilcox.test(
  x=subset(tdat,tdat$type=="review"&tdat$Review.Time..in.hours.)$Review.Time..in.hours.,
  y=subset(tdat,tdat$type=="count only")$Review.Time..in.hours.,
  alternative="greater"
)
wilcox.test(
  x=subset(tdat,tdat$type=="review"&tdat$Review.Time..in.hours.>2)$hauls,
  y=subset(tdat,tdat$type=="count only")$hauls
)
