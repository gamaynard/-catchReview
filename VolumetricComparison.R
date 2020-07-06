## ---------------------------
##
## Script name: VolumetricComparison.R
##
## Purpose of script: Describe the distribution of fish counts in a new
##    volumetric sampling plan being tested for windowpane flounder discards
##
## Author: George A. Maynard
##
## Date Created: 2020-07-06
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
library(samplingbook)
## ---------------------------

## load up our functions into memory

## ---------------------------
## Read in the data
data=read.csv("C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Volumetric/CleanedData.csv")
## Plot the count of fish as a function of the number of baskets
jpeg(
  filename="C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Volumetric/countByBasket.jpeg",
  width=650,
  height=650,
  units="px",
  quality=100
)
x=boxplot(
  data$Count~data$Baskets,
  xlab="Baskets",
  ylab="Windowpane Flounder Count",
  ylim=c(0,max(data$Count+50))
)
text( 
  x=c(1:length(unique(data$Baskets))), 
  y=x$stats[nrow(x$stats),] + 25, 
  paste("n = ",table(data$Baskets),sep="")  
)
dev.off()
## Calculate an average count per basket
data$CPB=data$Count/data$Baskets
## Plot the count of fish per basket as a function of the number of baskets
jpeg(
  filename="C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Volumetric/countPerBasket.jpeg",
  width=650,
  height=650,
  units="px",
  quality=100
)
x=boxplot(
  data$CPB~data$Baskets,
  xlab="Baskets",
  ylab="Windowpane Flounder per Basket",
  ylim=c(0,max(data$CPB+50)),
  main=paste(
    "Avg. Fish per Basket = ",
    round(
      mean(data$CPB),
      0
    ),
    sep=""
  )
)
text( 
  x=c(1:length(unique(data$Baskets))), 
  y=x$stats[nrow(x$stats),] + 25, 
  paste("n = ",table(data$Baskets),sep="")  
)
## Add a line for the average across all baskets
abline(
  h=mean(data$CPB),
  lty=1,
  col='red'
)
dev.off()
## Descriptive statistics of average fish per basket
jpeg(
  filename="C:/Users/George/Desktop/Autotask Workplace/Electronic Monitoring/Georges Analyses/Volumetric/countPerBasket2.jpeg",
  width=650,
  height=650,
  units="px",
  quality=100
)
hist(
  data$CPB,
  breaks=seq(0,round(max(data$CPB),-1),5),
  col='gray',
  main='',
  xlab='Count per Basket',
  ylab='Instances'
)
abline(
  v=quantile(
    data$CPB,
    c(0.025,0.5,0.975)
    ),
  col='red',
  lwd=c(1,2,1),
  lty=c(2,1,2)
  )
dev.off()
quantile(
  data$CPB,
  c(0.025,0.5,0.975)
  )
mean(data$CPB)
sd(data$CPB)
