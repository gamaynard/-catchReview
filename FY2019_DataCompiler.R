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
library(XLConnect)
library(lubridate)
library(stringdist)
library(rgdal)
library(zip)
library(sp)
library(marmap)
library(devtools)
## ---------------------------

## load up our functions into memory
## The marmap library is partly deprecated because of NOAA's website updates
## Users in the GitHub community have developed a new function, and the code
## below downloads the most recent version from Eric Plante's GitHub repo
source_url(
  url="https://raw.githubusercontent.com/ericpante/marmap/master/R/getNOAA.bathy.R"
)
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
    if(
      grepl(
        pattern=".xlsx",
        x=filename
      )==TRUE
    ){
      partial=XLConnect::readWorksheetFromFile(
        file=filename,
        sheet=1
      )
    } else {
      if(
        grepl(
          pattern=".csv",
          x=filename
        )==TRUE
      ){
        partial=read.csv(
          file=filename
        )
      }
    }
  } else {
    partial=XLConnect::readWorksheetFromFile(
      file=filename,
      sheet="dealer"
    )
  }
  Dealer=rbind(Dealer,partial)
}
## The next step is to turn the .json EM file into a dataframe to enable merging
## it with other records
## Rename the EM .json to EM_JSON and create a new empty EM data frame
EM_JSON=EM
EM=data.frame(
  VTR=as.numeric(),
  VESSEL=as.character(),
  HAUL_NO=as.numeric(),
  startTime=as.numeric(),
  endTime=as.numeric(),
  startLat=as.numeric(),
  startLon=as.numeric(),
  species=as.character(),
  count=as.numeric(),
  weight=as.numeric()
)
## Each trip is an item in a list
## For each trip
for(i in 1:length(EM_JSON)){
  ## Open the list item
  trip=EM_JSON[[i]]
  ## Each trip is a list of variables and dataframes
  ## Extract the VTR number
  vtr=trip$trip_id
  ## Extract the vessel name
  vessel=trip$vessel_name
  ## Extract the number of hauls
  hauls=trip$total_hauls
  ## Report discards haul by haul
  for(h in 1:hauls){
    haul=trip$hauls[[h]]
    startTime=haul[2]
    endTime=haul[3]
    startLat=haul[4]
    startLon=haul[5]
    ## Discards are listed by species
    discards=haul$discards
    for(d in 1:length(discards)){
      species=discards[[d]]$species
      count=discards[[d]]$count_discarded
      weight=discards[[d]]$pounds_discarded
      newline=data.frame(
        VTR=as.numeric(),
        VESSEL=as.character(),
        HAUL_NO=as.numeric(),
        startTime=as.numeric(),
        endTime=as.numeric(),
        startLat=as.numeric(),
        startLon=as.numeric(),
        species=as.character(),
        count=as.numeric(),
        weight=as.numeric()
      )
      newline[1,]=NA
      newline$VESSEL=as.character(newline$VESSEL)
      newline$species=as.character(newline$species)
      newline$VTR=vtr
      newline$VESSEL=toupper(
        as.character(vessel)
      )
      newline$HAUL_NO=h
      newline$startTime=startTime
      newline$endTime=endTime
      newline$startLat=startLat
      newline$startLon=startLon
      newline$species=toupper(
        as.character(species)
      )
      newline$count=count
      newline$weight=weight
      EM=rbind(EM,newline)
      rm(newline)
    }
  }
}
## Create interoperable data sets that have all the information of interest
## Read in the species standardization list
species=read.csv(
  "https://raw.githubusercontent.com/gamaynard/ElectronicMonitoring/master/species.csv"
)
## ---------------------------
## VTR data
iVTR=VTR[,c(
  "DATE_SAIL",
  "DATE_LAND",
  "VESSEL_PERMIT_NUM",
  "SERIAL_NUM",
  "GEARCODE",
  "GEARQTY",
  "GEARSIZE",
  "AREA",
  "LAT_DEGREE",
  "LAT_MINUTE",
  "LAT_SECOND",
  "LON_DEGREE",
  "LON_MINUTE",
  "LON_SECOND",
  "NTOWS",
  "DATETIME_HAUL_START",
  "DATETIME_HAUL_END",
  "SPECIES_ID",
  "KEPT",
  "DISCARDED",
  "PORT_LANDED"
)]
colnames(iVTR)=tolower(
  colnames(iVTR)
)
## SAILDATE should be a POSIX value
iVTR$SAILDATE=ymd_hms(
  as.character(iVTR$date_sail)
  )
## LANDDATE should be a POSIX value
iVTR$LANDDATE=ymd_hms(
  as.character(iVTR$date_land)
)
## PERMIT should be a character string
iVTR$PERMIT=as.character(
  iVTR$vessel_permit_num
)
## Combine degrees, minutes, and seconds into decimal degrees for both latitude
## and longitude
iVTR$LAT=iVTR$lat_degree+iVTR$lat_minute/60+iVTR$lat_second/(60^2)
iVTR$LON=iVTR$lon_degree-iVTR$lon_minute/60-iVTR$lon_second/(60^2)
## Replace Gear Codes with human-readable values
iVTR$GEAR=ifelse(iVTR$gearcode=="GNS","GILLNET",
  ifelse(iVTR$gearcode=="HND","JIG",
    ifelse(iVTR$gearcode=="LLB","LONGLINE",
      ifelse(iVTR$gearcode=="OTF","TRAWL",
        ifelse(iVTR$gearcode=="PTL","LOBSTER POT",
          iVTR$gearcode
          )
        )
      )
    )
  )
## Ensure stat areas are reported as numbers
iVTR$AREA=as.numeric(
  as.character(
    iVTR$area
  )
)
## Trim serial numbers to generate VTR numbers
iVTR$VTR=NA
iVTR$VTR=ifelse(
  nchar(iVTR$serial_num)==16,
  substr(
    iVTR$serial_num,1,14
    ),
  iVTR$VTR
)
## Standardize species names
iVTR$SPECIES=NA
for(i in 1:nrow(iVTR)){
  iVTR$SPECIES[i]=as.character(
    species$AFS[
      which(
        stringsim(
          a=as.character(iVTR$species_id[i]),
          b=as.character(species$PEBKAC)
        )==max(  
          stringsim(
            a=as.character(iVTR$species_id[i]),
            b=as.character(species$PEBKAC)
          )
        )
      )[1]
      ]
  )
}
## Haul start and end times should be POSIX formatted values
iVTR$HAULSTART=ymd_hms(
  as.character(
    iVTR$datetime_haul_start
    )
  )
iVTR$HAULEND=ymd_hms(
  as.character(
    iVTR$datetime_haul_end
  )
)
## Kept and discarded weights should be numeric
iVTR$KEPT=as.numeric(
  as.character(
    iVTR$kept
    )
  )
iVTR$DISCARDED=as.numeric(
  as.character(
    iVTR$discarded
  )
)
## ---------------------------
## EM data
## because the EM data frame is already a modification of the original data, the
## script works on it directly
## The VTR column is already a character vector (to avoid loss of leading zeros)
## The VESSEL column is already a character vector
## The HAUL_NO column is already an integer
## The startTime column needs to be converted to a POSIX value
EM$STARTTIME=ymd_hm(
  as.character(
    EM$startTime
    )
  )
## The endTime column needs to be converted to a POSIX value
EM$ENDTIME=ymd_hm(
  as.character(
    EM$endTime
  )
)
## The startLat column needs to be converted to a number
EM$STARTLAT=as.numeric(
  as.character(
    EM$startLat
  )
)
## The startLon column needs to be converted to a number
EM$STARTLON=as.numeric(
  as.character(
    EM$startLon
  )
)
## Create a standardized species column
EM$SPECIES=NA
for(i in 1:nrow(EM)){
  EM$SPECIES[i]=as.character(
    species$AFS[
      which(
        stringsim(
          a=as.character(EM$species[i]),
          b=as.character(species$PEBKAC)
        )==max(  
          stringsim(
            a=as.character(EM$species[i]),
            b=as.character(species$PEBKAC)
          )
        )
      )[1]
    ]
  )
}
## Discard Count needs to be a number
EM$DiscardCount=as.numeric(
  as.character(
    EM$count
    )
  )
## DiscardWeight needs to be a number
EM$DiscardWeight=as.numeric(
  as.character(
    EM$weight
  )
)
## ---------------------------
## Dealer data
iDealer=Dealer[,c(
  "Mri",
  "Vessel.Permit.No",
  "Vessel.Name",
  "Vessel.Reg.No",
  "Vtr.Serial.No",
  "State.Land",
  "Port.Land",
  "Species.Itis",
  "Landed.Weight",
  "Live.Weight"
)]
## Permit numbers should be character strings to maintain leading zeros
iDealer$PERMIT=as.character(
  iDealer$Vessel.Permit.No
  )
## Vessel names should be all caps
iDealer$Vessel.Name=toupper(
  as.character(
    iDealer$Vessel.Name
  )
)
## VTR numbers should be character strings to maintain leading zeros
iDealer$VTR=as.character(
  iDealer$Vtr.Serial.No
  )
## Species names can be converted directly frim ITIS numbers
iDealer$SPECIES=NA
for(i in 1:nrow(iDealer)){
  itis=iDealer$Species.Itis[i]
  iDealer$SPECIES[i]=as.character(
    unique(
      species[
        which(
          species$ITIS==itis
        ),
        "AFS"
      ]
    )
  )
}
## Live weights should be reported as numbers
iDealer$WEIGHT=as.numeric(
  as.character(
    iDealer$Live.Weight
  )
)
## ---------------------------
## All data are standardized and ready for analysis
## ---------------------------
## Create a new data frame to store information about whether trips took place 
## inside closed areas or not
CA=data.frame(
  VTR=as.character(),
  LAT=as.numeric(),
  LON=as.numeric(),
  CAII=as.logical(),
  CL=as.logical(),
  WGOM=as.logical(),
  OUT=as.logical()
)
## Read in spatial data for all EM reviews
for(i in 1:nrow(EM)){
  vtr=EM$VTR[i]
  lat=EM$STARTLAT[i]
  lon=EM$STARTLON[i]
  new=data.frame(
    VTR=as.character(vtr),
    LAT=as.numeric(lat),
    LON=as.numeric(lon),
    CAII=NA,
    CL=NA,
    WGOM=NA,
    OUT=NA
  )
  CA=rbind(CA,new)
  rm(new)
}
## Read in spatial data for all VTRs
for(i in 1:nrow(iVTR)){
  vtr=iVTR$VTR[i]
  lat=iVTR$LAT[i]
  lon=iVTR$LON[i]
  new=data.frame(
    VTR=as.character(vtr),
    LAT=as.numeric(lat),
    LON=as.numeric(lon),
    CAI=NA,
    CAII=NA,
    CL=NA,
    WGOM=NA,
    OUT=NA
  )
  CA=rbind(CA,new)
  rm(new)
}
## Remove all duplicate entries from the spatial data frame
CA$dup=duplicated(CA)
CA=subset(
  CA,
  CA$dup==FALSE
)
CA$dup=NULL
## Remove all trips with a malfunctioning GPS
CA=subset(
  CA,
  CA$LAT>20 & abs(CA$LON)>20
)
## Download groundfish closures from the NOAA website as a .zip archive of 
## shapefiles into a temporary file
dest_file="AllCA.zip"
urlzip="https://s3.amazonaws.com/media.fisheries.noaa.gov/2020-09/Groundfish_Closure_Areas_20180409_0.zip?ON7sHgWHiJxpWm.B1IW5REVNRKhUvMrz"
download.file(
  url=urlzip,
  destfile=dest_file,
  mode="wb"
)
zip::unzip(
  zipfile=dest_file,
  exdir="AllCA"
)
## Read in the shapefile that contains all closed areas
AllCA=readOGR(
  dsn="AllCA/Groundfish_Closure_Areas/Groundfish_Closure_Areas.shp"
)
## Break up the shapefile into individual closed areas
## In FY2019, the closed area list includes the following:
## Cashes Ledge Closure
CL=AllCA[AllCA$AREANAME=="Cashes Ledge Closure Area",]
## Closed Area II
CA2=AllCA[AllCA$AREANAME=="Closed Area II Closure Area",]
## Western Gulf of Maine
WGOM=AllCA[AllCA$AREANAME=="Western Gulf of Maine Closure Area",]

## Remove the temporary files from the directory
unlink("AllCA.zip")
unlink(
  "AllCA",
  recursive=TRUE
  )
rm(AllCA)
for(i in 1:nrow(CA)){
  ## Separate the individual trip out
  trip=CA[i,]
  ## Assign the trip a spatial reference
  coordinates(trip)=~LON+LAT
  ## Reproject the trip to the same coordinate system as the closed area 
  ## shapefiles
  proj4string(trip)=proj4string(CL)
  ## Check to see if each trip overlaps with the boundaries of Cashes Ledge
  if(is.na(over(trip,CL)$AREANAME)==TRUE){
    CA$CL[i]=FALSE
  } else {
    CA$CL[i]=ifelse(
      as.character(over(trip,CL)$AREANAME)=="Cashes Ledge Closure Area",
      TRUE,
      FALSE
    )
  }
  ## Check to see if each trip overlaps with the boundaries of Closed Area II
  if(is.na(over(trip,CA2)$AREANAME)==TRUE){
    CA$CAII[i]=FALSE
  } else {
    CA$CAII[i]=ifelse(
      as.character(over(trip,CA2)$AREANAME)=="Closed Area II Closure Area",
      TRUE,
      FALSE
    )
  }
  ## Check to see if each trip overlaps with the boundaries of the WGOM
  if(is.na(over(trip,WGOM)$AREANAME)==TRUE){
    CA$WGOM[i]=FALSE
  } else {
    CA$WGOM[i]=ifelse(
      as.character(over(trip,WGOM)$AREANAME)=="Western Gulf of Maine Closure Area",
      TRUE,
      FALSE
    )
  }
  ## If the trips do not take place in a closed area, label them as OUT
  CA$OUT[i]=ifelse(
    CA$CAII[i]+CA$CL[i]+CA$WGOM[i]==0,
    TRUE,
    FALSE
  )
}
## Create a table of trips by closed area and vessel
CAV=data.frame(
  VESSEL=as.character(),
  CAII=as.numeric(),
  CL=as.numeric(),
  WGOM=as.numeric(),
  OUT=as.numeric()
)
## Link each VTR in the CA table with a vessel name
CA$VESSEL=NA
for(i in 1:nrow(CA)){
  CA$VESSEL[i]=as.character(
    unique(
      EM$VESSEL[which(
        EM$VTR==as.character(
          CA$VTR[i]
        )
      )
      ]
    )
  )
}
## Make a list of all unique vessels in the CA data frame
VESSEL=unique(CA$VESSEL)[order(
  unique(CA$VESSEL)
  )]
## For each vessel, total up how many trips it took inside and outside of the
## closed areas
for(i in 1:length(VESSEL)){
  v=VESSEL[i]
  x=subset(CA,CA$VESSEL==v)
  ca2=sum(x$CAII)
  cl=sum(x$CL)
  wg=sum(x$WGOM)
  o=sum(x$OUT)
  y=data.frame(
    VESSEL=v,
    CAII=ca2,
    CL=cl,
    WGOM=wg,
    OUT=o
  )
  CAV=rbind(CAV,y)
}
##########################################################################
## Table 1 is CAV
##########################################################################
## Create a vector of blues for plotting a map of trips
blues=c(
  "lightsteelblue4", 
  "lightsteelblue3", 
  "lightsteelblue2", 
  "lightsteelblue1"
  )
## Create a vector of grays for plotting a map of trips
grays=c(
  gray(0.6), 
  gray(0.93), 
  gray(0.99)
  )
## Download bathymetric data for plotting trip locations
basemap=getNOAA.bathy(
  lon1=-75, 
  lon2=-65, 
  lat1=40, 
  lat2=48, 
  resolution=5
  )
