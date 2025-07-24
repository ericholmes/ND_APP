
# Zooper data download ----------------------------------------------------

library(zooper)

MyZoops <- Zoopsynther(Data_type = "Taxa", 
                       Sources = c("EMP", "FRP", "FMWT", "STN", "20mm", "DOP"), 
                       Size_class = "Meso", 
                       Date_range = c("2014-10-01", "2025-09-30"))

# EDSM data download ------------------------------------------------------

# Package ID: edi.415.12 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program and US Fish and Wildlife Service: San Francisco Estuary Enhanced Delta Smelt Monitoring Program Data, 2016-2024.
# Data set creator:    - United States Fish and Wildlife Service 
# Data set creator:  Claudia Macfarlane - United States Fish and Wildlife Service 
# Data set creator:  Denise Goodman - United States Fish and Wildlife 
# Data set creator:  Kate Erly - United States Fish and Wildlife Service 
# Data set creator:  Eric Huber - United States Fish and Wildlife Service 
# Data set creator:  J. Ryan Cook - United States Fish and Wildlife Service 
# Data set creator:  Gershom Bigham - United States Fish and Wildlife Service 
# Data set creator:  Vivian Peck - United States Fish and Wildlife Service 
# Data set creator:  Savannah Valdez - United States Fish and Wildlife Service 
# Data set creator:  Eric Louwerens - United States Fish and Wildlife Service 
# Data set creator:  Jonathan Speegle - United States Fish and Wildlife Service 
# Data set creator:  Trevor Hope - United States Fish and Wildlife Service 
# Data set creator:  Gabriela Garcia - United States Fish and Wildlife Service 
# Data set creator:  Adriana Arrambide - United States Fish and Wildlife Service 
# Data set creator:  Paula Higginson - United States Fish and Wildlife Service 
# Data set creator:  Jessica Falcon - United States Fish and Wildlife Service 
# Contact:  Claudia Macfarlane -  United States Fish and Wildlife Service  - claudia_macfarlane@fws.gov
# Contact:  Denise Goodman -  United States Fish and Wildlife Service  - denise_goodman@fws.gov
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       

savedata = F

options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/415/12/d468c513fa69c4fc6ddc02e443785f28" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "RegionCode",     
                 "Subregion",     
                 "Stratum",     
                 "DepthStratum",     
                 "PairedDepth",     
                 "Phase",     
                 "StationCode",     
                 "PairedStudy",     
                 "SampleDate",     
                 "SampleTime",     
                 "Latitude",     
                 "Longitude",     
                 "LatitudeStart",     
                 "LongitudeStart",     
                 "LatitudeEnd",     
                 "LongitudeEnd",     
                 "MethodCode",     
                 "TowType",     
                 "NetSize",     
                 "SamplingDirection",     
                 "TowNumber",     
                 "TowSchedule",     
                 "TowDuration",     
                 "MaxCableLength",     
                 "GearConditionCode",     
                 "FlowDebris",     
                 "Volume",     
                 "FlaggedVolume",     
                 "Comments",     
                 "Tide",     
                 "WeatherCode",     
                 "BottomDepth",     
                 "Secchi",     
                 "WaterTempTop",     
                 "WaterTempBottom",     
                 "SpecificConductanceTop",     
                 "SpecificConductanceBottom",     
                 "TurbidityTopFNU",     
                 "TurbidityTopNTU",     
                 "TurbidityBottom",     
                 "DOTop",     
                 "DOBottom",     
                 "IDLocation",     
                 "IDStatus",     
                 "CommonName",     
                 "OrganismCode",     
                 "IEPFishCode",     
                 "MarkCode",     
                 "StageCode",     
                 "Expression",     
                 "Dead",     
                 "ForkLength",     
                 "RaceByLength",     
                 "TagCode",     
                 "RaceByTag",     
                 "SpecialStudy",     
                 "SpecialStudyID",     
                 "FishComments",     
                 "Count"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$RegionCode)!="factor") dt1$RegionCode<- as.factor(dt1$RegionCode)
if (class(dt1$Subregion)!="factor") dt1$Subregion<- as.factor(dt1$Subregion)
if (class(dt1$Stratum)!="factor") dt1$Stratum<- as.factor(dt1$Stratum)
if (class(dt1$DepthStratum)!="factor") dt1$DepthStratum<- as.factor(dt1$DepthStratum)
if (class(dt1$PairedDepth)!="factor") dt1$PairedDepth<- as.factor(dt1$PairedDepth)
if (class(dt1$Phase)!="factor") dt1$Phase<- as.factor(dt1$Phase)
if (class(dt1$StationCode)!="factor") dt1$StationCode<- as.factor(dt1$StationCode)
if (class(dt1$PairedStudy)!="factor") dt1$PairedStudy<- as.factor(dt1$PairedStudy)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$SampleDate != "",]) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$Latitude)=="factor") dt1$Latitude <-as.numeric(levels(dt1$Latitude))[as.integer(dt1$Latitude) ]               
if (class(dt1$Latitude)=="character") dt1$Latitude <-as.numeric(dt1$Latitude)
if (class(dt1$Longitude)=="factor") dt1$Longitude <-as.numeric(levels(dt1$Longitude))[as.integer(dt1$Longitude) ]               
if (class(dt1$Longitude)=="character") dt1$Longitude <-as.numeric(dt1$Longitude)
if (class(dt1$LatitudeStart)=="factor") dt1$LatitudeStart <-as.numeric(levels(dt1$LatitudeStart))[as.integer(dt1$LatitudeStart) ]               
if (class(dt1$LatitudeStart)=="character") dt1$LatitudeStart <-as.numeric(dt1$LatitudeStart)
if (class(dt1$LongitudeStart)=="factor") dt1$LongitudeStart <-as.numeric(levels(dt1$LongitudeStart))[as.integer(dt1$LongitudeStart) ]               
if (class(dt1$LongitudeStart)=="character") dt1$LongitudeStart <-as.numeric(dt1$LongitudeStart)
if (class(dt1$LatitudeEnd)=="factor") dt1$LatitudeEnd <-as.numeric(levels(dt1$LatitudeEnd))[as.integer(dt1$LatitudeEnd) ]               
if (class(dt1$LatitudeEnd)=="character") dt1$LatitudeEnd <-as.numeric(dt1$LatitudeEnd)
if (class(dt1$LongitudeEnd)=="factor") dt1$LongitudeEnd <-as.numeric(levels(dt1$LongitudeEnd))[as.integer(dt1$LongitudeEnd) ]               
if (class(dt1$LongitudeEnd)=="character") dt1$LongitudeEnd <-as.numeric(dt1$LongitudeEnd)
if (class(dt1$MethodCode)!="factor") dt1$MethodCode<- as.factor(dt1$MethodCode)
if (class(dt1$TowType)!="factor") dt1$TowType<- as.factor(dt1$TowType)
if (class(dt1$NetSize)!="factor") dt1$NetSize<- as.factor(dt1$NetSize)
if (class(dt1$SamplingDirection)!="factor") dt1$SamplingDirection<- as.factor(dt1$SamplingDirection)
if (class(dt1$TowNumber)=="factor") dt1$TowNumber <-as.numeric(levels(dt1$TowNumber))[as.integer(dt1$TowNumber) ]               
if (class(dt1$TowNumber)=="character") dt1$TowNumber <-as.numeric(dt1$TowNumber)
if (class(dt1$TowSchedule)=="factor") dt1$TowSchedule <-as.numeric(levels(dt1$TowSchedule))[as.integer(dt1$TowSchedule) ]               
if (class(dt1$TowSchedule)=="character") dt1$TowSchedule <-as.numeric(dt1$TowSchedule)
if (class(dt1$TowDuration)=="factor") dt1$TowDuration <-as.numeric(levels(dt1$TowDuration))[as.integer(dt1$TowDuration) ]               
if (class(dt1$TowDuration)=="character") dt1$TowDuration <-as.numeric(dt1$TowDuration)
if (class(dt1$MaxCableLength)=="factor") dt1$MaxCableLength <-as.numeric(levels(dt1$MaxCableLength))[as.integer(dt1$MaxCableLength) ]               
if (class(dt1$MaxCableLength)=="character") dt1$MaxCableLength <-as.numeric(dt1$MaxCableLength)
if (class(dt1$GearConditionCode)!="factor") dt1$GearConditionCode<- as.factor(dt1$GearConditionCode)
if (class(dt1$FlowDebris)!="factor") dt1$FlowDebris<- as.factor(dt1$FlowDebris)
if (class(dt1$Volume)=="factor") dt1$Volume <-as.numeric(levels(dt1$Volume))[as.integer(dt1$Volume) ]               
if (class(dt1$Volume)=="character") dt1$Volume <-as.numeric(dt1$Volume)
if (class(dt1$FlaggedVolume)!="factor") dt1$FlaggedVolume<- as.factor(dt1$FlaggedVolume)
if (class(dt1$Comments)!="factor") dt1$Comments<- as.factor(dt1$Comments)
if (class(dt1$Tide)!="factor") dt1$Tide<- as.factor(dt1$Tide)
if (class(dt1$WeatherCode)!="factor") dt1$WeatherCode<- as.factor(dt1$WeatherCode)
if (class(dt1$BottomDepth)=="factor") dt1$BottomDepth <-as.numeric(levels(dt1$BottomDepth))[as.integer(dt1$BottomDepth) ]               
if (class(dt1$BottomDepth)=="character") dt1$BottomDepth <-as.numeric(dt1$BottomDepth)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$WaterTempTop)=="factor") dt1$WaterTempTop <-as.numeric(levels(dt1$WaterTempTop))[as.integer(dt1$WaterTempTop) ]               
if (class(dt1$WaterTempTop)=="character") dt1$WaterTempTop <-as.numeric(dt1$WaterTempTop)
if (class(dt1$WaterTempBottom)=="factor") dt1$WaterTempBottom <-as.numeric(levels(dt1$WaterTempBottom))[as.integer(dt1$WaterTempBottom) ]               
if (class(dt1$WaterTempBottom)=="character") dt1$WaterTempBottom <-as.numeric(dt1$WaterTempBottom)
if (class(dt1$SpecificConductanceTop)=="factor") dt1$SpecificConductanceTop <-as.numeric(levels(dt1$SpecificConductanceTop))[as.integer(dt1$SpecificConductanceTop) ]               
if (class(dt1$SpecificConductanceTop)=="character") dt1$SpecificConductanceTop <-as.numeric(dt1$SpecificConductanceTop)
if (class(dt1$SpecificConductanceBottom)=="factor") dt1$SpecificConductanceBottom <-as.numeric(levels(dt1$SpecificConductanceBottom))[as.integer(dt1$SpecificConductanceBottom) ]               
if (class(dt1$SpecificConductanceBottom)=="character") dt1$SpecificConductanceBottom <-as.numeric(dt1$SpecificConductanceBottom)
if (class(dt1$TurbidityTopFNU)=="factor") dt1$TurbidityTopFNU <-as.numeric(levels(dt1$TurbidityTopFNU))[as.integer(dt1$TurbidityTopFNU) ]               
if (class(dt1$TurbidityTopFNU)=="character") dt1$TurbidityTopFNU <-as.numeric(dt1$TurbidityTopFNU)
if (class(dt1$TurbidityTopNTU)=="factor") dt1$TurbidityTopNTU <-as.numeric(levels(dt1$TurbidityTopNTU))[as.integer(dt1$TurbidityTopNTU) ]               
if (class(dt1$TurbidityTopNTU)=="character") dt1$TurbidityTopNTU <-as.numeric(dt1$TurbidityTopNTU)
if (class(dt1$TurbidityBottom)=="factor") dt1$TurbidityBottom <-as.numeric(levels(dt1$TurbidityBottom))[as.integer(dt1$TurbidityBottom) ]               
if (class(dt1$TurbidityBottom)=="character") dt1$TurbidityBottom <-as.numeric(dt1$TurbidityBottom)
if (class(dt1$DOTop)=="factor") dt1$DOTop <-as.numeric(levels(dt1$DOTop))[as.integer(dt1$DOTop) ]               
if (class(dt1$DOTop)=="character") dt1$DOTop <-as.numeric(dt1$DOTop)
if (class(dt1$DOBottom)=="factor") dt1$DOBottom <-as.numeric(levels(dt1$DOBottom))[as.integer(dt1$DOBottom) ]               
if (class(dt1$DOBottom)=="character") dt1$DOBottom <-as.numeric(dt1$DOBottom)
if (class(dt1$IDLocation)!="factor") dt1$IDLocation<- as.factor(dt1$IDLocation)
if (class(dt1$IDStatus)!="factor") dt1$IDStatus<- as.factor(dt1$IDStatus)
if (class(dt1$CommonName)!="factor") dt1$CommonName<- as.factor(dt1$CommonName)
if (class(dt1$OrganismCode)!="factor") dt1$OrganismCode<- as.factor(dt1$OrganismCode)
if (class(dt1$IEPFishCode)!="factor") dt1$IEPFishCode<- as.factor(dt1$IEPFishCode)
if (class(dt1$MarkCode)!="factor") dt1$MarkCode<- as.factor(dt1$MarkCode)
if (class(dt1$StageCode)!="factor") dt1$StageCode<- as.factor(dt1$StageCode)
if (class(dt1$Expression)!="factor") dt1$Expression<- as.factor(dt1$Expression)
if (class(dt1$Dead)!="factor") dt1$Dead<- as.factor(dt1$Dead)
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$RaceByLength)!="factor") dt1$RaceByLength<- as.factor(dt1$RaceByLength)
if (class(dt1$TagCode)!="factor") dt1$TagCode<- as.factor(dt1$TagCode)
if (class(dt1$RaceByTag)!="factor") dt1$RaceByTag<- as.factor(dt1$RaceByTag)
if (class(dt1$SpecialStudy)!="factor") dt1$SpecialStudy<- as.factor(dt1$SpecialStudy)
if (class(dt1$SpecialStudyID)!="factor") dt1$SpecialStudyID<- as.factor(dt1$SpecialStudyID)
if (class(dt1$FishComments)!="factor") dt1$FishComments<- as.factor(dt1$FishComments)
if (class(dt1$Count)=="factor") dt1$Count <-as.numeric(levels(dt1$Count))[as.integer(dt1$Count) ]               
if (class(dt1$Count)=="character") dt1$Count <-as.numeric(dt1$Count)

# Convert Missing Values to NA for non-dates

dt1$RegionCode <- as.factor(ifelse((trimws(as.character(dt1$RegionCode))==trimws("n/p")),NA,as.character(dt1$RegionCode)))
dt1$Subregion <- as.factor(ifelse((trimws(as.character(dt1$Subregion))==trimws("n/p")),NA,as.character(dt1$Subregion)))
dt1$Stratum <- as.factor(ifelse((trimws(as.character(dt1$Stratum))==trimws("n/p")),NA,as.character(dt1$Stratum)))
dt1$DepthStratum <- as.factor(ifelse((trimws(as.character(dt1$DepthStratum))==trimws("NA")),NA,as.character(dt1$DepthStratum)))
dt1$PairedDepth <- as.factor(ifelse((trimws(as.character(dt1$PairedDepth))==trimws("NA")),NA,as.character(dt1$PairedDepth)))
dt1$Phase <- as.factor(ifelse((trimws(as.character(dt1$Phase))==trimws("NA")),NA,as.character(dt1$Phase)))
dt1$PairedStudy <- as.factor(ifelse((trimws(as.character(dt1$PairedStudy))==trimws("NA")),NA,as.character(dt1$PairedStudy)))
dt1$Latitude <- ifelse((trimws(as.character(dt1$Latitude))==trimws("NA")),NA,dt1$Latitude)               
suppressWarnings(dt1$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Latitude))==as.character(as.numeric("NA"))),NA,dt1$Latitude))
dt1$LatitudeStart <- ifelse((trimws(as.character(dt1$LatitudeStart))==trimws("NA")),NA,dt1$LatitudeStart)               
suppressWarnings(dt1$LatitudeStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$LatitudeStart))==as.character(as.numeric("NA"))),NA,dt1$LatitudeStart))
dt1$LongitudeStart <- ifelse((trimws(as.character(dt1$LongitudeStart))==trimws("NA")),NA,dt1$LongitudeStart)               
suppressWarnings(dt1$LongitudeStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$LongitudeStart))==as.character(as.numeric("NA"))),NA,dt1$LongitudeStart))
dt1$LatitudeEnd <- ifelse((trimws(as.character(dt1$LatitudeEnd))==trimws("NA")),NA,dt1$LatitudeEnd)               
suppressWarnings(dt1$LatitudeEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$LatitudeEnd))==as.character(as.numeric("NA"))),NA,dt1$LatitudeEnd))
dt1$LongitudeEnd <- ifelse((trimws(as.character(dt1$LongitudeEnd))==trimws("NA")),NA,dt1$LongitudeEnd)               
suppressWarnings(dt1$LongitudeEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$LongitudeEnd))==as.character(as.numeric("NA"))),NA,dt1$LongitudeEnd))
dt1$TowType <- as.factor(ifelse((trimws(as.character(dt1$TowType))==trimws("NA")),NA,as.character(dt1$TowType)))
dt1$NetSize <- as.factor(ifelse((trimws(as.character(dt1$NetSize))==trimws("NA")),NA,as.character(dt1$NetSize)))
dt1$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt1$SamplingDirection))==trimws("NA")),NA,as.character(dt1$SamplingDirection)))
dt1$TowNumber <- ifelse((trimws(as.character(dt1$TowNumber))==trimws("NA")),NA,dt1$TowNumber)               
suppressWarnings(dt1$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowNumber))==as.character(as.numeric("NA"))),NA,dt1$TowNumber))
dt1$TowSchedule <- ifelse((trimws(as.character(dt1$TowSchedule))==trimws("NA")),NA,dt1$TowSchedule)               
suppressWarnings(dt1$TowSchedule <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowSchedule))==as.character(as.numeric("NA"))),NA,dt1$TowSchedule))
dt1$TowDuration <- ifelse((trimws(as.character(dt1$TowDuration))==trimws("NA")),NA,dt1$TowDuration)               
suppressWarnings(dt1$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowDuration))==as.character(as.numeric("NA"))),NA,dt1$TowDuration))
dt1$MaxCableLength <- ifelse((trimws(as.character(dt1$MaxCableLength))==trimws("NA")),NA,dt1$MaxCableLength)               
suppressWarnings(dt1$MaxCableLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$MaxCableLength))==as.character(as.numeric("NA"))),NA,dt1$MaxCableLength))
dt1$FlowDebris <- as.factor(ifelse((trimws(as.character(dt1$FlowDebris))==trimws("NA")),NA,as.character(dt1$FlowDebris)))
dt1$Volume <- ifelse((trimws(as.character(dt1$Volume))==trimws("NA")),NA,dt1$Volume)               
suppressWarnings(dt1$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Volume))==as.character(as.numeric("NA"))),NA,dt1$Volume))
dt1$Comments <- as.factor(ifelse((trimws(as.character(dt1$Comments))==trimws("NA")),NA,as.character(dt1$Comments)))
dt1$Tide <- as.factor(ifelse((trimws(as.character(dt1$Tide))==trimws("NA")),NA,as.character(dt1$Tide)))
dt1$WeatherCode <- as.factor(ifelse((trimws(as.character(dt1$WeatherCode))==trimws("NA")),NA,as.character(dt1$WeatherCode)))
dt1$BottomDepth <- ifelse((trimws(as.character(dt1$BottomDepth))==trimws("NA")),NA,dt1$BottomDepth)               
suppressWarnings(dt1$BottomDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BottomDepth))==as.character(as.numeric("NA"))),NA,dt1$BottomDepth))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$WaterTempTop <- ifelse((trimws(as.character(dt1$WaterTempTop))==trimws("NA")),NA,dt1$WaterTempTop)               
suppressWarnings(dt1$WaterTempTop <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterTempTop))==as.character(as.numeric("NA"))),NA,dt1$WaterTempTop))
dt1$WaterTempBottom <- ifelse((trimws(as.character(dt1$WaterTempBottom))==trimws("NA")),NA,dt1$WaterTempBottom)               
suppressWarnings(dt1$WaterTempBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterTempBottom))==as.character(as.numeric("NA"))),NA,dt1$WaterTempBottom))
dt1$SpecificConductanceTop <- ifelse((trimws(as.character(dt1$SpecificConductanceTop))==trimws("NA")),NA,dt1$SpecificConductanceTop)               
suppressWarnings(dt1$SpecificConductanceTop <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpecificConductanceTop))==as.character(as.numeric("NA"))),NA,dt1$SpecificConductanceTop))
dt1$SpecificConductanceBottom <- ifelse((trimws(as.character(dt1$SpecificConductanceBottom))==trimws("NA")),NA,dt1$SpecificConductanceBottom)               
suppressWarnings(dt1$SpecificConductanceBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpecificConductanceBottom))==as.character(as.numeric("NA"))),NA,dt1$SpecificConductanceBottom))
dt1$TurbidityTopFNU <- ifelse((trimws(as.character(dt1$TurbidityTopFNU))==trimws("NA")),NA,dt1$TurbidityTopFNU)               
suppressWarnings(dt1$TurbidityTopFNU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityTopFNU))==as.character(as.numeric("NA"))),NA,dt1$TurbidityTopFNU))
dt1$TurbidityTopNTU <- ifelse((trimws(as.character(dt1$TurbidityTopNTU))==trimws("NA")),NA,dt1$TurbidityTopNTU)               
suppressWarnings(dt1$TurbidityTopNTU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityTopNTU))==as.character(as.numeric("NA"))),NA,dt1$TurbidityTopNTU))
dt1$TurbidityBottom <- ifelse((trimws(as.character(dt1$TurbidityBottom))==trimws("NA")),NA,dt1$TurbidityBottom)               
suppressWarnings(dt1$TurbidityBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityBottom))==as.character(as.numeric("NA"))),NA,dt1$TurbidityBottom))
dt1$DOTop <- ifelse((trimws(as.character(dt1$DOTop))==trimws("NA")),NA,dt1$DOTop)               
suppressWarnings(dt1$DOTop <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOTop))==as.character(as.numeric("NA"))),NA,dt1$DOTop))
dt1$DOBottom <- ifelse((trimws(as.character(dt1$DOBottom))==trimws("NA")),NA,dt1$DOBottom)               
suppressWarnings(dt1$DOBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOBottom))==as.character(as.numeric("NA"))),NA,dt1$DOBottom))
dt1$IDLocation <- as.factor(ifelse((trimws(as.character(dt1$IDLocation))==trimws("NA")),NA,as.character(dt1$IDLocation)))
dt1$IDStatus <- as.factor(ifelse((trimws(as.character(dt1$IDStatus))==trimws("NA")),NA,as.character(dt1$IDStatus)))
dt1$CommonName <- as.factor(ifelse((trimws(as.character(dt1$CommonName))==trimws("NA")),NA,as.character(dt1$CommonName)))
dt1$OrganismCode <- as.factor(ifelse((trimws(as.character(dt1$OrganismCode))==trimws("NA")),NA,as.character(dt1$OrganismCode)))
dt1$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt1$IEPFishCode))==trimws("NA")),NA,as.character(dt1$IEPFishCode)))
dt1$MarkCode <- as.factor(ifelse((trimws(as.character(dt1$MarkCode))==trimws("NA")),NA,as.character(dt1$MarkCode)))
dt1$StageCode <- as.factor(ifelse((trimws(as.character(dt1$StageCode))==trimws("NA")),NA,as.character(dt1$StageCode)))
dt1$Expression <- as.factor(ifelse((trimws(as.character(dt1$Expression))==trimws("NA")),NA,as.character(dt1$Expression)))
dt1$Dead <- as.factor(ifelse((trimws(as.character(dt1$Dead))==trimws("NA")),NA,as.character(dt1$Dead)))
dt1$ForkLength <- ifelse((trimws(as.character(dt1$ForkLength))==trimws("NA")),NA,dt1$ForkLength)               
suppressWarnings(dt1$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ForkLength))==as.character(as.numeric("NA"))),NA,dt1$ForkLength))
dt1$RaceByLength <- as.factor(ifelse((trimws(as.character(dt1$RaceByLength))==trimws("NA")),NA,as.character(dt1$RaceByLength)))
dt1$TagCode <- as.factor(ifelse((trimws(as.character(dt1$TagCode))==trimws("NA")),NA,as.character(dt1$TagCode)))
dt1$RaceByTag <- as.factor(ifelse((trimws(as.character(dt1$RaceByTag))==trimws("NA")),NA,as.character(dt1$RaceByTag)))
dt1$SpecialStudy <- as.factor(ifelse((trimws(as.character(dt1$SpecialStudy))==trimws("NA")),NA,as.character(dt1$SpecialStudy)))
dt1$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt1$SpecialStudyID))==trimws("NA")),NA,as.character(dt1$SpecialStudyID)))
dt1$FishComments <- as.factor(ifelse((trimws(as.character(dt1$FishComments))==trimws("NA")),NA,as.character(dt1$FishComments)))
dt1$Count <- ifelse((trimws(as.character(dt1$Count))==trimws("NA")),NA,dt1$Count)               
suppressWarnings(dt1$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Count))==as.character(as.numeric("NA"))),NA,dt1$Count))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(RegionCode)
summary(Subregion)
summary(Stratum)
summary(DepthStratum)
summary(PairedDepth)
summary(Phase)
summary(StationCode)
summary(PairedStudy)
summary(SampleDate)
summary(SampleTime)
summary(Latitude)
summary(Longitude)
summary(LatitudeStart)
summary(LongitudeStart)
summary(LatitudeEnd)
summary(LongitudeEnd)
summary(MethodCode)
summary(TowType)
summary(NetSize)
summary(SamplingDirection)
summary(TowNumber)
summary(TowSchedule)
summary(TowDuration)
summary(MaxCableLength)
summary(GearConditionCode)
summary(FlowDebris)
summary(Volume)
summary(FlaggedVolume)
summary(Comments)
summary(Tide)
summary(WeatherCode)
summary(BottomDepth)
summary(Secchi)
summary(WaterTempTop)
summary(WaterTempBottom)
summary(SpecificConductanceTop)
summary(SpecificConductanceBottom)
summary(TurbidityTopFNU)
summary(TurbidityTopNTU)
summary(TurbidityBottom)
summary(DOTop)
summary(DOBottom)
summary(IDLocation)
summary(IDStatus)
summary(CommonName)
summary(OrganismCode)
summary(IEPFishCode)
summary(MarkCode)
summary(StageCode)
summary(Expression)
summary(Dead)
summary(ForkLength)
summary(RaceByLength)
summary(TagCode)
summary(RaceByTag)
summary(SpecialStudy)
summary(SpecialStudyID)
summary(FishComments)
summary(Count) 
# Get more details on character variables

summary(as.factor(dt1$RegionCode)) 
summary(as.factor(dt1$Subregion)) 
summary(as.factor(dt1$Stratum)) 
summary(as.factor(dt1$DepthStratum)) 
summary(as.factor(dt1$PairedDepth)) 
summary(as.factor(dt1$Phase)) 
summary(as.factor(dt1$StationCode)) 
summary(as.factor(dt1$PairedStudy)) 
summary(as.factor(dt1$MethodCode)) 
summary(as.factor(dt1$TowType)) 
summary(as.factor(dt1$NetSize)) 
summary(as.factor(dt1$SamplingDirection)) 
summary(as.factor(dt1$GearConditionCode)) 
summary(as.factor(dt1$FlowDebris)) 
summary(as.factor(dt1$FlaggedVolume)) 
summary(as.factor(dt1$Comments)) 
summary(as.factor(dt1$Tide)) 
summary(as.factor(dt1$WeatherCode)) 
summary(as.factor(dt1$IDLocation)) 
summary(as.factor(dt1$IDStatus)) 
summary(as.factor(dt1$CommonName)) 
summary(as.factor(dt1$OrganismCode)) 
summary(as.factor(dt1$IEPFishCode)) 
summary(as.factor(dt1$MarkCode)) 
summary(as.factor(dt1$StageCode)) 
summary(as.factor(dt1$Expression)) 
summary(as.factor(dt1$Dead)) 
summary(as.factor(dt1$RaceByLength)) 
summary(as.factor(dt1$TagCode)) 
summary(as.factor(dt1$RaceByTag)) 
summary(as.factor(dt1$SpecialStudy)) 
summary(as.factor(dt1$SpecialStudyID)) 
summary(as.factor(dt1$FishComments))
detach(dt1)               



inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/415/12/4d7de6f0a38eff744a009a92083d37ae" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "RegionCode",     
                 "Subregion",     
                 "Stratum",     
                 "Phase",     
                 "StationCode",     
                 "PairedStudy",     
                 "SampleDate",     
                 "SampleTime",     
                 "Latitude",     
                 "Longitude",     
                 "LatitudeStart",     
                 "LongitudeStart",     
                 "LatitudeEnd",     
                 "LongitudeEnd",     
                 "MethodCode",     
                 "SamplingDirection",     
                 "TowNumber",     
                 "TowMax",     
                 "TowDuration",     
                 "GearConditionCode",     
                 "FlowDebris",     
                 "Volume",     
                 "FlaggedVolume",     
                 "Comments",     
                 "Tide",     
                 "WeatherCode",     
                 "BottomDepth",     
                 "Secchi",     
                 "WaterTemp",     
                 "SpecificConductance",     
                 "TurbidityNTU",     
                 "TurbidityFNU",     
                 "DO",     
                 "CommonName",     
                 "OrganismCode",     
                 "IEPFishCode",     
                 "MarkCode",     
                 "StageCode",     
                 "Expression",     
                 "Dead",     
                 "ForkLength",     
                 "RaceByLength",     
                 "TagCode",     
                 "RaceByTag",     
                 "SpecialStudy",     
                 "SpecialStudyID",     
                 "Count"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$RegionCode)!="factor") dt2$RegionCode<- as.factor(dt2$RegionCode)
if (class(dt2$Subregion)!="factor") dt2$Subregion<- as.factor(dt2$Subregion)
if (class(dt2$Stratum)!="factor") dt2$Stratum<- as.factor(dt2$Stratum)
if (class(dt2$Phase)!="factor") dt2$Phase<- as.factor(dt2$Phase)
if (class(dt2$StationCode)!="factor") dt2$StationCode<- as.factor(dt2$StationCode)
if (class(dt2$PairedStudy)!="factor") dt2$PairedStudy<- as.factor(dt2$PairedStudy)                                   
# attempting to convert dt2$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2SampleDate<-as.Date(dt2$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$SampleDate != "",]) == length(tmp2SampleDate[!is.na(tmp2SampleDate)])){dt2$SampleDate <- tmp2SampleDate } else {print("Date conversion failed for dt2$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt2$Latitude)=="factor") dt2$Latitude <-as.numeric(levels(dt2$Latitude))[as.integer(dt2$Latitude) ]               
if (class(dt2$Latitude)=="character") dt2$Latitude <-as.numeric(dt2$Latitude)
if (class(dt2$Longitude)=="factor") dt2$Longitude <-as.numeric(levels(dt2$Longitude))[as.integer(dt2$Longitude) ]               
if (class(dt2$Longitude)=="character") dt2$Longitude <-as.numeric(dt2$Longitude)
if (class(dt2$LatitudeStart)=="factor") dt2$LatitudeStart <-as.numeric(levels(dt2$LatitudeStart))[as.integer(dt2$LatitudeStart) ]               
if (class(dt2$LatitudeStart)=="character") dt2$LatitudeStart <-as.numeric(dt2$LatitudeStart)
if (class(dt2$LongitudeStart)=="factor") dt2$LongitudeStart <-as.numeric(levels(dt2$LongitudeStart))[as.integer(dt2$LongitudeStart) ]               
if (class(dt2$LongitudeStart)=="character") dt2$LongitudeStart <-as.numeric(dt2$LongitudeStart)
if (class(dt2$LatitudeEnd)=="factor") dt2$LatitudeEnd <-as.numeric(levels(dt2$LatitudeEnd))[as.integer(dt2$LatitudeEnd) ]               
if (class(dt2$LatitudeEnd)=="character") dt2$LatitudeEnd <-as.numeric(dt2$LatitudeEnd)
if (class(dt2$LongitudeEnd)=="factor") dt2$LongitudeEnd <-as.numeric(levels(dt2$LongitudeEnd))[as.integer(dt2$LongitudeEnd) ]               
if (class(dt2$LongitudeEnd)=="character") dt2$LongitudeEnd <-as.numeric(dt2$LongitudeEnd)
if (class(dt2$MethodCode)!="factor") dt2$MethodCode<- as.factor(dt2$MethodCode)
if (class(dt2$SamplingDirection)!="factor") dt2$SamplingDirection<- as.factor(dt2$SamplingDirection)
if (class(dt2$TowNumber)=="factor") dt2$TowNumber <-as.numeric(levels(dt2$TowNumber))[as.integer(dt2$TowNumber) ]               
if (class(dt2$TowNumber)=="character") dt2$TowNumber <-as.numeric(dt2$TowNumber)
if (class(dt2$TowMax)=="factor") dt2$TowMax <-as.numeric(levels(dt2$TowMax))[as.integer(dt2$TowMax) ]               
if (class(dt2$TowMax)=="character") dt2$TowMax <-as.numeric(dt2$TowMax)
if (class(dt2$TowDuration)=="factor") dt2$TowDuration <-as.numeric(levels(dt2$TowDuration))[as.integer(dt2$TowDuration) ]               
if (class(dt2$TowDuration)=="character") dt2$TowDuration <-as.numeric(dt2$TowDuration)
if (class(dt2$GearConditionCode)!="factor") dt2$GearConditionCode<- as.factor(dt2$GearConditionCode)
if (class(dt2$FlowDebris)!="factor") dt2$FlowDebris<- as.factor(dt2$FlowDebris)
if (class(dt2$Volume)=="factor") dt2$Volume <-as.numeric(levels(dt2$Volume))[as.integer(dt2$Volume) ]               
if (class(dt2$Volume)=="character") dt2$Volume <-as.numeric(dt2$Volume)
if (class(dt2$FlaggedVolume)!="factor") dt2$FlaggedVolume<- as.factor(dt2$FlaggedVolume)
if (class(dt2$Comments)!="factor") dt2$Comments<- as.factor(dt2$Comments)
if (class(dt2$Tide)!="factor") dt2$Tide<- as.factor(dt2$Tide)
if (class(dt2$WeatherCode)!="factor") dt2$WeatherCode<- as.factor(dt2$WeatherCode)
if (class(dt2$BottomDepth)=="factor") dt2$BottomDepth <-as.numeric(levels(dt2$BottomDepth))[as.integer(dt2$BottomDepth) ]               
if (class(dt2$BottomDepth)=="character") dt2$BottomDepth <-as.numeric(dt2$BottomDepth)
if (class(dt2$Secchi)=="factor") dt2$Secchi <-as.numeric(levels(dt2$Secchi))[as.integer(dt2$Secchi) ]               
if (class(dt2$Secchi)=="character") dt2$Secchi <-as.numeric(dt2$Secchi)
if (class(dt2$WaterTemp)=="factor") dt2$WaterTemp <-as.numeric(levels(dt2$WaterTemp))[as.integer(dt2$WaterTemp) ]               
if (class(dt2$WaterTemp)=="character") dt2$WaterTemp <-as.numeric(dt2$WaterTemp)
if (class(dt2$SpecificConductance)=="factor") dt2$SpecificConductance <-as.numeric(levels(dt2$SpecificConductance))[as.integer(dt2$SpecificConductance) ]               
if (class(dt2$SpecificConductance)=="character") dt2$SpecificConductance <-as.numeric(dt2$SpecificConductance)
if (class(dt2$TurbidityNTU)=="factor") dt2$TurbidityNTU <-as.numeric(levels(dt2$TurbidityNTU))[as.integer(dt2$TurbidityNTU) ]               
if (class(dt2$TurbidityNTU)=="character") dt2$TurbidityNTU <-as.numeric(dt2$TurbidityNTU)
if (class(dt2$TurbidityFNU)=="factor") dt2$TurbidityFNU <-as.numeric(levels(dt2$TurbidityFNU))[as.integer(dt2$TurbidityFNU) ]               
if (class(dt2$TurbidityFNU)=="character") dt2$TurbidityFNU <-as.numeric(dt2$TurbidityFNU)
if (class(dt2$DO)=="factor") dt2$DO <-as.numeric(levels(dt2$DO))[as.integer(dt2$DO) ]               
if (class(dt2$DO)=="character") dt2$DO <-as.numeric(dt2$DO)
if (class(dt2$CommonName)!="factor") dt2$CommonName<- as.factor(dt2$CommonName)
if (class(dt2$OrganismCode)!="factor") dt2$OrganismCode<- as.factor(dt2$OrganismCode)
if (class(dt2$IEPFishCode)!="factor") dt2$IEPFishCode<- as.factor(dt2$IEPFishCode)
if (class(dt2$MarkCode)!="factor") dt2$MarkCode<- as.factor(dt2$MarkCode)
if (class(dt2$StageCode)!="factor") dt2$StageCode<- as.factor(dt2$StageCode)
if (class(dt2$Expression)!="factor") dt2$Expression<- as.factor(dt2$Expression)
if (class(dt2$Dead)!="factor") dt2$Dead<- as.factor(dt2$Dead)
if (class(dt2$ForkLength)=="factor") dt2$ForkLength <-as.numeric(levels(dt2$ForkLength))[as.integer(dt2$ForkLength) ]               
if (class(dt2$ForkLength)=="character") dt2$ForkLength <-as.numeric(dt2$ForkLength)
if (class(dt2$RaceByLength)!="factor") dt2$RaceByLength<- as.factor(dt2$RaceByLength)
if (class(dt2$TagCode)!="factor") dt2$TagCode<- as.factor(dt2$TagCode)
if (class(dt2$RaceByTag)!="factor") dt2$RaceByTag<- as.factor(dt2$RaceByTag)
if (class(dt2$SpecialStudy)!="factor") dt2$SpecialStudy<- as.factor(dt2$SpecialStudy)
if (class(dt2$SpecialStudyID)!="factor") dt2$SpecialStudyID<- as.factor(dt2$SpecialStudyID)
if (class(dt2$Count)=="factor") dt2$Count <-as.numeric(levels(dt2$Count))[as.integer(dt2$Count) ]               
if (class(dt2$Count)=="character") dt2$Count <-as.numeric(dt2$Count)

# Convert Missing Values to NA for non-dates

dt2$RegionCode <- as.factor(ifelse((trimws(as.character(dt2$RegionCode))==trimws("n/p")),NA,as.character(dt2$RegionCode)))
dt2$Subregion <- as.factor(ifelse((trimws(as.character(dt2$Subregion))==trimws("n/p")),NA,as.character(dt2$Subregion)))
dt2$Stratum <- as.factor(ifelse((trimws(as.character(dt2$Stratum))==trimws("n/p")),NA,as.character(dt2$Stratum)))
dt2$Phase <- as.factor(ifelse((trimws(as.character(dt2$Phase))==trimws("NA")),NA,as.character(dt2$Phase)))
dt2$PairedStudy <- as.factor(ifelse((trimws(as.character(dt2$PairedStudy))==trimws("NA")),NA,as.character(dt2$PairedStudy)))
dt2$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt2$SamplingDirection))==trimws("NA")),NA,as.character(dt2$SamplingDirection)))
dt2$TowNumber <- ifelse((trimws(as.character(dt2$TowNumber))==trimws("NA")),NA,dt2$TowNumber)               
suppressWarnings(dt2$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowNumber))==as.character(as.numeric("NA"))),NA,dt2$TowNumber))
dt2$TowDuration <- ifelse((trimws(as.character(dt2$TowDuration))==trimws("NA")),NA,dt2$TowDuration)               
suppressWarnings(dt2$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowDuration))==as.character(as.numeric("NA"))),NA,dt2$TowDuration))
dt2$FlowDebris <- as.factor(ifelse((trimws(as.character(dt2$FlowDebris))==trimws("NA")),NA,as.character(dt2$FlowDebris)))
dt2$Volume <- ifelse((trimws(as.character(dt2$Volume))==trimws("NA")),NA,dt2$Volume)               
suppressWarnings(dt2$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Volume))==as.character(as.numeric("NA"))),NA,dt2$Volume))
dt2$Comments <- as.factor(ifelse((trimws(as.character(dt2$Comments))==trimws("NA")),NA,as.character(dt2$Comments)))
dt2$Tide <- as.factor(ifelse((trimws(as.character(dt2$Tide))==trimws("NA")),NA,as.character(dt2$Tide)))
dt2$WeatherCode <- as.factor(ifelse((trimws(as.character(dt2$WeatherCode))==trimws("NA")),NA,as.character(dt2$WeatherCode)))
dt2$BottomDepth <- ifelse((trimws(as.character(dt2$BottomDepth))==trimws("NA")),NA,dt2$BottomDepth)               
suppressWarnings(dt2$BottomDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$BottomDepth))==as.character(as.numeric("NA"))),NA,dt2$BottomDepth))
dt2$Secchi <- ifelse((trimws(as.character(dt2$Secchi))==trimws("NA")),NA,dt2$Secchi)               
suppressWarnings(dt2$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Secchi))==as.character(as.numeric("NA"))),NA,dt2$Secchi))
dt2$WaterTemp <- ifelse((trimws(as.character(dt2$WaterTemp))==trimws("NA")),NA,dt2$WaterTemp)               
suppressWarnings(dt2$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$WaterTemp))==as.character(as.numeric("NA"))),NA,dt2$WaterTemp))
dt2$SpecificConductance <- ifelse((trimws(as.character(dt2$SpecificConductance))==trimws("NA")),NA,dt2$SpecificConductance)               
suppressWarnings(dt2$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt2$SpecificConductance))
dt2$TurbidityNTU <- ifelse((trimws(as.character(dt2$TurbidityNTU))==trimws("NA")),NA,dt2$TurbidityNTU)               
suppressWarnings(dt2$TurbidityNTU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TurbidityNTU))==as.character(as.numeric("NA"))),NA,dt2$TurbidityNTU))
dt2$TurbidityFNU <- ifelse((trimws(as.character(dt2$TurbidityFNU))==trimws("NA")),NA,dt2$TurbidityFNU)               
suppressWarnings(dt2$TurbidityFNU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TurbidityFNU))==as.character(as.numeric("NA"))),NA,dt2$TurbidityFNU))
dt2$DO <- ifelse((trimws(as.character(dt2$DO))==trimws("NA")),NA,dt2$DO)               
suppressWarnings(dt2$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DO))==as.character(as.numeric("NA"))),NA,dt2$DO))
dt2$CommonName <- as.factor(ifelse((trimws(as.character(dt2$CommonName))==trimws("NA")),NA,as.character(dt2$CommonName)))
dt2$OrganismCode <- as.factor(ifelse((trimws(as.character(dt2$OrganismCode))==trimws("NA")),NA,as.character(dt2$OrganismCode)))
dt2$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt2$IEPFishCode))==trimws("NA")),NA,as.character(dt2$IEPFishCode)))
dt2$MarkCode <- as.factor(ifelse((trimws(as.character(dt2$MarkCode))==trimws("NA")),NA,as.character(dt2$MarkCode)))
dt2$StageCode <- as.factor(ifelse((trimws(as.character(dt2$StageCode))==trimws("NA")),NA,as.character(dt2$StageCode)))
dt2$Expression <- as.factor(ifelse((trimws(as.character(dt2$Expression))==trimws("NA")),NA,as.character(dt2$Expression)))
dt2$Dead <- as.factor(ifelse((trimws(as.character(dt2$Dead))==trimws("NA")),NA,as.character(dt2$Dead)))
dt2$ForkLength <- ifelse((trimws(as.character(dt2$ForkLength))==trimws("NA")),NA,dt2$ForkLength)               
suppressWarnings(dt2$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ForkLength))==as.character(as.numeric("NA"))),NA,dt2$ForkLength))
dt2$RaceByLength <- as.factor(ifelse((trimws(as.character(dt2$RaceByLength))==trimws("NA")),NA,as.character(dt2$RaceByLength)))
dt2$TagCode <- as.factor(ifelse((trimws(as.character(dt2$TagCode))==trimws("NA")),NA,as.character(dt2$TagCode)))
dt2$RaceByTag <- as.factor(ifelse((trimws(as.character(dt2$RaceByTag))==trimws("NA")),NA,as.character(dt2$RaceByTag)))
dt2$SpecialStudy <- as.factor(ifelse((trimws(as.character(dt2$SpecialStudy))==trimws("NA")),NA,as.character(dt2$SpecialStudy)))
dt2$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt2$SpecialStudyID))==trimws("NA")),NA,as.character(dt2$SpecialStudyID)))
dt2$Count <- ifelse((trimws(as.character(dt2$Count))==trimws("NA")),NA,dt2$Count)               
suppressWarnings(dt2$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Count))==as.character(as.numeric("NA"))),NA,dt2$Count))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(RegionCode)
summary(Subregion)
summary(Stratum)
summary(Phase)
summary(StationCode)
summary(PairedStudy)
summary(SampleDate)
summary(SampleTime)
summary(Latitude)
summary(Longitude)
summary(LatitudeStart)
summary(LongitudeStart)
summary(LatitudeEnd)
summary(LongitudeEnd)
summary(MethodCode)
summary(SamplingDirection)
summary(TowNumber)
summary(TowMax)
summary(TowDuration)
summary(GearConditionCode)
summary(FlowDebris)
summary(Volume)
summary(FlaggedVolume)
summary(Comments)
summary(Tide)
summary(WeatherCode)
summary(BottomDepth)
summary(Secchi)
summary(WaterTemp)
summary(SpecificConductance)
summary(TurbidityNTU)
summary(TurbidityFNU)
summary(DO)
summary(CommonName)
summary(OrganismCode)
summary(IEPFishCode)
summary(MarkCode)
summary(StageCode)
summary(Expression)
summary(Dead)
summary(ForkLength)
summary(RaceByLength)
summary(TagCode)
summary(RaceByTag)
summary(SpecialStudy)
summary(SpecialStudyID)
summary(Count) 
# Get more details on character variables

summary(as.factor(dt2$RegionCode)) 
summary(as.factor(dt2$Subregion)) 
summary(as.factor(dt2$Stratum)) 
summary(as.factor(dt2$Phase)) 
summary(as.factor(dt2$StationCode)) 
summary(as.factor(dt2$PairedStudy)) 
summary(as.factor(dt2$MethodCode)) 
summary(as.factor(dt2$SamplingDirection)) 
summary(as.factor(dt2$GearConditionCode)) 
summary(as.factor(dt2$FlowDebris)) 
summary(as.factor(dt2$FlaggedVolume)) 
summary(as.factor(dt2$Comments)) 
summary(as.factor(dt2$Tide)) 
summary(as.factor(dt2$WeatherCode)) 
summary(as.factor(dt2$CommonName)) 
summary(as.factor(dt2$OrganismCode)) 
summary(as.factor(dt2$IEPFishCode)) 
summary(as.factor(dt2$MarkCode)) 
summary(as.factor(dt2$StageCode)) 
summary(as.factor(dt2$Expression)) 
summary(as.factor(dt2$Dead)) 
summary(as.factor(dt2$RaceByLength)) 
summary(as.factor(dt2$TagCode)) 
summary(as.factor(dt2$RaceByTag)) 
summary(as.factor(dt2$SpecialStudy)) 
summary(as.factor(dt2$SpecialStudyID))
detach(dt2)               



inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/415/12/613ff2b59d13cd72428cffa1a12fa3bd" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "OrganismCode",     
                 "IEPFishCode",     
                 "CommonName",     
                 "NonNative",     
                 "Phylum",     
                 "Class",     
                 "Order",     
                 "Family",     
                 "Genus",     
                 "Species",     
                 "Active",     
                 "LengthMeasurementType"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$OrganismCode)!="factor") dt3$OrganismCode<- as.factor(dt3$OrganismCode)
if (class(dt3$IEPFishCode)!="factor") dt3$IEPFishCode<- as.factor(dt3$IEPFishCode)
if (class(dt3$CommonName)!="factor") dt3$CommonName<- as.factor(dt3$CommonName)
if (class(dt3$NonNative)!="factor") dt3$NonNative<- as.factor(dt3$NonNative)
if (class(dt3$Phylum)!="factor") dt3$Phylum<- as.factor(dt3$Phylum)
if (class(dt3$Class)!="factor") dt3$Class<- as.factor(dt3$Class)
if (class(dt3$Order)!="factor") dt3$Order<- as.factor(dt3$Order)
if (class(dt3$Family)!="factor") dt3$Family<- as.factor(dt3$Family)
if (class(dt3$Genus)!="factor") dt3$Genus<- as.factor(dt3$Genus)
if (class(dt3$Species)!="factor") dt3$Species<- as.factor(dt3$Species)
if (class(dt3$Active)!="factor") dt3$Active<- as.factor(dt3$Active)
if (class(dt3$LengthMeasurementType)!="factor") dt3$LengthMeasurementType<- as.factor(dt3$LengthMeasurementType)

# Convert Missing Values to NA for non-dates

dt3$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt3$IEPFishCode))==trimws("NA")),NA,as.character(dt3$IEPFishCode)))
dt3$NonNative <- as.factor(ifelse((trimws(as.character(dt3$NonNative))==trimws("NA")),NA,as.character(dt3$NonNative)))
dt3$NonNative <- as.factor(ifelse((trimws(as.character(dt3$NonNative))==trimws("N/A")),NA,as.character(dt3$NonNative)))
dt3$Phylum <- as.factor(ifelse((trimws(as.character(dt3$Phylum))==trimws("NA")),NA,as.character(dt3$Phylum)))
dt3$Class <- as.factor(ifelse((trimws(as.character(dt3$Class))==trimws("NA")),NA,as.character(dt3$Class)))
dt3$Order <- as.factor(ifelse((trimws(as.character(dt3$Order))==trimws("NA")),NA,as.character(dt3$Order)))
dt3$Family <- as.factor(ifelse((trimws(as.character(dt3$Family))==trimws("NA")),NA,as.character(dt3$Family)))
dt3$Genus <- as.factor(ifelse((trimws(as.character(dt3$Genus))==trimws("NA")),NA,as.character(dt3$Genus)))
dt3$Species <- as.factor(ifelse((trimws(as.character(dt3$Species))==trimws("NA")),NA,as.character(dt3$Species)))
dt3$LengthMeasurementType <- as.factor(ifelse((trimws(as.character(dt3$LengthMeasurementType))==trimws("NA")),NA,as.character(dt3$LengthMeasurementType)))
dt3$LengthMeasurementType <- as.factor(ifelse((trimws(as.character(dt3$LengthMeasurementType))==trimws("N/A")),NA,as.character(dt3$LengthMeasurementType)))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(OrganismCode)
summary(IEPFishCode)
summary(CommonName)
summary(NonNative)
summary(Phylum)
summary(Class)
summary(Order)
summary(Family)
summary(Genus)
summary(Species)
summary(Active)
summary(LengthMeasurementType) 
# Get more details on character variables

summary(as.factor(dt3$OrganismCode)) 
summary(as.factor(dt3$IEPFishCode)) 
summary(as.factor(dt3$CommonName)) 
summary(as.factor(dt3$NonNative)) 
summary(as.factor(dt3$Phylum)) 
summary(as.factor(dt3$Class)) 
summary(as.factor(dt3$Order)) 
summary(as.factor(dt3$Family)) 
summary(as.factor(dt3$Genus)) 
summary(as.factor(dt3$Species)) 
summary(as.factor(dt3$Active)) 
summary(as.factor(dt3$LengthMeasurementType))
detach(dt3)               

# Save data? --------------------------------------------------------------

ifelse(savedata = T){save(dt1, dt2, dt3, MyZoops, file = "data/Zooper_and_EDSM_EDI_download.Rdata")}

