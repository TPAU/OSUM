# Model_Inputs.R
# Code Base (C) 2.2: 6-1-16 (AB)

# This script inputs data needed to run the model
# 
# Created: 02/14/03 Brian Gregor brian.j.gregor@odot.state.or.us
# Revised: 08/03/05 Alex Bettinardi alexander.o.bettinardi@odot.state.or.us
# Revised: 12/27/13 Alex Bettinardi alexander.o.bettinardi@odot.state.or.us
# Revised: 8/26/15  Alex Bettinardi alexander.o.bettinardi@odot.state.or.us - Changed to read AWDT from select_link.csv as opposed to AADT, which was incorrect
# Consolidated and revised from parts of other modules of the model code
# In 2013 this script was revised to work with OSUM V2 and use the SWIM external model
# Revised: 4/11/16 Alex Bettinardi alexander.o.bettinardi@odot.state.or.us - re-aligned OSUM with the latest version of externalModel_SWIM.R
# Revised: 4/18/16 Alex Bettinardi alexander.o.bettinardi@odot.state.or.us - Updated to take the "data" folder as an input (dataDir).  This is needed for Newport - dual - logic
# Revised: 6/1/16 Alex Bettinardi - corrected spelling of "vistorModel" due to edits needs in trip_distribution.r

# This script loads all the data needed to run the model.
# This sciprt loads land use and peak hour data from the VISUM network,
# and most of the csv datasets contained in the "data" folder
# (the acception is any optional dataset that are read in later scripts if provided)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODEL RUN DESCRIPTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# auto mode is the "key" that points to the auto or car OD matrix
aMode <- "C"

#   Household trip rate for study area
#   This is not anticpated to change ever.
hh.trip.rate <- comGetProperty(vbConv("Visum.Net"), "AttValue", "HHTRIPRATE")

# Peak Hour for this Model
pkHr <- comGetProperty(vbConv("Visum.Net"), "AttValue", "PEAKHOUR")
# am peak is used to obtain the to-work travel times
# which are assumed to be used by those choseing where to work.
amPk <- comGetProperty(vbConv("Visum.Net"), "AttValue", "AM_PEAKHOUR")

# Get Intrazonal travel time
terminalTime <- comGetProperty(vbConv("Visum.Net"), "AttValue", "TERMINALTIME")

# Get City and Year (for documentation and plotting)
city <- comGetProperty(vbConv("Visum.Net"), "AttValue", "CITY")
year <- comGetProperty(vbConv("Visum.Net"), "AttValue", "YEAR")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPITIONAL INPUT - if used the "bg" (block group) field must exist as a zone attribute 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check for exixting workers per HH object (built from 1990 CTPP).
# Otherwise, use the estimated values from the hh submodel
# needs to have a joint distribution for worker by house type (16 columns)
# The code is currently written to handel additional columns from census
# such as 4+workers and totals, it uses these columns to adjust the raw census data
# This is so the user can just take the census output, delete the two header rows and the seven summary columns,
# and lastly save it as workershh.csv in the data folder (one also as the option to
# balance totals and combine 3 & 4+ columns outside of OSUM).
if(file.exists(paste(dataDir,"\\workershh.csv",sep=""))) workershh <- read.table(paste(dataDir,"\\workershh.csv",sep=""), header=T, row.names=1, sep=",", check.names=F)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTERNAL TRAFFIC INPUTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# additional check to see if the SWIM external model should run for the visitor model, if present
visitorSWIMcheck <- TRUE
if(exists("visitorModel")){
   if(visitorModel==T){
      visitorSWIMcheck <- ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "V_SWIMEXTERNALMODEL")),TRUE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "V_SWIMEXTERNALMODEL"))=="TRUE")
}}   

#New if statement to run the SWIM External Model, if flagged "TRUE" in VISUM under SWIMEXTERNALMODEL
if(ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL")),FALSE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL"))=="TRUE") & visitorSWIMcheck){
   
   ##/
   #SWIM time-of-day periods for external model
   #ensure that daily is a period
   #@param SWIM_TOD_Periods - SWIM time-of-day periods
   #@return SWIM_TOD_Periods - SWIM time-of-day periods
   ##/
   if(!file.exists(paste(dataDir,"\\TOD_Periods.csv",sep=""))) stop("TOD_Periods.csv file is missing")
   TOD_periods  <- read.csv(paste(dataDir,"\\TOD_Periods.csv",sep=""), header=T, as.is=T)
   if(sum("daily" %in% TOD_periods$Period) == 0) TOD_periods <- rbind(TOD_periods, list("daily", 0, 2359, "all times of the day"))

   ##/
   #External model input data
   #For SWIM subarea model columns are STATIONNUMBER, AutoAWDT, TruckAWDT, AWDT_YEAR, GrowthRate, and all applicable period factor columns
   #The stations should be sorted from lowest to highest
   #This input is used to adjust SWIM external model trends to known or predicted station volumes
   #@param externals - selectLinks.csv input table
   #@return externals - externals input table
   ##/
   if(!file.exists(paste(dataDir,"\\selectLinks.csv",sep=""))) stop("selectLinks.csv file is missing")
   externals <- read.csv(paste(dataDir,"\\selectLinks.csv",sep=""))
   # clean the file so only required fields are kept
   # first find period fields
   pCols <- unlist(sapply(TOD_periods$Period, function(x) grep(paste("^",x,"_",sep=""), colnames(externals))))
   colnames(externals)[pCols] <- names(pCols)
   externals <- externals[,c("STATIONNUMBER", "DIRECTION", "AutoAWDT", "TruckAWDT", "AWDT_YEAR", names(pCols), "GrowthRate")]
   externals <- externals[order(externals$STATIONNUMBER,externals$DIRECTION),]
   rm(pCols)
   colnames(externals)[1] <- "station"
   
   # make a legacy ext.traffic data set to work with other aspects of OSUM
   ext.traffic <- as.data.frame(tapply(externals$AutoAWDT, externals$station, sum))
   names(ext.traffic) <- "adt"

   ##/
   #SWIM zones to JEMnR model zones crosswalk
   #@param Crosswalk - SWIM zones to JEMnR model zones crosswalk
   #@return Crosswalk - SWIM zones to JEMnR model zones crosswalk
   ##/
   if(!file.exists(paste(dataDir,"\\SWIM_Local_TAZ_CW.csv",sep=""))) stop("SWIM_Local_TAZ_CW.csv file is missing")
   Crosswalk <- read.csv(paste(dataDir,"\\SWIM_Local_TAZ_CW.csv",sep=""))
   
} else {
   
   # External station volumes and splits
   if(!file.exists(paste(dataDir,"\\ext_traffic.csv",sep=""))) stop("ext_traffic.csv file is missing")
   ext.traffic <- as.matrix(read.csv(paste(dataDir,"\\ext_traffic.csv",sep=""), row.names=1, check.names=F))

   # Seed matrix for distributing external-external trips
   if(!file.exists(paste(dataDir,"\\ext_seed.csv",sep=""))) stop("ext_seed.csv file is missing")
   ext.seed <- as.matrix(read.csv(paste(dataDir,"\\ext_seed.csv",sep=""), row.names=1, check.names=F))

   # Diurnal factors for external trips
   if(!file.exists(paste(dataDir,"\\ext_diurnal.csv",sep=""))) stop("ext_diurnal.csv file is missing")
   ext.diurnal <- read.table(paste(dataDir,"\\ext_diurnal.csv",sep=""), header=T, row.names=1, sep=",", check.names=F) 
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SOCIO-ECONOMIC INPUTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in taz data, apply column names as lower case and sort TAZs in ascending order.
correctTAZnames <- toupper(c("TAZ","HH","HHI1","HHI2","HHI3","HHI4","HHS1","HHS2","HHS3","HHS4","EMP","RETL","SERV","SCHE","DISTRICT"))
if(exists("visitorModel")){
   if(visitorModel==T){ #4-18-16 AB - add visitor model case # 6-1-16 AB - corrected spelling
   correctTAZnames <- toupper(c("TAZ","VHH","VHHI1","VHHI2","VHHI3","VHHI4","VHHS1","VHHS2","VHHS3","VHHS4","EMP","RETL","SERV","SCHE","DISTRICT"))
}}
# check to see of the Block Group field (BG) is needed - if the workershh (worker distribution exists
if(file.exists(paste(dataDir,"\\workershh.csv",sep=""))) correctTAZnames <- toupper(c("TAZ","HH","BG","HHI1","HHI2","HHI3","HHI4","HHS1","HHS2","HHS3","HHS4","EMP","RETL","SERV","SCHE","DISTRICT")) 
taz.data <- getAttTable(Visum,"Zones",correctTAZnames,initalFilter=T)   
taz.data <- taz.data[!taz.data[,1] %in% as.numeric(row.names(ext.traffic)),]
rownames(taz.data) <- taz.data[,1]
names(taz.data) <- gsub("^V","",names(taz.data)) # 4-13-16 AB handle leading V from external model
correctTAZnames <- gsub("^V","",correctTAZnames) # 4-13-16 AB handle leading V from external model
# check to see if the average household size field needs to be created
if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "FUTURE_HH_PREPROCESSOR"))){
if(tolower(comGetProperty(vbConv("Visum.Net"), "AttValue", "FUTURE_HH_PREPROCESSOR")) %in% c("new", "orig")){   
   taz.data$AHHS <- colSums(t(taz.data[,paste("HHS",1:4, sep="")])*1:4)
   correctTAZnames <- c(correctTAZnames[1:2], "AHHS", correctTAZnames[3:length(correctTAZnames)])
}}
names(taz.data) <- tolower(names(taz.data))
taz.data <- taz.data[order(taz.data$taz),tolower(correctTAZnames)]
names(taz.data) <- c("taz", paste(tolower(correctTAZnames[!correctTAZnames %in% c("TAZ","DISTRICT")]), "base", sep=""), "district")
if(file.exists(paste(dataDir,"\\workershh.csv",sep=""))) taz.data$bgbase <- as.character(taz.data$bgbase)
rm(correctTAZnames)

# Regional joint distribution of households by size and income
if(!file.exists(paste(dataDir,"\\size_inc_seed.csv",sep=""))) stop("size_inc_seed.csv file is missing")
size.inc.seed <- as.matrix(read.csv(paste(dataDir,"\\size_inc_seed.csv",sep=""), row.names=1))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TRIP GENERATION INPUTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Home-based work trip rates
#   This is part of the joint model specification and ordinarily should not be changed.
if(!file.exists(paste(dataDir,"\\hbw_rate.csv",sep=""))) stop("hbw_rate.csv file is missing")
hbw.rate <- unlist(read.csv(paste(dataDir,"\\hbw_rate.csv",sep=""), check.names=F))
   
#   Home-based school trip rates
#   This is part of the joint model specification and ordinarily should not be changed.
if(!file.exists(paste(dataDir,"\\hbsch_rate.csv",sep=""))) stop("hbsch_rate.csv file is missing")
hbsch.rate <- unlist(read.csv(paste(dataDir,"\\hbsch_rate.csv",sep=""), check.names=F))
   
#   Home-based shop trip rates
#   This is part of the joint model specification and ordinarily should not be changed.
if(!file.exists(paste(dataDir,"\\hbshp_rate.csv",sep=""))) stop("hbshp_rate.csv file is missing")
hbshp.rate <- as.matrix(read.csv(paste(dataDir,"\\hbshp_rate.csv",sep=""), row.names=1, check.names=F))

#   Home-based recreation/other trip rates
#   This is part of the joint model specification and ordinarily should not be changed.
if(!file.exists(paste(dataDir,"\\hbro_rate.csv",sep=""))) stop("hbro_rate.csv file is missing")
hbro.rate <- as.matrix(read.csv(paste(dataDir,"\\hbro_rate.csv",sep=""), row.names=1, check.names=F))

#   Non-home-based trip rates
#   This is part of the joint model specification and ordinarily should not be changed.
if(!file.exists(paste(dataDir,"\\nhb_rate.csv",sep=""))) stop("nhb_rate.csv file is missing")
nhb.rate <- as.matrix(read.csv(paste(dataDir,"\\nhb_rate.csv",sep=""), row.names=1, check.names=F))

# Special generators
if(!file.exists(paste(dataDir,"\\spec_gen.csv",sep=""))) {
     cat("\nFYI - no spec_gen.csv file is provided\n")
} else {     
   spec.gen <- read.csv(paste(dataDir,"\\spec_gen.csv",sep=""), row.names=1, check.names=F)
   spec.gen$zone <- as.character(spec.gen$zone)
   spec.gen$type <- as.character(spec.gen$type)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VEHICLE OCCUPANCY FACTORS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Vehicle occupancy factors for internal trips by purpose and external
# This may vary by model area.
if(!file.exists(paste(dataDir,"\\occ.csv",sep=""))) stop("occ.csv file is missing")
occ <- unlist(read.csv(paste(dataDir,"\\occ.csv",sep="")))
ext.occ <- occ["external"]
occ <- occ[c("hbw","hbsch","hbshp","hbro","nhb")]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DESTINATION CHOICE UTILITY COEFFICIENTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Size utility coefficients for internal trip distribution
#   This is part of the joint model specification and ordinarily should not be changed.
if(!file.exists(paste(dataDir,"\\size_coeff.csv",sep=""))) stop("size_coeff.csv file is missing")
size.coeff <- as.matrix(read.csv(paste(dataDir,"\\size_coeff.csv",sep=""), row.names=1, check.names=F))

#   Time utility coefficients for internal trip distribution
#   These may be adjusted by model area to match average trip length statistics.
if(!file.exists(paste(dataDir,"\\time_coeff.csv",sep=""))) stop("time_coeff.csv file is missing")
time.coeff <- as.matrix(read.csv(paste(dataDir,"\\time_coeff.csv",sep=""), row.names=1, check.names=F))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DIURNAL AND DIRECTIONAL FACTORS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Diurnal factors for internal trips
if(!file.exists(paste(dataDir,"\\diurnal.csv",sep=""))) stop("diurnal.csv file is missing")
diurnal <- read.table(paste(dataDir,"\\diurnal.csv",sep=""), header=T, sep=",", row.names=1)

# Directional factors for internal trips
if(!file.exists(paste(dataDir,"\\directional.csv",sep=""))) stop("directional.csv file is missing")
directional <- read.table(paste(dataDir,"\\directional.csv",sep=""), header=T, sep=",", row.names=1)


# Extra levers availalbe in OSUM
# They must exist, but maybe be left with default (no adjustment) settings
#=============================================================================================

if(!file.exists(paste(dataDir,"\\retention.csv",sep=""))) stop("retention.csv file is missing")
retention <- unlist(read.csv(paste(dataDir,"\\retention.csv",sep="")))

if(!file.exists(paste(dataDir,"\\district_bias.csv",sep=""))) stop("district_bias.csv file is missing")
district.bias <- read.csv(paste(dataDir,"\\district_bias.csv",sep=""))
addDistrict <- unique(taz.data$district)[is.na(match(unique(taz.data$district), district.bias$dist))]
if(length(addDistrict) > 0) district.bias <- do.call(rbind, list(district.bias[1:7],unlist(lapply(addDistrict,function(x) c(x,rep(1,5),0)))))
district.bias <- as.list(district.bias[1:7])  
district.names <- as.character(district.bias[[1]])
district.bias <- lapply(district.bias[2:7], function(x) {names(x) <- district.names; x})
rm(addDistrict, district.names)

if(!file.exists(paste(dataDir,"\\district_time_adj.csv",sep=""))) stop("district_time_adj.csv file is missing")
district.time.adj <- read.table(paste(dataDir,"\\district_time_adj.csv",sep=""), header=T, sep=",")
#Format the district.time.adj object
districts <- unique(taz.data$district)
district.time.adj[[1]] <- as.character(district.time.adj[[1]])
district.time.adj[[2]] <- as.character(district.time.adj[[2]])
district.time.adj <- flat.to.mat(district.time.adj[,c(1,2,3)],districts,1)
rm(districts)

if(!file.exists(paste(dataDir,"\\global_time_adj.csv",sep=""))) stop("global_time_adj.csv file is missing")
global.time.adj <- unlist(read.csv(paste(dataDir,"\\global_time_adj.csv",sep="")))

#==============================================================

# source in the SWIM external Model, if Flagged
#==============================================================
if(ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL")),FALSE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL"))=="TRUE") & visitorSWIMcheck){
   #start "fun" list to match JEMnR
   fun <- list()
   source("rcode\\externalModel_SWIM.R")
   attach(fun) 
   library(doParallel) # required for SWIM external model     
   # load required objects and rename existing object to match JEMnR 
   SWIM_SL_Filename_Pattern <- comGetProperty(vbConv("Visum.Net"), "AttValue", "EXT_MODEL_DATA_FILENAME_PATTERN")     
   externalDisaggregateMethodNumber <- comGetProperty(vbConv("Visum.Net"), "AttValue", "EXT_MODEL_DISAGGREGATE_METHOD_NUMBER")
   externalIPFflag <- comGetProperty(vbConv("Visum.Net"), "AttValue", "EXT_MODEL_IPF_FLAG")
   externalIPFflag <- ifelse(is.null(externalIPFflag),T,ifelse(externalIPFflag=="FALSE",F,T))
   # update taz table to match JEMnR structure
   taz <- taz.data
   names(taz) <- toupper(names(taz))
   
   # remove the spec.gen employment and or households from the external land use control totals, spec gen exsits.
   if(exists("spec.gen")){ 
      specGenEmp <- tapply(rowSums(spec.gen[,c("retl.emp", "serv.emp", "othr.emp")]),spec.gen$zone, sum)
      taz[names(specGenEmp),"EMPBASE"] <- taz[names(specGenEmp),"EMPBASE"] - specGenEmp
      if(any(taz[names(specGenEmp),"EMPBASE"]<0)) stop(paste("The following Spec Gen TAZs have more employment then in the TAZ file:", paste(names(specGenEmp)[taz[names(specGenEmp),"EMPBASE"]<0], collapse=", "), sep=" "))
      rm(specGenEmp)
      specGenHH <- tapply(spec.gen$hh,spec.gen$zone, sum)
      taz[names(specGenHH),"HHBASE"] <- taz[names(specGenHH),"HHBASE"] - specGenHH
      if(any(taz[names(specGenHH),"HHBASE"]<0)) stop(paste("The following Spec Gen TAZs have more Households then in the TAZ file:", paste(names(specGenHH)[taz[names(specGenHH),"HHBASE"]<0], collapse=", "), sep=" "))
      rm(specGenHH)       
   }
   taz$POPBASE <- taz$HHBASE* rowSums(sweep(taz[,grep("^HHS",names(taz))],2,1:4,"*"))
   taz$EMPBASE <- taz$EMPBASE + district.bias[["ext"]][as.character(taz.data$district)]  
   # create external zones object 
   externalZones <- row.names(ext.traffic)
   # set up the external model inputs
   inputLoc=paste(dataDir,"/",sep="")
   storeLoc=paste(dataDir,"/",sep="")
   IPF=externalIPFflag
   # update ext.traffic (for other OSUM modules)
   ext.traffic$ei.pct <- ext.traffic$ie.pct <- ext.traffic$ee.pct <- 0
   
   # run the external model code
   # ============================
   #process SWIM select link datasets to create RData files
	 procSLDataSets(SWIM_SL_Filename_Pattern, inputLoc)
	
	 # Create external array
	 # Choose a disaggregation  method (select between 1 to 4)
   disaggregateMethod <- c("SWIMPCT","LOCALPOPSHARE", "LOCALEMPSHARE", "LOCALPOP2EMPSHARE")
	 disaggregateMethod <- disaggregateMethod[externalDisaggregateMethodNumber]
	 print(paste("create external demand array using", disaggregateMethod))
	 pct <- createExtMats(disaggregateMethod, inputLoc, storeLoc)
	 ext.traffic[,c("ee.pct", "ie.pct", "ei.pct")] <- round(pct$total/rowSums(pct$total),4)
	 
	 # Adjust volumes to analysis year
   externals$daily_auto <- externals$AutoAWDT * (1 + (externals$GrowthRate*(year  - externals$AWDT_YEAR)))
   externals$daily_truck <- externals$TruckAWDT * (1 + (externals$GrowthRate*(year  - externals$AWDT_YEAR)))
   ext.traffic$adt <- tapply(rowSums(externals[,c("daily_auto", "daily_truck")]), externals$station, sum)[rownames(ext.traffic)]
	 
	 # create a spec gen external matrix and factor down the 
	 if(exists("spec.gen")){ 
      
      #   Calculate the number of ei trips to special generators by zone and purpose
      specGenExtTrips.ZnPu <- tapply(spec.gen[spec.gen$type != "truck","trips"] * (1 - spec.gen[spec.gen$type != "truck","int.pct"]), paste(spec.gen[spec.gen$type != "truck","zone"], spec.gen[spec.gen$type != "truck","type"]), sum)
      
      #   Establish origin probabilities based on ei traffic volumes
      auto.ei <- round(pct$auto/rowSums(pct$auto),4)[as.character(externals$station),"ei"]
      spec.ext.orig.prob <- (externals$daily_auto * auto.ei) / sum(externals$daily_auto * auto.ei)
      #   Create factors for each period (hour of the day)
      spec.ext.orig.prob <- as.matrix(sweep(externals[,TOD_periods$Period[TOD_periods$Period != "daily"]], 1, spec.ext.orig.prob, "*")) 
      rownames(spec.ext.orig.prob) <- paste(externals$station, externals$DIRECTION)
      
      #   Create vehicle special generators matrix by external station, hour, zone, and purpose
      spec.ext.OD <- outer(spec.ext.orig.prob, specGenExtTrips.ZnPu)
      
      rm(specGenExtTrips.ZnPu, spec.ext.orig.prob, auto.ei)
      
      # create a copy of externals
      copyExternals <- externals
      # reduce the auto AWDT in the externals table for the IPF process to follow
      externals$daily_auto <- externals$daily_auto - apply(spec.ext.OD,1,sum)
      
      # reduce truck AWDT, if truck is a purpose in the spec gen table
      if(any(spec.gen$type == "truck")){
         specGenExtTrips.Zn <- tapply(spec.gen[spec.gen$type == "truck","trips"], paste(spec.gen[spec.gen$type == "truck","zone"],"truck"), sum)
         
         #   Establish origin probabilities based on ei truck volumes
         truck.ei <- round(pct$truck/rowSums(pct$truck),4)[as.character(externals$station),"ei"]
         spec.ext.orig.prob <- (externals$daily_truck * truck.ei) / sum(externals$daily_truck * truck.ei)
         #   Create factors for each period (hour of the day)
         spec.ext.orig.prob <- as.matrix(sweep(externals[,TOD_periods$Period[TOD_periods$Period != "daily"]], 1, spec.ext.orig.prob, "*")) 
         rownames(spec.ext.orig.prob) <- paste(externals$station, externals$DIRECTION)
      
         #   Create vehicle special generators matrix by external station, hour, zone, and purpose
         spec.ext.truck.OD <- outer(spec.ext.orig.prob, specGenExtTrips.Zn)
         
         # Combine truck matrix to auto matrix for easier processing in the next step
         temp <- spec.ext.OD[,,rep(1:dim(spec.ext.OD)[3],length.out=(dim(spec.ext.OD)[3]+dim(spec.ext.truck.OD)[3]))] 
         dimnames(temp)[[3]] <- c(dimnames(spec.ext.OD)[[3]], dimnames(spec.ext.truck.OD)[[3]])
         temp[,,dimnames(spec.ext.truck.OD)[[3]]] <- spec.ext.truck.OD
         spec.ext.OD <- temp
      
         # reduce the auto AWDT in the externals table for the IPF process to follow
         externals$daily_truck <- externals$daily_truck - apply(spec.ext.truck.OD,1,sum)
         
         # Update Spec Gen table so that it is used correctly in the rest of OSUM modules - need to remove the truck purpose, if exists 
         spec.gen <- spec.gen[spec.gen$type != "truck",]  
         
         rm(specGenExtTrips.Zn, spec.ext.orig.prob, truck.ei, spec.ext.truck.OD, temp)        
      }
      spec.gen.dist <- apply(spec.ext.OD, c(1,3), sum)
      spec.gen.dist <- tapply(as.vector(spec.gen.dist), as.list(expand.grid(unlist(lapply(strsplit(rownames(spec.gen.dist), " "), function(x) x[1])), unlist(lapply(strsplit(colnames(spec.gen.dist), " "), function(x) x[1])))), sum)
	    spec.gen.dist <- spec.gen.dist[,colSums(spec.gen.dist)!=0]
   }
	
	 # IPF external matrix to counts   
   ipfExtMatsToCounts(IPF,storeLoc) #set to FALSE to skip IPF and just collapse on purpose 
   
   # Add the Special Generators to final external OD matrix and return externals back to the input
	 if(exists("spec.gen")){ 
	    #get OD array to add special gen
      load(paste(storeLoc, "externalOD_ZnZnTdMd.RData", sep=""))
      
      for(r in dimnames(spec.ext.OD)[[3]]){
         # add ie by period
         ext.ZnZnTdMd[strsplit(r, " ")[[1]][1],gsub(" OUT", "", rownames(spec.ext.OD)[grep(" OUT", rownames(spec.ext.OD))]),colnames(spec.ext.OD),strsplit(r, " ")[[1]][2]] <- ext.ZnZnTdMd[strsplit(r, " ")[[1]][1],gsub(" OUT", "", rownames(spec.ext.OD)[grep(" OUT", rownames(spec.ext.OD))]),colnames(spec.ext.OD),strsplit(r, " ")[[1]][2]] + spec.ext.OD[grep(" OUT", rownames(spec.ext.OD)),,r]
         # add ie for Daily
         ext.ZnZnTdMd[strsplit(r, " ")[[1]][1],gsub(" OUT", "", rownames(spec.ext.OD)[grep(" OUT", rownames(spec.ext.OD))]),"daily",strsplit(r, " ")[[1]][2]] <- ext.ZnZnTdMd[strsplit(r, " ")[[1]][1],gsub(" OUT", "", rownames(spec.ext.OD)[grep(" OUT", rownames(spec.ext.OD))]),"daily",strsplit(r, " ")[[1]][2]] + rowSums(spec.ext.OD[grep(" OUT", rownames(spec.ext.OD)),,r])
         # add ei by period
         ext.ZnZnTdMd[gsub(" IN", "", rownames(spec.ext.OD)[grep(" IN", rownames(spec.ext.OD))]), strsplit(r, " ")[[1]][1],colnames(spec.ext.OD),strsplit(r, " ")[[1]][2]] <- ext.ZnZnTdMd[gsub(" IN", "", rownames(spec.ext.OD)[grep(" IN", rownames(spec.ext.OD))]),strsplit(r, " ")[[1]][1],colnames(spec.ext.OD),strsplit(r, " ")[[1]][2]] + spec.ext.OD[grep(" IN", rownames(spec.ext.OD)),,r]
         # add ei for Daily
         ext.ZnZnTdMd[gsub(" IN", "", rownames(spec.ext.OD)[grep(" IN", rownames(spec.ext.OD))]), strsplit(r, " ")[[1]][1],"daily",strsplit(r, " ")[[1]][2]] <- ext.ZnZnTdMd[gsub(" IN", "", rownames(spec.ext.OD)[grep(" IN", rownames(spec.ext.OD))]),strsplit(r, " ")[[1]][1],"daily",strsplit(r, " ")[[1]][2]] + rowSums(spec.ext.OD[grep(" IN", rownames(spec.ext.OD)),,r])            
      }   
      save(ext.ZnZnTdMd, file=paste(storeLoc, "externalOD_ZnZnTdMd.RData", sep=""))
      
      # save out correct copy of externals information for reporting
      externals[,c("daily_auto", "daily_truck")] <- copyExternals[,c("daily_auto", "daily_truck")]
      write.csv(externals, paste(storeLoc, "selectLinks_Report.csv", sep=""), row.names=F)
      
      rm(r,spec.ext.OD,ext.ZnZnTdMd, copyExternals)
	 }
   
   # clean up everything that is not required for the rest of OSUM
   detach(fun)
   rm(fun, SWIM_SL_Filename_Pattern, externalDisaggregateMethodNumber, disaggregateMethod, taz, TOD_periods, externalZones, inputLoc, storeLoc, IPF, pct, Crosswalk, externalIPFflag, externals) 
}
rm(visitorSWIMcheck)
