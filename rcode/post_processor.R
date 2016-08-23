# post_processor.R
# Code Base (C) 2.1: 3-7-16 (AB)

# Written by Alex Bettinardi
# July 14, 2006
# Slighly cleaning done for V2 3-6-2013 (AB) , but this needs heavy work to make this script readable.
# 2-3-2014 AB  - added correction to properly grow hourly volume to the future year - (2013 changes broke the logic)
#              - also worked to added better comments and explanation
# 8-26-2015 AB - adding Astoria's external model to the post processor
# 12-28-15 AB - this code wasn't fully tested with all the other changes in OSUM.  A small update (chaning i to iter) was needed to allow the code to run successfully.
# 1-13-16 AB - work with Prinevill revealed that the In/Out coding was backward.  It was corrected.

# To force OD to meet certain known Characteristics
# 1.  Correct External Model  - July 14, 2006 (AB)
      # Small update on 4/2/2013, removing meaningless ipf warning to clean up OSUM V2 - AB
      
# 2.  Add a special case external model for Astoria - Aug 26, 2015
      

#######################################
# 1.  Correct External Model
#######################################

#   Import Hourly External Volumes
#   This is csv should have a row for both directions for each external station.
#   The first 24 colmuns should be the hourly volumes in order from 12am-1am to 11pm-12am
#   The 25 colmun should indicate the station and the 26th should indicate AP or PA
#   Additional colmuns can be added for additional information, currently they are not used.

if(file.exists("data\\ext_Hr.csv")){ # if the ex_Hr.csv file doesn't exist this logic doesn't run
ext.Hr <- read.csv("data\\ext_Hr.csv", check.names=F)

if(any(!sort(unique(ext.Hr[,"ExtSta"])) == sort(as.numeric(rownames(ext.traffic))))) stop("ExtSta in ext.Hr don't match ext.traffic")

# Update blank hours or incorrect total volumes

for(st in rownames(ext.traffic)) { # for loop to work through each station
    
    # overall if statment to see if base volumes need to have blanks filled.  
    # the check is to ensure that the hourly volumes sum up to the "base" control
    if(sum(ext.Hr[ext.Hr[,"ExtSta"]==st,1:24]) != sum(ext.Hr[ext.Hr[,"ExtSta"]==st,"BaseVolume"])){
       
       # if the hourly volumes do not equal the daily control determine the differnce
       dif <- ext.Hr[ext.Hr[,"ExtSta"]==st,"BaseVolume"] - rowSums(ext.Hr[ext.Hr[,"ExtSta"]==st,1:24])
       
       # logic to see if zero columnes need to be filled with the ext.diurnal information
       if(any(colSums(ext.Hr[ext.Hr[,"ExtSta"]==st,1:24])==0)){
         
         # find missing percents from the ext.diurnal
         misPer <- ext.diurnal[colSums(ext.Hr[ext.Hr[,"ExtSta"]==st,1:24])==0,st]
         
         # logic to ensure that the percentage to apply to the missing hours is available from the daily
         if(sum(misPer) <= sum(dif)/sum(ext.Hr[ext.Hr[,"ExtSta"]==st,"BaseVolume"])){
           # create adjusted volume fields - 
           # under the case that there is more volume to be assigned than filling the missing hours covers (extraDif is the remaining)
           extraDif <- dif - sum(misPer)*ext.Hr[ext.Hr[,"ExtSta"]==st,"BaseVolume"]
           dif <- outer(ext.Hr[ext.Hr[,"ExtSta"]==st,"BaseVolume"],misPer)
         } else {
           # if the volume from the ext.diurnal file is greater than volume available to assign, adjust the diurnal volume down to the available volume 
           dif <- outer(dif, (misPer/sum(misPer)))
           extraDif <- 0
         } # end of misPer if statment
           
           # now that dif is created (if zero columns exist) - fill the zero columns with "dif"
           ext.Hr[ext.Hr[,"ExtSta"]==st,c(colSums(ext.Hr[ext.Hr[,"ExtSta"]==st,1:24])==0,rep(F,ncol(ext.Hr)-24))] <- dif
       } else {
          # if no missing fields, then any differences need to be accounted for in the "extra" difference logic below
          extraDif <- dif
       }# end of if zero col
    } else {
        # if hourly volumes match daily control "BaseVolume", then no extra volume needs to be assigned
        extraDif <- 0
    }# end of if !=
    
       # It needs to be assured that the ext.Hr input is controled by the ext.traffic input - specifically for future years
       extraDif <- extraDif + rep(ext.traffic[st,"adt"],2)/2 - ext.Hr[ext.Hr[,"ExtSta"]==st,"BaseVolume"]
       # add the "extra volume" propotionally to the trends in the filled "ext.Hr" file
       ext.Hr[ext.Hr[,"ExtSta"]==st,1:24] <- ext.Hr[ext.Hr[,"ExtSta"]==st,1:24] + sweep(sweep(ext.Hr[ext.Hr[,"ExtSta"]==st,1:24],1,rowSums(ext.Hr[ext.Hr[,"ExtSta"]==st,1:24]),"/"),1,extraDif,"*")

} # end of external station for loop
options(warn=-1)
rm(st, dif, misPer, extraDif)
options(warn=0)

# balance 24 hour volumes(in and out have to be equal)

inOut <- tapply(rowSums(ext.Hr[,1:24]),list(ext.Hr[,"ExtSta"],ext.Hr[,"DIRECTION"]),sum)
inOutFac <- rowSums(inOut)/2/inOut
ext.Hr[,1:24] <- ext.Hr[,1:24]*inOutFac[cbind(match(ext.Hr[,"ExtSta"],rownames(inOutFac)), match(ext.Hr[,"DIRECTION"],colnames(inOutFac)))]

rm(inOutFac)

# update OD matrix

for(hr in 1:24){
    
    rows <- ext.Hr[ext.Hr[,"DIRECTION"] == "IN",hr]
    names(rows) <- ext.Hr[ext.Hr[,"DIRECTION"] == "IN","ExtSta"]
    rows=rows[as.character(sort(as.numeric(names(rows))))]    
    cols <- ext.Hr[ext.Hr[,"DIRECTION"] == "OUT",hr]
    names(cols) <- ext.Hr[ext.Hr[,"DIRECTION"] == "OUT","ExtSta"]
    cols=cols[as.character(sort(as.numeric(names(cols))))]

   ee <- vehicle.ee.dist[,,hr]
   ei <- vehicle.ei.dist[,,hr]
   ie <- vehicle.ie.dist[,,hr]
   rowcheck <- 1
   colcheck <- 1
   iter <- 1
   # successively proportion rows and columns until closure or iteration criteria are met
   while((rowcheck > .01) & (colcheck > .01) & (iter < 100))
      {
	 rowtotal <- rowSums(ee)+rowSums(ei)
	 rowfactor <- rows/rowtotal
	 rowfactor[is.infinite(rowfactor)] <- 0
	 ee <- sweep(ee, 1, rowfactor, "*")/2 + ee/2
	 ei <- sweep(ei, 1, rowfactor, "*")
	 coltotal <- colSums(ee)+colSums(ie)
	 colfactor <- cols/coltotal
	 colfactor[is.infinite(colfactor)] <- 0
	 ee <- sweep(ee, 2, colfactor, "*")/2 + ee/2
	 ie <- sweep(ie, 2, colfactor, "*")
   rowcheck <- sum(abs(1-rowfactor[rows>0]))   # AB 4/2/2013 - only check rows that have a volume greater than zero
	 colcheck <- sum(abs(1-colfactor[cols>0]))   # AB 4/2/2013 - only check cols that have a volume greater than zero
	 iter <- iter + 1
       }
   if(iter == 100) warning(paste( "The maximum (", iter, ") number of iterations was reached, the post processor did NOT close, hr ", hr, sep=""))   
   rm(iter, rowcheck, colcheck, rowtotal, coltotal, rowfactor, colfactor)
   
   vehicle.ee.dist[,,hr] <- round(ee,2)
   vehicle.ei.dist[,,hr] <- round(ei,2)
   vehicle.ie.dist[,,hr] <- round(ie,2)

   } # end of hour for loop
rm(cols, rows, ee, ei, ie, hr)
} # end of hourly adjust for external volumes


#######################################
# 2.  Astoria's external model
#######################################

#   Only run for Astoria
if(ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "ASTORIAEXTERNALMODEL")),FALSE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "ASTORIAEXTERNALMODEL"))=="TRUE")){

#   Set up array to store the ee distributions
    vehicle.ee.dist <- array(0, c(num.ezones, num.ezones, 24))
    dimnames(vehicle.ee.dist) <- list(zone.names[e], zone.names[e], 1:24)

#   Calculate external-external vehicle trip distribution by hour
    # Develop daily distribution by iterative proportional fitting
    vehicle.ee.daily.dist <- ipf(ext.traffic[,"adt"]*ext.traffic[,"ee.pct"]/2, ext.traffic[,"adt"]*ext.traffic[,"ee.pct"]/2, ext.seed) 
    # Apply diurnal factors by destinations to 1/2 of daily EE O-D matrix
    # Apply diurnal factors by origins to 1/2 of daily EE O-D matrix
    # Add the two together to average the diurnal factor by origin and destination
    for(hour in 1:24){
        o.factor <- matrix(unlist(ext.diurnal[hour,]),ncol(ext.diurnal),ncol(ext.diurnal), byrow=T) # Fill by Row
        d.factor <- matrix(unlist(ext.diurnal[hour,]),ncol(ext.diurnal),ncol(ext.diurnal), byrow=F) # Fill by Column
    	vehicle.ee.dist[,,hour] <- o.factor*vehicle.ee.daily.dist/2 + d.factor*vehicle.ee.daily.dist/2
    }

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CALCULATE EXTERNAL-INTERNAL AND INTERNAL-EXTERNAL DISTRIBUTIONS BY HOUR
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Set up matrix to store the ei daily vehicle distribution in production-attraction form
    spec.gen.ei.daily.od <- matrix(0, num.ezones, num.izones)
    rownames(spec.gen.ei.daily.od) <- zone.names[e]
    colnames(spec.gen.ei.daily.od) <- zone.names[i]
    vehicle.ei.daily.pa <- matrix(0, num.ezones, num.izones)
    rownames(vehicle.ei.daily.pa) <- zone.names[e]
    colnames(vehicle.ei.daily.pa) <- zone.names[i]

#   Tabulate the trips associated with external trips to special generators
    if(exists("spec.gen")){
        #   Calculate the number of ei trips to special generators
        spec.ei.trips <- spec.gen[,"trips"] * (1 - spec.gen[,"int.pct"]) / 2
        names(spec.ei.trips) <- rownames(spec.gen)
        #   Establish origin probabilities based on ei traffic volumes
        spec.ext.orig.prob <- (ext.traffic[,"adt"] * ext.traffic[,"ei.pct"])/sum(ext.traffic[,"adt"] * ext.traffic[,"ei.pct"])
        #   Fill in vehicle distribution matrix with ei trips to special generators
        for(gen in specials){
            zone <- spec.gen[gen, "zone"]
            spec.gen.ei.daily.od[,zone] <- spec.ei.trips[gen] * spec.ext.orig.prob
            vehicle.ei.daily.pa[,zone] <- spec.ei.trips[gen] * spec.ext.orig.prob
        }
    }
#
    
#   Calculate the remaining number of ei trips
    other.ei.trips <- ext.traffic[,"adt"] * ext.traffic[,"ei.pct"] - rowSums(vehicle.ei.daily.pa)
    
#   Distribute the remaining ei trips using a gravity model formulated as a utility to distribute ei trips to internal zones.
    ei.attrac <- log(apply(daily.dist.pa, 2, sum)) # Sum ii attractions by all trip purposes and take log
    ei.attrac[is.infinite(ei.attrac)] <- 0 # Replace - infinity (log of 0) with zero
    ei.util <- sweep(-0.3*offpk.time[e,i], 2, ei.attrac, "+") # Make a utility of time and attractions
    ei.probs <- exp(ei.util)/sumExp(ei.util) # Calculate distribution probabilities
    ei.dist <- sweep(ei.probs, 1, other.ei.trips, "*") # Distribute daily trips according to probabilites
    vehicle.ei.daily.pa <- ei.dist + vehicle.ei.daily.pa

#   Set up matrix to store the ie daily vehicle distribution in production-attraction form
    vehicle.ie.daily.pa <- matrix(0, num.izones, num.ezones)
    rownames(vehicle.ie.daily.pa) <- zone.names[i]
    colnames(vehicle.ie.daily.pa) <- zone.names[e]
    
#   Distribute ie vehicle trips according to share of internal trip productions by zone
    ie.attrac <- ext.traffic[,"adt"] * ext.traffic[,"ie.pct"] # Calculate ie attractions at external stations
    ie.prod <- log(apply(daily.dist.pa, 1, sum)) # Sum ii productions by all trip purposes and take log
    ie.prod[is.infinite(ie.prod)] <- 0 # Replace -infinity values for zones with no productions to zero
    ie.util <- sweep(-0.3*offpk.time[i,e], 1, ie.prod, "+") # Make a utility of time and productions
    # Calculate production probabilities.  Note that since a fixed number of external trips is being apportioned
    # to internal zones based on the production utilities, the columns of the probability matrix need to add to one.
    # To do this using the sum.exp function, the matrix needs to be transposed and the results transposed.
    ie.probs <- t(exp(t(ie.util))/sumExp(t(ie.util))) # Calculate production probabilities
    vehicle.ie.daily.pa <- sweep(ie.probs, 2, ie.attrac, "*") # Distribute daily trips according to probabilities

    
#   Convert the ie and ei production-attraction matricies to origin-destination form
    vehicle.ei.daily.od <- (vehicle.ei.daily.pa + t(vehicle.ie.daily.pa))/2
    vehicle.ie.daily.od <- t(vehicle.ei.daily.od)
    
#   Distribute ei and ie trips temporally
    vehicle.ei.dist <- array(0, c(num.ezones, num.izones, 24))
    dimnames(vehicle.ei.dist) <- list(zone.names[e], zone.names[i], 1:24)
    vehicle.ie.dist <- array(0, c(num.izones, num.ezones, 24))
    dimnames(vehicle.ie.dist) <- list(zone.names[i], zone.names[e], 1:24)
    for(hour in 1:24){
        diurnal <- unlist(ext.diurnal[hour,])
        vehicle.ei.dist[,,hour] <- sweep(vehicle.ei.daily.od, 1, diurnal, "*")
	    vehicle.ie.dist[,,hour] <- sweep(vehicle.ie.daily.od, 2, diurnal, "*")
    }
     
#   Collapse and save the special generator ei and ie trip tables
    spec.gen.dist <- spec.gen.ei.daily.od[,spec.gen.ei.daily.od[1,] != 0]
    
    
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   COMBINE THE II, EE, EI AND IE VEHICLE TRIP TABLES
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Initialize an array to hold the results
    vehicle.dist <- array(0, c(num.zones, num.zones, 24))
    dimnames(vehicle.dist) <- list(zone.names, zone.names, 1:24)
#   Combine the individual tables for each hour
    for(hour in 1:24){
        vehicle.dist[,,hour] <- round(cbind(rbind(vehicle.ee.dist[,,hour], vehicle.ie.dist[,,hour]),  rbind(vehicle.ei.dist[,,hour], vehicle.ii.dist[,,hour])), 2)
    }
    vehicle.dist.daily <- apply(vehicle.dist, c(1,2), sum)

     # extra step to add the external trips into the peak OD by type array 7-28-06 AB
    peak.ODPu[zone.names[i],zone.names[e],"IE"] <- vehicle.ie.dist[,,pkHr]
    peak.ODPu[zone.names[e],zone.names[i],"EI"] <- vehicle.ei.dist[,,pkHr]
    peak.ODPu[zone.names[e],zone.names[e],"EE"] <- vehicle.ee.dist[,,pkHr]

} # end of Astoria's external model
