# trip_distribution.R
# Code Base (C) 2.2: 6-1-16 (AB)

# This program calculates trip distribution for small urban areas in Oregon.
# 
# Created: 6/25/02 Brian Gregor brian.j.gregor@odot.state.or.us
# Updated: 1/10/03 Ben Stabler benjamin.stabler@odot.state.or.us, Brian Gregor brian.j.gregor@odot.state.or.us
# Rewritten: 2/14/03 Brian Gregor
# Updated: 3/19/03 Brian Gregor
# Updated: 3/20/03 Brian Gregor
# Updated: 5/12/03 Brian Gregor
# Updated: 9/2/03 Ben Stabler
# Updated: 02/21/13 Alex Bettinardi
# Updated: 05/8/13 Alex Bettinardi
# Updated: 04/13/16 Alex Bettinardi
# Updated: 06/01/16 Alex Bettinardi
#
# Copyright (C) 2002  Oregon Department of Transportation
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#
# CHANGES
# 12/18/02: Changed the approach to distribute external trips and to split by hour
#   1.  External trips are distributed spatially first and temporally second, reversing the previous process
#   2.  Separate processes deal with EE, EI and IE trips
#   3.  EI and IE trips are distributed spatially based on a gravity model implemented as a logit
#       Formerly they were distributed in the same proportions as II trips.
#       This change makes the distribution of external trips sensitive to travel time
#       To do this, a time skim for all zones, not just internal zones needs to be used
# 1/10/03: Changed the IE and EI model to a basic gravity model
# 2/14/03:
#   Changed the code to account for trips from special generators.
#   Corrected errors in distribution methods for external trips.
# 3/19/03:
#   Changed the code for ei and ie trip distribution to account for these in origin-destination form.
#   Added the distribution of external trips to special generators to the output list.
# 3/20/03:
#   Corrected imbalances between ei and ie trip matrices
# 5/12/03 change in call to distribution probability function to reflect changes in dist.prob function
# 9/2/03 Changed ext.traffic and ext.traffic.futr inputs to only contain ee percent, thus making the
#   code split the remaining ADT equally between EI and IE.
# 2/21/13 AB
#   Revised for OSUM V2
# 5/8/2013
#   Added the Optional SWIM external model code 
# 8/26/2015 AB
#   Added a "local=T" call to the source line for the post_processor.R code.
#   This is so the Astoria external model would run in the trip Distribution enviornment which contains all the data objects needed
#   This change should not impact the performance or results of other OSUM models that will use this code.
# 4/13/2016 AB
#   Allowed for the code to work with the vistor model
# 6/1/2016 AB
#   Updated to allow for orginal external model to work with vistor model even if swim external is used for non-visitor travel.


#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   PURPOSE
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   This trip distribution function distributes person and vehicle trips using a destination choice method.
#   The function returns a list object.
#   The first element of the list is a 3D array of vehicle trip distribution by origin zone (row), destination zone (column) and hour (table).
#   The second element of the list is a 3D array of person trip distribution by production zone (row), attraction zone (column) and trip purpose (table).
#   The third element of the list is a 3D array of person trip distribution by origin zone (row), attraction zone (column) and trip purpose (table)
#   The fourth element of the list is a data.frame of summary travel statistics for internal trips by purpose (total trips, mean travel time, travel time sd)


#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   DATA OBJECTS THE FUNCTION USES
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   The following objects are always used and must exist in the workspace:
#   ----------------------------------------------------------------------
#   "pk.time" is a matrix of congested (peak period) interzonal travel times.
#   "offpk.time" is a matrix of uncongested (offpeak period) interzonal travel times.
#   "trip.prod" is a list containing two 3D arrays of internal trip productions called "regular" and "special".
#       The 1st array stores regular trip productions by zone (rows), hour (columns), and purpose (tables).
#       The 2nd array stores special generator trip productions by zone (rows), hour (columns), and generator (tables).
#   "taz.data" is a dataframe.  Each row contains the socioeconomic data for each model zone.
#   "directional" is a matrix of directional factors by time of day.  Rows are hours of the day.
#       Columns are purpose and direction (eg. nhb.pa and nhb.ap means proportions of nhb trips in the production to attraction and attraction to production directions respectively.
#   "ext.dirurnal" is a matrix of diurnal factors by hour (rows) and external station (columns).
#   "occ" is a vector of average vehicle occupany by trip purpose


#   The following objects are used if they are in the workspace and are needed:
#   --------------------------------------------------------------------------
#   "spec.gen" is a matrix of data on special generators.  Columns are special generators.  Rows are the data fields.
#   "ext.occ" is a scalar value of average vehicle occupancy for external trips.


# Function to calculate vehicle trip distribution
calc.trip.dist <- function()
{

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   COMMON DATA
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    zone.names <- c(rownames(ext.traffic), taz.data[,"taz"])
    num.izones <- nrow(taz.data)
    num.ezones <- nrow(ext.traffic)
    num.zones <- num.izones + num.ezones
    i <- num.ezones + 1:num.izones  # index for internal portions of matrix
    e <- 1:num.ezones  # index for external portions of matrix
    trip.purp <- c("hbw", "hbsch", "hbshp", "hbro", "nhb")
    num.purp <- length(trip.purp)
    if(exists("spec.gen")) specials <- rownames(spec.gen)

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CREATE MATRIX OF SIZE VARIABLES FOR TRIP DISTRIBUTION UTILITIES
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Extract the proper fields of data from taz.data
    attach(taz.data)  # attaching the object allows direct referencing of object columns
    size.vars <- cbind(retl.emp=retlbase, serv.emp=servbase, othr.emp=empbase-retlbase-servbase, hh=hhbase, sch.enrl=schebase)
    rownames(size.vars) <- taz
    detach(taz.data)  # detach to avoid name conflicts
    
#   Subtract the portions of the size variables attributable to the special generators since these are distributed separately
    if(exists("spec.gen")){
        for(gen in specials){
            spec.zone <- spec.gen[gen, "zone"]
            size.adj <- unlist(spec.gen[gen, c("retl.emp", "serv.emp", "othr.emp", "hh", "sch.enrl")])
            size.vars[spec.zone,] <- size.vars[spec.zone,] - size.adj
            if(any(size.vars[spec.zone,]<0)) stop(paste("One of the size variables for the", gen, "special generator is miscoded. Check input data.", sep=" "))
        }
    }
    
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CALCULATE PEAK AND OFF PEAK DISTRIBUTION PROBABILITIES BY TRIP PURPOSE
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    for(purp in trip.purp){
        assign(paste(purp, "offpk.prob", sep="."), dist.prob(offpk.time[i,i], purp, size.c=size.coeff, time.c=time.coeff, size.v=size.vars, global.t=global.time.adj[purp], district.t=district.time.adj, district.b=district.bias))
    }

    for(purp in trip.purp){
        assign(paste(purp, "pk.prob", sep="."), dist.prob(pk.time[i,i], purp, size.c=size.coeff, time.c=time.coeff, size.v=size.vars, global.t=global.time.adj[purp], district.t=district.time.adj, district.b=district.bias))
    }

    # make lists of peak and off-peak distribution probabilities
    offpk.prob <- list(hbw=hbw.offpk.prob, hbsch=hbsch.offpk.prob, hbshp=hbshp.offpk.prob, hbro=hbro.offpk.prob, nhb=nhb.offpk.prob)
    pk.prob <- list(hbw=hbw.pk.prob, hbsch=hbsch.pk.prob, hbshp=hbshp.pk.prob, hbro=hbro.pk.prob, nhb=nhb.pk.prob) 
    prob.check <<- list(offpeak=offpk.prob, peak=pk.prob)

    
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CALCULATE INTERNAL-INTERNAL PERSON AND VEHICLE TRIP DISTRIBUTIONS BY PURPOSE AND HOUR
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Set up array to store the hourly and daily person trip distributions for each trip purpose
    hourly.dist.pa <- array(0, c(num.izones, num.izones, num.purp))
    hourly.dist.od <- array(0, c(num.izones, num.izones, num.purp))
    daily.dist.pa <- array(0, c(num.izones, num.izones, num.purp))
    daily.dist.od <- array(0, c(num.izones, num.izones, num.purp))
    dimnames(hourly.dist.pa) <- dimnames(hourly.dist.od) <- dimnames(daily.dist.pa) <- dimnames(daily.dist.od) <- list(zone.names[i], zone.names[i], trip.purp)

    # Set up array to store the hourly vehicle trip distributions
    vehicle.ii.dist <- array(0, c(num.izones, num.izones, 24))
    dimnames(vehicle.ii.dist) <- list(zone.names[i], zone.names[i], 1:24)
    
    # Set up array to store the hourly peak ODs by purp 7-28-06 AB
    # modified on 5-29-13 to add the truck purpose - AB
    if(ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL")),FALSE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL"))=="TRUE")){ 
       peak.ODPu <- array(0, c(num.zones, num.zones, num.purp+4))
       dimnames(peak.ODPu) <- list(zone.names, zone.names, c(trip.purp, "truck", "IE", "EI", "EE"))
    } else {
       peak.ODPu <- array(0, c(num.zones, num.zones, num.purp+3))
       dimnames(peak.ODPu) <- list(zone.names, zone.names, c(trip.purp, "IE", "EI", "EE"))
    }

    # Calculate internal-internal person and vehicle trip distributions for each hour and add to arrays.
    for(hour in 1:24){
        for(purp in trip.purp){
            # Determine distribution for the hour and purpose using the appropriate distribution probabilities and trip generation
            ifelse(isPeak(hour), prob <- pk.prob[[purp]], prob <- offpk.prob[[purp]])
            hourly.dist.pa[,,purp] <- sweep(prob, 1, trip.prod[["regular"]][,hour,purp], "*")
            # Add in the trips to special generators
            if(exists("spec.gen")) {
                for(gen in specials) { 
                    if(spec.gen[gen, "type"]==purp) {
                        zone <- spec.gen[gen, "zone"]
                        if (is.matrix(trip.prod[["special"]])) {
                        	hourly.dist.pa[,zone,purp] <- hourly.dist.pa[,zone,purp] + trip.prod[["special"]][,hour]
			                  } else {
                        	hourly.dist.pa[,zone,purp] <- hourly.dist.pa[,zone,purp] + trip.prod[["special"]][,hour,gen]
                        }
                    }
                    
                }
            }
            # Apply directional factors to make OD table from PA table
            pa <- paste(purp, "pa", sep="")  # variable to identify the appropriate pa column of directional table, e.g. "hbwpa"
            ap <- paste(purp, "ap", sep="")  # variable to identify the appropriate ap column of directional table, e.g. "hbwpa"
            hourly.dist.od[,,purp] <- (hourly.dist.pa[,,purp] * directional[hour,pa]) + t(hourly.dist.pa[,,purp]) * directional[hour,ap]
            # Calculate added vehicles by multiplying by average occupancy rate for the trip purpose
            vehicle.ii.dist[,,hour] <- vehicle.ii.dist[,,hour] + hourly.dist.od[,,purp] / occ[purp]
            # 7-28-06 extra add in to do multi class assignment
            if(hour == pkHr){
               peak.ODPu[zone.names[i],zone.names[i],purp] <- hourly.dist.od[,,purp] / occ[purp]
            }
            # Add the hourly trips for the purpose to the daily trips to build up a daily sum
            daily.dist.pa[,,purp] <- daily.dist.pa[,,purp] + hourly.dist.pa[,,purp]
            daily.dist.od[,,purp] <- daily.dist.od[,,purp] + hourly.dist.od[,,purp]
        }
    }

   # extra section for reporting the effectiveness of district.bias
   dist.bias.Out <- round(apply(tapply(sweep(daily.dist.pa,3,occ,"/"),  as.list(expand.grid(taz.data$district,taz.data$district,trip.purp)), sum),c(2,3),sum),0)
   dist.bias.Out2 <- round(2*apply(tapply(sweep(daily.dist.od,3,occ,"/"),  as.list(expand.grid(taz.data$district,taz.data$district,trip.purp)), sum),c(2,3),sum),0)

# External model
#===============

# additional check to see if the SWIM external model should run for the visitor model, if present
visitorSWIMcheck <- TRUE
if(exists("visitorModel")){
   if(visitorModel==T){
      visitorSWIMcheck <- ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "V_SWIMEXTERNALMODEL")),TRUE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "V_SWIMEXTERNALMODEL"))=="TRUE")
}}

# Use the SWIM external model dataset, if Flagged
   if(ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL")),FALSE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL"))=="TRUE") & visitorSWIMcheck){
      #get OD array 
      load(paste(dataDir,"/externalOD_ZnZnTdMd.RData",sep=""))
      
      # create the OSUM objects needed in Trip Dist
      vehicle.ie.daily.od <- apply(ext.ZnZnTdMd[zone.names[i], zone.names[e],"daily",],1:2,sum)
      vehicle.ei.daily.od <- apply(ext.ZnZnTdMd[zone.names[e], zone.names[i],"daily",],1:2,sum)
      
      # create matrix sets by hour
      vehicle.ee.dist <-  apply(ext.ZnZnTdMd[zone.names[e], zone.names[e],,],1:3,sum)
      vehicle.ei.dist <-  apply(ext.ZnZnTdMd[zone.names[e], zone.names[i],,],1:3,sum)
      vehicle.ie.dist <-  apply(ext.ZnZnTdMd[zone.names[i], zone.names[e],,],1:3,sum) 
      
      # Legacy output - does not really apply to the SWIM model, but easier to just produce the output then to try and rework the code to ignore.
      ie.dest.prob <- size.vars[,"hh"]/sum(size.vars[,"hh"])     
      ie.prod <- ie.dest.prob * sum(ext.traffic[,"adt"] * ext.traffic[,"ie.pct"])*ext.occ  

   } else {
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CALCULATE EXTERNAL-EXTERNAL VEHICLE TRIP DISTRIBUTIONS BY HOUR
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Set up array to store the ee distributions
    vehicle.ee.dist <- array(0, c(num.ezones, num.ezones, 24))
    dimnames(vehicle.ee.dist) <- list(zone.names[e], zone.names[e], 1:24)

#   Calculate external-external vehicle trip distribution by hour
    # Develop daily distribution by iterative proportional fitting
    vehicle.ee.daily.dist <- ipf(ext.traffic[,"adt"]*ext.traffic[,"ee.pct"]/2, ext.traffic[,"adt"]*ext.traffic[,"ee.pct"]/2, ext.seed) 
    vehicle.ee.daily.dist <- vehicle.ee.daily.dist/2+t(vehicle.ee.daily.dist)/2
    # Apply diurnal factors by destinations to 1/2 of daily EE O-D matrix
    # Apply diurnal factors by origins to 1/2 of daily EE O-D matrix
    # Add the two together to average the diurnal factor by origin and destination
    for(hour in 1:24){
        o.factor <- matrix(unlist(ext.diurnal[hour,]),ncol(ext.diurnal),ncol(ext.diurnal), byrow=T) # Fill by Row
        d.factor <- matrix(unlist(ext.diurnal[hour,]),ncol(ext.diurnal),ncol(ext.diurnal), byrow=F) # Fill by Column
    	vehicle.ee.dist[,,hour] <- o.factor*vehicle.ee.daily.dist/2 + d.factor*vehicle.ee.daily.dist/2
    }


######### External - Internal ######### 

#   Set up matrix to store the ei special generator and total daily vehicle distribution in origin-destination form
    spec.gen.ei.daily.od <- array(0, c(num.ezones, num.izones))
    dimnames(spec.gen.ei.daily.od) <- list(zone.names[e], zone.names[i])
    
#   Tabulate the trips associated with external trips to special generators
    if(exists("spec.gen")){        
    
        #   Calculate the number of ei trips to special generators
        spec.ei.trips <- spec.gen[,"trips"] * (1 - spec.gen[,"int.pct"])
        names(spec.ei.trips) <- rownames(spec.gen) 
                
        #   Establish origin probabilities based on ei traffic volumes
        spec.ext.orig.prob <- (ext.traffic[,"adt"] * ext.traffic[,"ei.pct"]) / sum(ext.traffic[,"adt"] * ext.traffic[,"ei.pct"])
        
        #   Fill in vehicle distribution matrix with ei trips to special generators
        spec.gen.ei.daily.od[,spec.gen[, "zone"]] <- outer(spec.ext.orig.prob, spec.ei.trips)        
    }

#   Calculate distribution equation for remaining ei trips according to the proportion of total employment, 
#   not counting special generator employment, in each zone
    ei.attrac <- size.vars[,"retl.emp"] + size.vars[,"serv.emp"] + size.vars[,"othr.emp"] + district.bias[["ext"]][as.character(taz.data$district)]
    ei.dest.prob <- ei.attrac/sum(ei.attrac)
  
    other.ei.trips <- ext.traffic[,"adt"] * ext.traffic[,"ei.pct"] - apply(spec.gen.ei.daily.od, 1, sum)
    vehicle.ei.daily.pa <- outer(other.ei.trips, ei.dest.prob) + spec.gen.ei.daily.od            
#   Note that matrix is PA form not OD
    

######### Internal - External ######### 

#   Calculate distribution equation for Ps for remaining ie trips according to hhs in each zone,
#   employment for nhb
    ie.dest.prob <- size.vars[,"hh"]/sum(size.vars[,"hh"])     
    vehicle.ie.daily.pa <- outer(ie.dest.prob, ext.traffic[,"adt"] * ext.traffic[,"ie.pct"])
    ie.prod <- ie.dest.prob * sum(ext.traffic[,"adt"] * ext.traffic[,"ie.pct"])*ext.occ
#   Note that matrix is PA form not OD
    
######### Convert EI and IE daily PA matrices to OD form #########

    vehicle.ei.daily.od <- (vehicle.ei.daily.pa + t(vehicle.ie.daily.pa))/2
    vehicle.ie.daily.od <- (vehicle.ie.daily.pa + t(vehicle.ei.daily.pa))/2

######### Distribute by hour and simplify spec gen data for reporting #########

    #   Collapse and save the special generator ei and ie trip tables
    #spec.gen.dist <- spec.gen.ei.daily.od[,spec.gen.ei.daily.od[1,] != 0]
    
    spec.gen.dist.ei <- apply(spec.gen.ei.daily.od,c(1,2),sum)
    spec.gen.dist <- spec.gen.dist.ei[,colSums(spec.gen.dist.ei)!=0]
    
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
    vehicle.ee.dist <<- vehicle.ee.dist
    vehicle.ei.dist <<- vehicle.ei.dist
    vehicle.ie.dist <<- vehicle.ie.dist
    rm(vehicle.ee.dist,vehicle.ei.dist, vehicle.ie.dist) 
    source("rcode\\post_processor.R", local=T)
    } # end of the external model options  
    
    # write out the district bias reports
    dist.bias.Out <- cbind(dist.bias.Out, ext=round(tapply(rowSums(vehicle.ie.daily.od)+colSums(vehicle.ei.daily.od),taz.data$district,sum),0))
    write.csv(dist.bias.Out, paste(attr(Visum, "Path"),"dist_bias_Report_PA.csv", sep="/"))

    dist.bias.Out2 <- cbind(dist.bias.Out2, ext=round(tapply(rowSums(vehicle.ie.daily.od)+colSums(vehicle.ei.daily.od),taz.data$district,sum),0))
    write.csv(dist.bias.Out2, paste(attr(Visum, "Path"),"dist_bias_Report_OD.csv", sep="/"))      
        
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

    # extra step to add the external trips into the peak OD by type array 7-28-06 AB, edited on 5-29-13 for SWIM external - AB
   if(ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "MULTICLASSASSIGNMENT")),FALSE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "MULTICLASSASSIGNMENT"))=="TRUE") | (!exists("ext.ZnZnTdMd"))){  
      peak.ODPu[zone.names[i],zone.names[e],"IE"] <- vehicle.ie.dist[,,pkHr]
      peak.ODPu[zone.names[e],zone.names[i],"EI"] <- vehicle.ei.dist[,,pkHr]
      peak.ODPu[zone.names[e],zone.names[e],"EE"] <- vehicle.ee.dist[,,pkHr]
    } else {         
      peak.ODPu[zone.names[i],zone.names[e],dimnames(ext.ZnZnTdMd)[[4]]] <- ext.ZnZnTdMd[zone.names[i], zone.names[e],pkHr,]
      peak.ODPu[zone.names[e],zone.names[i],dimnames(ext.ZnZnTdMd)[[4]]] <- ext.ZnZnTdMd[zone.names[e], zone.names[i],pkHr,]
      peak.ODPu[zone.names[e],zone.names[e],dimnames(ext.ZnZnTdMd)[[4]]] <- ext.ZnZnTdMd[zone.names[e], zone.names[e],pkHr,]
    }

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CALCULATE SUMMARY STATISTICS FOR INTERNAL-INTERNAL PERSON TRIPS
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Calculate total trips
    est.trips <- apply(daily.dist.pa, 3, sum)
#   Calculate mean travel times
    est.means <- c(hbw=0, hbsch=0, hbshp=0, hbro=0, nhb=0)
    for(purp in trip.purp) est.means[purp] <- sum(daily.dist.pa[,,purp] * pk.time[i,i] / est.trips[purp])
#   Calculate standard deviations from means
    est.sds <- c(hbw=0, hbsch=0, hbshp=0, hbro=0, nhb=0)
    for(purp in trip.purp) est.sds[purp] <- sqrt(sum((pk.time[i,i] - est.means[purp])^2 * daily.dist.pa[,,purp])/est.trips[purp])
#   Make a dataframe holding the summary statistics
    est.stats <- data.frame(cbind(est.trips, est.means, est.sds))


#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   COMBINE ALL OF THE RESULTS INTO A LIST AND RETURN THEM TO THE CALLING OBJECt
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(exists("spec.gen")){
       trip.dist <- list(hourly.vehicles=vehicle.dist, daily.vehicles=vehicle.dist.daily, daily.int.persontrips.pa=daily.dist.pa, daily.int.persontrips.od=daily.dist.od, peak.od.purp=peak.ODPu, ext.spec.gen.trips=spec.gen.dist, ext.ie.prod=ie.prod, summary.int.stats=est.stats)
    } else {
       trip.dist <- list(hourly.vehicles=vehicle.dist, daily.vehicles=vehicle.dist.daily, daily.int.persontrips.pa=daily.dist.pa, daily.int.persontrips.od=daily.dist.od, peak.od.purp=peak.ODPu, ext.ie.prod=ie.prod, summary.int.stats=est.stats)
    }
    trip.dist

# End of calc.trip.dist function
}



# Call the function to produce an array of trip distributions by type and clean up afterwards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
trip.dist <- calc.trip.dist()
rm(calc.trip.dist)   
