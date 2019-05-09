# trip_generation_v5.R
# Code Base (C) 2.6: 1-11-18 (AB)

# This program calculates trip generation for small urban areas Oregon by hour of the day
# 
# Created: 06/26/02 Brian Gregor brian.j.gregor@odot.state.or.us
# Updated: 11/14/02 Ben Stabler benjamin.stabler@odot.state.or.us
# Updated: 02/14/03 Brian Gregor
# Updated: 02/19/03 Brian Gregor
# Updated: 05/06/03 Brian Gregor
# Updated: 09/02/03 Ben Stabler
# Updated: 02/14/13 Alex Bettinardi
# Updated: 08/26/15 Alex Bettinardi
# Updated: 01/11/18 Alex Bettinardi 

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

# Changes
#   02/14/03
#       Added code to incorporate trips to special generators and allocate the production ends
#       Changed code for calculating underreported trips.  Trips are adjusted only if they are less than the target value.
#   02/19/03
#       Corrected errors in trip to allocate test. line # 185
#   05/06/03
#       Production probability for special generator trips calculated from productions of corresponding types of trips
#       This replaced old code which calculated probabilities for all special generators based on percentage of households
#   9/2/03 
#       Changed reference to ext.traffic since ie.pct and ei.pct must the be same and are really just (1-ee.pct)/2
#       Also fixed colnames reference to spec.gen for case where there is only one special generator.
#   2/14/13
#       Revised for OSUM V2  
#   8/26/2015
#       Added a special Astoria handeling of NHB trips when special generators are used
#   1/11/2018
#       Corrected error in spec gen external traffic error check (dataed in code below)

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   PURPOSE
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   This trip generation function calculates the number of trips by zone, hour and trip purpose.
#   It does this for regular trips and for trips to special generators.
#   Trips to special generators are allocated to household productions.
#   It adjusts trips to account for the effects of underreporting on trip rates in the joint model specification.
#   It also takes the non-home based productions and allocates them to attractions.
#   The function returns a list object.
#   The first element of the list object is an array of regular trips by zone (rows), hour (columns) and purpose (tables).
#   The second element of the list object is an array of special generator trips by zone (rows), hour (columns) and special generator (tables).


#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   DATA OBJECTS THE FUNCTION USES
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   The following objects are always used and must exist in the workspace:
#   ----------------------------------------------------------------------
#   "taz.data" is a dataframe.  Each row contains the socioeconomic data for each model zone.
#   "hh.wkr.dist" is a matrix of households by zone (rows) and number of workers in the household (columns).
#   "hh.size.dist" is a matrix of households by zone (rows) and number of persons in the household (columns).
#   "size.by.wkr" is a 3D array of households by number of persons (dim 1: rows), number of workers (dim 2: columns), and zone (dim 3: tables)
#   "hbw.rate" is a vector of hbw trip rates by number of workers in the household.
#   "hbsch.rate" is a vector of hbsch trip rates by household size.
#   "hbshp.rate" is a matrix of hbshp trip rates by household size (rows) and number of workers (columns).
#   "hbro.rate" is a matrix of hbro trip rates by household size (rows) and number of workers (columns).
#   "scenario" is a string scalar indicating whether the scenario is "base" or "future" used to select appropriate data.


#   The following objects are used if they are in the workspace and are needed:
#   --------------------------------------------------------------------------
#   "spec.gen" is a matrix of data on special generators.  Columns are special generators.  Rows are the data fields.
#   "ext.occ" is a scalar value of average vehicle occupancy for external trips.


#   Start the function
calc.trip.gen <- function()
{


#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   COMMON VALUES
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    zone.names <- taz.data[,"taz"]
    num.zones <- length(zone.names)
    trip.purp <- c("hbw", "hbsch", "hbshp", "hbro", "nhb")
    num.purp <- length(trip.purp)
    if(exists("spec.gen")) specials <- rownames(spec.gen)

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CALCULATE INITIAL INTERNAL TRIPS
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Calculate initial values of trips for each trip purpose using the input trip rates.
#   The number of hbshp, hbro, and nhb trips are expanded later in the function to account for underreporting
#   The outputs for each of these trip purposes is a matrix of trips by zone (rows) and household size (columns)
#   This is done because underreporting varies by household size.
#   The hbw and hbsch trips are not expanded, so they are output as vectors of trips by zone
#   Attractions at special generators are converted into productions by zone.
#   Internal-external trips are also calculated.

#   Calculate hbw trips.  Rates vary with number of workers in the household.
#   The hbw rates are applied to hh.wkr.dist and summed by zone and multiplied by percent trips retained in the model area.
    hbw.trips <- rowSums(sweep(hh.wkr.dist, 2, hbw.rate, "*"))*retention["hbw"]

#   Calculate hbsch trips.  Rates vary with the number of persons in the household.
#   The hbsch rates are applied to hh.size.dist and summed by zone and multiplied by percent trips retained in the model area.
    hbsch.trips <- rowSums(sweep(hh.size.dist, 2, hbsch.rate, "*"))*retention["hbsch"]

#   Calculate hbshp trips.  Rates vary with the number of persons and workers in the household.
#   The hbshp rates are applied to size.by.wkr and summed to produce a matrix of trips by zone (rows) and household size (columns).
#   These are multiplied by the percent of trips retained in the model area
    hbshp.trips <- t(apply(sweep(size.by.wkr, c(1,2), hbshp.rate, "*"), c(1,3), sum))*retention["hbshp"]  # c(1,3) sums across the 2nd dimension (workers)

#   Calculate hbro trips.  Rates vary with the number of persons and workers in the household.
#   The hbro rates are applied to size.by.wkr and summed to produce a matrix of trips by zone (rows) and household size (columns).
#   These are multiplied by the percent of trips retained in the model area
    hbro.trips <- t(apply(sweep(size.by.wkr, c(1,2), hbro.rate, "*"), c(1,3), sum))*retention["hbro"]

#   Calculate nhb trips.  Rates vary with the number of persons and workers in the household.
#   The nhb rates are applied to size.by.wkr and summed to produce a matrix of trips by zone (rows) and household size (columns).
#   These are multiplied by the percent of trips retained in the model area
    nhb.trips <- t(apply(sweep(size.by.wkr, c(1,2), nhb.rate, "*"), c(1,3), sum))*retention["nhb"]

#   Sum up all the internal trips
    ii.trips.init <- cbind(hbw=hbw.trips, hbsch=hbsch.trips, hbshp=rowSums(hbshp.trips), hbro=rowSums(hbro.trips), nhb=rowSums(nhb.trips))
    tot.ii.trips.init <- sum(ii.trips.init) 
       
#   Calculate productions for special generator trips
    if(exists("spec.gen")){
        ii.prob <- apply(ii.trips.init, 2, function(x) x/sum(x)) # matrix of production probabilities by trip purpose
        spec.ii.prob <- ii.prob[,spec.gen$type] # extract matrix associating probabilities corresponding to each special generator
        if(!is.vector(spec.ii.prob)) { colnames(spec.ii.prob) <- rownames(spec.gen) }
        spec.ii.trips <- spec.gen$trips * spec.gen$int.pct
        spec.ii.trips <- spec.ii.trips * occ[spec.gen$type] # convert vehicle trips to person trips
        # multiply the production probabilities by the special generator trips to get generation by zone
        if(is.vector(spec.ii.prob)) { 
        	spec.ii.prod <- spec.ii.prob * spec.ii.trips 
         } else { 
         	spec.ii.prod <- sweep(spec.ii.prob, 2, spec.ii.trips, "*") 
         	rownames(spec.ii.prod) <- zone.names
        	colnames(spec.ii.prod) <- rownames(spec.gen)
         }
        tot.spec.gen.trips <- sum(spec.ii.trips)
    
        # Quick check that the trips are available
        spec.ei.tot <- sum(spec.gen$trips)-sum(spec.gen$trips * spec.gen$int.pct) # 1-11-18 AB - modified this line to take out the impact of occ from above.
        ei.veh <- sum(ext.traffic[,"adt"] * ext.traffic[,"ei.pct"])
        if(spec.ei.tot > ei.veh){
           stop(paste("\nSpecial Generator External Trips have exceeded EI trips available\nSpec Gen Ext Trips:", spec.ei.tot, "  EI Trips", ei.veh))  
        } else {
          if(spec.ei.tot > 0.5*ei.veh){
           cat(paste("\nWarning:\n", round(100*spec.ei.tot/ei.veh), "% of EI Trips will be Special Generator External Trips\nSpec Gen Ext Trips:", spec.ei.tot, "  EI Trips", ei.veh, "\n"))  
          }
        }         
    }
    if(!exists("spec.gen")) tot.spec.gen.trips <- 0
    
#   Calculate total number of internal-external vehicle trips and convert to person trips
    ie.veh <- sum(ext.traffic[,"adt"] * ext.traffic[,"ie.pct"])
    tot.ie.trips <- ext.occ * ie.veh 

#   Calculate target number of trips generated by internal households.
#   The hh.trip.rate object declared in run_model.R is multiplied by the total number of base or future households.
    tot.ii.ie.trips <- hh.trip.rate * sum(taz.data$hhbase)
    
#   Calculate the number of missing trips.  These will be allocated to (hbshp, hbro and nhb)
    trips.to.allocate <- tot.ii.ie.trips - sum(tot.ii.trips.init, tot.spec.gen.trips, tot.ie.trips)

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ADJUST TRIPS TO ACCOUNT FOR UNDERREPORTING
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   THE FOLLOWING IS DONE ONLY IF THERE ARE MISSING TRIPS (e.g. "trips.to.allocate" is greater than zero)
#   Calculate adjustments to the number of trips calculated using the trip rates
#   A target number of internal-internal and internal-external trips is calculated from an average rate per household
#   This average rate is based on estimates determined from several model studies
#   The difference between the target number of trips and the number estimated from trip rates is apportioned
#   between hbshp.trips, hbro.trips, and nhb.trips.
#   Hassounah, et.al., have found that very little underreporting occurs for the hbw and hbsch trips (TRR 1412, pp.90-93)

    if(trips.to.allocate > 0){
        #   Vector of propensities for underreporting by household size adapted from Hassounah, et.al.
        report.fact <- c("1person"=1, "2persons"=1.4, "3persons"=2, "4+persons"=3)
        
        #   Allocate trips among households by size
        #   First create a matrix of trips by purpose (rows) and household size (columns).
        est.trips.by.size <- rbind("hbshp"=colSums(hbshp.trips), "hbro"=colSums(hbro.trips), "nhb"=colSums(nhb.trips))
        weight.trips.by.size <- report.fact * apply(est.trips.by.size, 2, sum)  # Multiply sums of trips by size by underreporting factors.
        pct.trips.by.size <- weight.trips.by.size/sum(weight.trips.by.size)  # Calculate proportions of total in each cell.
        alloc.trips.by.size <- trips.to.allocate * pct.trips.by.size  # Allocate underreported trips according to these proportions.
        
        #   Allocate trips among trip purposes (hbshp, hbro, nhb) for each household size
        #   First calculate the proportions of trips in each household size category by purpose.
        pct.trips.by.purpose <- sweep(est.trips.by.size, 2, colSums(est.trips.by.size), "/")
        #   Then multiply these proportions by the underreported allocations by size
        alloc.trips.by.purpose <- sweep(pct.trips.by.purpose, 2, alloc.trips.by.size, "*")
        
        #   Allocate trips among zones for each household size and purpose
        #   For each trip purpose, first calculate the proportions of trips in each household size category by zone.
        pct.hbshp.by.zone <- sweep(hbshp.trips, 2, colSums(hbshp.trips), "/")
        pct.hbro.by.zone <- sweep(hbro.trips, 2, colSums(hbro.trips), "/")
        pct.nhb.by.zone <- sweep(nhb.trips, 2, colSums(nhb.trips), "/")
        #   Then multiply these proportions by the underreported allocations by purpose and size.
        hbshp.trips.add <- sweep(pct.hbshp.by.zone, 2, alloc.trips.by.purpose["hbshp",], "*")
        hbro.trips.add <- sweep(pct.hbro.by.zone, 2, alloc.trips.by.purpose["hbro",], "*")
        nhb.trips.add <- sweep(pct.nhb.by.zone, 2, alloc.trips.by.purpose["nhb",], "*")
        #   Then sum the initial and added trips across the size categories to get total trips by zone.
        hbshp.trips <- rowSums(hbshp.trips.add) + rowSums(hbshp.trips)
        hbro.trips <- rowSums(hbro.trips.add) + rowSums(hbro.trips)
        nhb.trips <- rowSums(nhb.trips.add) + rowSums(nhb.trips)
    }

    if(!(trips.to.allocate > 0)){
        hbshp.trips <- rowSums(hbshp.trips)
        hbro.trips <- rowSums(hbro.trips)
        nhb.trips <- rowSums(nhb.trips)
    }

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ALLOCATE NON-HOME BASED TRIPS TO ATTRACTION END
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Although non-home based trips are produced by households (mostly) they do not occur mostly at households
#   The location of these trip productions is allocated to businesses etc. using a multinomial logit formulation

#   Calculate total number of nhb trips
    total.nhb.trips <- sum(nhb.trips)   

#   Calculate the utilities for trip allocation
    util <- log(taz.data$retlbase + 0.90712*taz.data$servbase + 0.07472*taz.data$hhbase)

#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Adding a special Astoria handeling of NHB trips when special generators are used
   if(ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "ASTORIA_NHB_PROD")),FALSE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "ASTORIA_NHB_PROD"))=="TRUE")){

    size.vars <- cbind(retl.emp=taz.data$retlbase, serv.emp=taz.data$servbase, hh=taz.data$hhbase)
    rownames(size.vars) <- taz.data$taz
    #   Subtract the portions of the size variables attributable to the special generators since these are distributed separately
    if(exists("spec.gen")){
        for(gen in specials){
            spec.zone <- spec.gen[gen, "zone"]
            size.adj <- unlist(spec.gen[gen, c("retl.emp", "serv.emp", "hh")])
            size.vars[spec.zone,] <- size.vars[spec.zone,] - size.adj
            if(any(size.vars[spec.zone,]<0)) stop(paste("One of the size variables for the", gen, "special generator is miscoded. Check input data.", sep=" "))
        }
    }

#   Calculate total number of nhb trips
    total.nhb.trips <- sum(nhb.trips)   

#   Calculate the utilities for trip allocation
    nhbCoeff <- c(1, 0.90712, 0.07472)
    util <- log(rowSums(sweep(size.vars, 2, nhbCoeff, "*")))
    }
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
#   Calculate the probability from the utilities
    nhb.prob <- exp(util)/sum(exp(util))

#   Allocate total nonhome-based trips according to the probabilities.
    nhb.trips <- nhb.prob*total.nhb.trips


#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   APPLY DIURNAL FACTORS TO TRIP PRODUCTIONS AND SAVE THE RESULTING ARRAYS IN A LIST
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Create an 3D array of regular trips by zone (rows), hour (columns), and purpose (table)
    daily <- cbind(hbw=hbw.trips, hbsch=hbsch.trips, hbshp=hbshp.trips, hbro=hbro.trips, nhb=nhb.trips)
    reg.trips <- array(0, c(num.zones, 24, num.purp))  # initialize the array
    dimnames(reg.trips) <- list(zone.names, 1:24, trip.purp)  # name the dimensions
    for(purp in trip.purp) reg.trips[,,purp] <- outer(daily[,purp], diurnal[,purp])

#   Create a 3D array of special generator trips by zone (rows), hour (columns), and purpose (table)
    if(exists("spec.gen")) {
        spec.trips <- array(0, c(num.zones, 24, nrow(spec.gen)))  # initialize the array
        dimnames(spec.trips) <- list(zone.names, 1:24, rownames(spec.gen))
        
        if(is.vector(spec.ii.prod))  { 
        	spec.trips <- outer(spec.ii.prod, diurnal[,spec.gen[specials, "type"]])
        } else  {
        	for(gen in specials) {
            		spec.trips[,,gen] <- outer(spec.ii.prod[,gen], diurnal[,spec.gen[gen, "type"]])
        	}
        }
    }
    
#   Combine the 3D arrays for regular trips ("reg.trips") and special generator trips ("spec.trips") into a list ("trips")
#   Also add a vector of expected number of trips, total trips to allocate and total ii and ie trips
#   This is the return value of the function.
    trip.data <- c(target.trips=tot.ii.ie.trips, trips.to.allocate=trips.to.allocate, tot.ii.trips.init=tot.ii.trips.init, tot.spec.gen.trips=tot.spec.gen.trips, tot.ie.trips=tot.ie.trips)
    if(exists("spec.gen")) trips <- list(regular = reg.trips, special = spec.trips, trip.data=trip.data)
    if(!exists("spec.gen")) trips <- list(regular = reg.trips, special = 0, trip.data=trip.data)
    trips
    
#   End of calc.trip.gen function
}


#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CALL THE FUNCTION TO PRODUCE A TABLE OF TRIP PRODUCTIONS BY TYPE AND CLEAN UP AFTERWARDS
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   trip.prod <- calc.trip.gen()
   rm(calc.trip.gen)
