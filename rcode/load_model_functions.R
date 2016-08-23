# load_model_functions.R
# Code Base (C) 2.1: 3-7-16 (AB)

# This file loads a number of basic functions to make model operations easier
# Created: 10/24/02, Brian Gregor, brian.j.gregor@odot.state.or.us
# Updated: 02/14/03, Brian Gregor
# Updated: 03/12/03, Ben Stabler, benjamin.stabler@odot.state.or.us
# Updated: 03/31/03, Ben Stabler
# Updated: 05/12/03, Brian Gregor
# Updated: 09/06/05, Alex Bettinardi
# Updated: 04/10/2013, Alex Bettinardi
# Updated: 08/26/2015, Alex Bettinardi
# Updated: 02/11/2016, Alex Bettinardi

# Copyright (C) 2002  Oregon Department of Transportation
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

# CHANGES
# 01/28/03 
#   Changed IPF function to check argument inputs and respond to closure argument for rows and columns
# 02/14/03
#   Added trip distribution probability function
#   Moved isPeak() function and read.emme2() function from other code modules
# 03/12/03
#   Added read.model.inputs function and inputs function 
# 3/31/03 Added ext.traffic.futr
# 5/12/03
#   Added a function to collapse a zone-zone matrix into a district-district matrix
#   Added a function to expand a district-district matrix into a zone-zone matrix
#   Added a function to expand a district vector into a zone vector
#   Added a function to convert a data frame in from-zone, to-zone, values format into a matrix
#   Added a function to adjust travel times using the calibration factors global.time.adj and district.time.adj
#   Modified the distribution probability function to add district bias factors and to adjust travel time based on global and district factors
#   Modified the inputs functions to include formatting of the district.time.adj and district.bias objects
# 9/06/05
#   Changed read.emme2 to work with R 2.1.1
#   Added 3 library calls needed to work with "run_model_Visum.R"
# 04/10/13
#   Updated functions for OSUM V2
# 08/26/15 - Alex Bettinardi
#   Added an option required in Astoria's setup for the user to set the factor applied to the closest zone to determine intrazonal travel time.    
# 02/11/16 - Alex Bettinardi
#   Adding in the option to have terminal time apply to intrazonal trips

# load libraries
if(!any(search()=="package:tcltk")) {
   options(warn=-1)
   wantQ <- library("tcltk", logical.return=T)
   options(warn=0)
}  else {
   wantQ <- T
}

# function to run a server script if it exists, or if it doesn't exist - a local script
serverLocalSource <- function(Server, Local) {
   
   # Code to check Inputs.  Use server version if available, otherwise use traveling copy
   if(file.exists(Server)) {
      # see if the "traveling copy" (Local) needs to be updated
      Old <- paste(readLines(Local), collapse="\n")
      New <- paste(readLines(Server), collapse="\n")
      if(Old != New) cat(paste("\n\nThe Server script at -", Server, "\nis different than the Local script at -", Local, "\nThe Server Script was used in this run\n\nHowever, if you ever run this model application off the server you will not be\nusing the latest scrip.  It is strongley recomended that you take\na minute or two at this time and update your local copy with the server copy\n\nServer:", Server, "\nLocal:",Local, "\n\n"))
      rm(Old, New)       
      source(Server)        
   } else {
      source(Local)      
   }      
} 

# source in VISUM functions
serverLocalSource("\\\\s6000e\\6420only\\Tools\\ModelingTools\\VISUM\\VisumFunctions.R", "rcode\\VisumFunctions.r")
# attach all VISUM functions if not already attachted
if(!exists("vbConv")) attach(VisFun)
# clean up space if needed
if("ref" %in% ls()) rm(ref)

# list to hold all of OSUM functions 
osumFun <- list()

# Function to calculate the sum of the exponential of a vector and make it into a matrix
  osumFun$sumExp <- function(mat){
   result <- matrix(rowSums(exp(mat)),nrow(mat),ncol(mat))
   result
   }

# Function to identify whether is peak period 
   osumFun$isPeak <- function(hour){
   peak <- (hour>7)&(hour<20)
   peak
   }

# collapse.matrix - a function to collapse a zone to zone matrix into a district to district matrix
# The arguments are as follows:
# districts is a vector of districts corresponding to zones
# zone.mat is a zone to zone matrix.
# FUN is the collapsing function such as sum, mean, median, sd, var, or any user defined function. The default is sum.
# Note: This function does not check whether the districts correspond to the order of zones in the matrix..
osumFun$collapse.matrix <- function(districts, zone.mat, FUN=sum){
    tapply(as.vector(zone.mat), as.list(expand.grid(districts,districts)),FUN)
}

# Function to expand a district to district matrix into a zone to zone matrix
# dist.mat is a district to district matrix
# zones is a vector of zone names in ascending order.
# districts is a vector of districts corresponding to zones
# factors is a vector corresponding to zones of proportions for disaggregating districts to zones
osumFun$expand.matrix <- function(dist.mat, zones, districts, factors){
    # If factors is just the number 1, then make a vector of ones the length of districts
    if(factors == 1) factors <- rep(1, length(districts)) else
    # Check that the sum of the factors for each district equals one
    if(any(round(tapply(factors, districts, sum),10)!=1)) stop("The factors for disaggregating trips must sum to one by district.")
    # Translate the dist.mat into a table corresponding to zonal matrix and populate with district to district values
    dist.combo <- expand.grid(districts,districts)
    dist.combo[[1]] <- as.character(dist.combo[[1]]); dist.combo[[2]] <- as.character(dist.combo[[2]]) # Must be character for proper references to dist.mat
    interdistrict.values <- apply(dist.combo, 1, function(x) dist.mat[x[1],x[2]])
    # Make a corresponding dataframe of factors and then multiply to get the interzonal factors
    interzonal.factors <- apply(expand.grid(factors, factors), 1, prod)
    # Multiply the interdistrict.values and the interzonal factors to get the interzonal.values
    interzonal.values <- interdistrict.values * interzonal.factors
    # Convert into a matrix
    zone.matrix <- matrix(interzonal.values, length(zones), length(zones))
    rownames(zone.matrix) <- colnames(zone.matrix) <- zones
    zone.matrix
}

# expand.vector - a function to expand a district vector into a zone vector
# dist.vect is a named vector, the names of which are the names of districts for which there are values
# zones is a vector of zones in ascending order. May be a character or numeric vector. The function makes is character.
# districts is a vector of districts corresponding to the zones vector. May be character or numeric. The function makes it character.
# empty.value is the value to be placed where there is no value for a district in dist.vect.
osumFun$expand.vector <- function(dist.vect, zones, districts, empty.value){
    zone.vector <- dist.vect[as.character(districts)]
    zone.vector[is.na(zone.vector)] <- empty.value
    names(zone.vector) <- as.character(zones)
    zone.vector
}

# flat.to.mat - a function to take a data frame in the form of "from zone", "to zone", "value" into a square matrix
# The first argument is a data frame with three column:
#   first column is the from zone or district name (a character type)
#   second column is the to zone or district name (a character type)
#   third column is the values for the corresponding cells of the matrix.
# The second argument is a vector of names of the districts or zones for the matrix.
# The third argument is the value to be placed in cells for which there is otherwise no value, typically 0 or 1.
osumFun$flat.to.mat <- function(input.frame, dists.or.zones, empty){
    rowcol.names <- as.character(sort(dists.or.zones))
    mat.dim <- length(dists.or.zones)
    out.mat <- matrix(empty, mat.dim, mat.dim)
    rownames(out.mat) <- colnames(out.mat) <- rowcol.names
    if(nrow(input.frame)!=0) apply(input.frame, 1, function(x) out.mat[x[1], x[2]] <<- as.numeric(x[3]))
    out.mat
}

# iterative proportional fitting function
osumFun$ipf <- function(rowcontrol, colcontrol, seed, maxiter=50, closure=0.01){
   # input data checks: sum of marginal totals equal and no zeros in marginal totals
   #if(sum(rowcontrol) != sum(colcontrol)) stop("sum of rowcontrol must equal sum of colcontrol")
   if(any(rowcontrol==0)){
      numzero <- sum(rowcontrol==0)
      rowcontrol[rowcontrol==0] <- 0.001
      warning(paste(numzero, "zeros in rowcontrol argument replaced with 0.001", sep=" "))
      }
   if(any(colcontrol==0)){
      numzero <- sum(colcontrol==0)
      colcontrol[colcontrol==0] <- 0.001
      warning(paste(numzero, "zeros in colcontrol argument replaced with 0.001", sep=" "))
      }
   # set initial values
   result <- seed
   rowcheck <- 1
   colcheck <- 1
   iter <- 0
   # successively proportion rows and columns until closure or iteration criteria are met
   while(((rowcheck > closure) | (colcheck > closure)) & (iter < maxiter))
      {
	 rowtotal <- rowSums(result)
	 rowfactor <- rowcontrol/rowtotal
	 rowfactor[is.infinite(rowfactor)] <- 0
	 result <- sweep(result, 1, rowfactor, "*")
	 coltotal <- colSums(result)
	 colfactor <- colcontrol/coltotal
	 colfactor[is.infinite(colfactor)] <- 0
	 result <- sweep(result, 2, colfactor, "*")
   rowcheck <- sum(abs(1-rowfactor))
	 colcheck <- sum(abs(1-colfactor))
	 iter <- iter + 1
       }
   if(iter == maxiter) warning(paste( "The maximum (", iter, ") number of iterations was reached, the ipf did NOT close", sep=""))  

   result
   }

# Internal-internal trip distribution probability function
osumFun$dist.prob <- function(travtime, purpose, size.c=size.coeff, time.c=time.coeff, size.v=size.vars, global.t=global.time.adj, district.t=district.time.adj, district.b=district.bias){
    sizeterm <- log(rowSums(sweep(size.v, 2, size.c[purpose,], "*")))
    bias <- expand.vector(district.b[[purpose]], taz.data$taz, taz.data$district, 0)
    sizeterm <- sizeterm+log(bias)
    time.adj <- global.t * expand.matrix(district.t, taz.data$taz, taz.data$district, 1)
    travtime <- travtime * time.adj
    time.coeff.vec <- time.c[purpose,]
    timeterm <- time.coeff.vec[1]*travtime + time.coeff.vec[2]*travtime^2 + time.coeff.vec[3]*travtime^3
    util <- sweep(timeterm, 2, sizeterm, "+")
    prob <- exp(util)/sumExp(util)
    prob
}

# Function to plot travel time distribution for trips
osumFun$plot.times <- function(purpose, tm=pk.time, pa=trip.dist[[3]], interval=1) {
   time.cut <- cut(as.vector(tm[rownames(pa),colnames(pa)]), seq(0, max(tm), by=interval))
   interval.names <- sapply(levels(time.cut), function(x) substring(x, 2, nchar(x)-1))
   interval.names <- sapply(interval.names, function(x) sub(",", "-", x))
   dist <- as.vector(pa[,,purpose])
   results <- tapply(dist, time.cut, sum)
   barplot(results, main=paste(city, toupper(purpose), "Trip Time Distribution", sep=" "), names.arg=interval.names, xlab="Minutes", ylab="Number of trips")
}

# function to run a peak assignment and get off.peak and peak travel time matrices
osumFun$save.TT <- function(pkHr, init=F) {

   matrices <- getMatList(Visum)

   if(init){
      setDmdSeg(Visum, aMode, paste("hour", pkHr, sep="_"))
      #setVisumDSegMatrix(Visum, aMode, getVisumODMatrix(Visum, pkHr+1))
   } else {
      setDSegMatrix(Visum, aMode, trip.dist$hourly.vehicles[,,pkHr])
   }
   comInvoke(vbConv("Visum.Procedures"), "Open", paste(getwd(), "ParameterFiles/calcTT.par", sep="/"))
   comInvoke(vbConv("Visum.Procedures"), "Execute")
   
   TT2NEARESTZONEFACTOR <- ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "TT2NEARESTZONEFACTOR")),0.5, comGetProperty(vbConv("Visum.Net"), "AttValue", "TT2NEARESTZONEFACTOR"))
   
  #offpeak period travel times
    offpk.time <- getSkimMatrix(Visum, as.numeric(matrices$SkimMatrices[matrices$SkimMatrices[,"CODE"]=="TT0","NO"]))
    Diag <- apply(offpk.time, 1, function(x) TT2NEARESTZONEFACTOR * min(x[x != 0]))
    offpk.time <- offpk.time + terminalTime
    if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "TT2INTRAZONAL"))){  # AB 2-11-16 New option to allow Terminal time to be added to intrazonal
       if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "TT2INTRAZONAL")) == "TRUE") Diag <- Diag + terminalTime
    }
    diag(offpk.time) <- Diag
  #peak period travel times
    pk.time <- getSkimMatrix(Visum, as.numeric(matrices$SkimMatrices[matrices$SkimMatrices[,"CODE"]=="TTC","NO"]))
    Diag <- apply(pk.time, 1, function(x) TT2NEARESTZONEFACTOR * min(x[x != 0]))
    pk.time <- pk.time + terminalTime
    if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "TT2INTRAZONAL"))){  # AB 2-11-16 New option to allow Terminal time to be added to intrazonal
       if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "TT2INTRAZONAL")) == "TRUE") Diag <- Diag + terminalTime
    }
    diag(pk.time) <- Diag
    rm(Diag)

pk.time <<- round(pk.time,4)
offpk.time <<- round(offpk.time,4)
   
} # end of save.TT

#Define function to calculate root mean square error
#---------------------------------------------------

#:Name: calcRmse
#:Description: This function calculates the root mean square error of a series of observed and estimated values.
#:Argument: obs - a vector of observed values (e.g. traffic counts)
#:Argument: est - a vector of estimated values corresponding to the observed values (e.g. model assignments)
#:Argument: pct - a logical identifying whether percentages should be returned (TRUE) or decimal values (FALSE)
#:Return: a scalar value of the root mean square error for the vector inputs

osumFun$calcRmse <- function(obs, est, pct=TRUE){
          if(length(obs) != length(est)) stop("Observed and estimated data are unequal length")
          if(any(is.na(obs)) | any(is.na(est))) stop("Remove NA values from data")
          SqError <- (obs - est)^2
          MeanSqError <- sum(SqError) / length(SqError)
          ObsMean <- sum(obs) / length(obs)
          Result <- sqrt(MeanSqError) / ObsMean
          if(pct == TRUE) Result <- Result * 100
          Result
     }

# attacht the OSUM functions
if(!exists("calcRmse")) attach(osumFun)           
 