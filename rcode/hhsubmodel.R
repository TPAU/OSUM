# hhsubmodel.R
# Code Base (C) 2.1: 3-7-16 (AB)

# This script is part of the Oregon small urban area R model.
# It produces household data needed for trip generation.
#
# Created: 10/24/02 Brian Gregor brian.j.gregor@odot.state.or.us
# Updated: 11/12/02 Ben Stabler benjamin.stabler@odot.state.or.us
# Updated: 03/12/03 Ben Stabler
# Updated: 04/2/13 Alex Bettinardi
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
# History
# This script is an alteration of the hhsubmodel.R script.  
# Changes include using Census data on household size and 
# income directly, rather than using regression equations to develop the distributions. 
# Added worker probability calculation
# Rearranged the script to list function definitions at the top and calls at the bottom
# Added a test to use CTPP workers per HH input object instead of the worker probability logit method
# In April 2013, this script was updated to work with OSUM V2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION DEFINITIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define function to calculate worker probabilities
# *************************************************

calc.wkr.prob <- function() {
   
   # Create Dummy Income Size Profile Table
   work.dist<-matrix(NA,16,6)
   colnames(work.dist) <- c("hhsize", "income1", "income2", "income3", "income4", "profile") 
   # Input Income Size Profile Vectors
   work.dist[,1] <- rep(1:4, each=4)
   work.dist[,2] <- rep(c(1,0,0,0),4)
   work.dist[,3] <- rep(c(0,1,0,0),4)
   work.dist[,4] <- rep(c(0,0,1,0),4)
   work.dist[,5] <- rep(c(0,0,0,1),4)
   work.dist[,6] <- c(11,12,13,14,21,22,23,24,31,32,33,34,41,42,43,44)
   
   # Create Dummy Utility Table
   util<-matrix(NA,16,4)
   colnames(util) <- c("util-wk0", "util-wk1", "util-wk2", "util-wk3+")
   # Calculate utilities for 0,1,2,3+ workers
   util[,1]<- rep(0,16)  # zero workers utilities (base utility)
   util[,2]<- -1.462+0.6092*work.dist[,"hhsize"]+0.5369*work.dist[,"income3"]+0.6864*work.dist[,"income4"]
   util[,3]<- -4.488+1.323*work.dist[,"hhsize"]+0.6838*work.dist[,"income2"]+1.464*work.dist[,"income3"]+2.458*work.dist[,"income4"]
   util[,4]<- -10.09+2.4898*work.dist[,"hhsize"]+1.254*work.dist[,"income3"]+2.448*work.dist[,"income4"]
   
   # Calculate 0,1,2,3+ worker probabilities
   work.dist.prob <- exp(util)/rowSums(exp(util))
   colnames(work.dist.prob) <- paste("wkr-prob",0:3,sep="")
   work.dist.prob <- cbind(profile=work.dist[,6],work.dist.prob)
   
   # Create a dummy 4x4x4 array
   work.dist.temp <- array(NA, c(4,4,4))
   # Map the work.dist.prob table to the dummy array
   work.dist.temp[,,1] <- work.dist.prob[1:4, 2:5]
   work.dist.temp[,,2] <- work.dist.prob[5:8, 2:5]
   work.dist.temp[,,3] <- work.dist.prob[9:12, 2:5]
   work.dist.temp[,,4] <- work.dist.prob[13:16, 2:5]
   
   # For the profile Size1-Inc1 add Prob for 2 & 3 to 1worker prob and set prob of 2 & 3+ to Zero
   work.dist.temp[,2,1] <- apply(work.dist.temp[,2:4,1],1,sum)
   work.dist.temp[,3:4,1] <- 0
   # Repeat the Above process to Size2-Inc2 but only on Prob 3+ workers
   work.dist.temp[,3,2] <- apply(work.dist.temp[,3:4,2],1,sum)
   work.dist.temp[,4,2]<- 0
   
   # Copy transposed work.dist.temp groups to wrk.prof.fin
   wrk.prob.fin <- apply(work.dist.temp,c(1,3), function(x) t(x))
   dimnames(wrk.prob.fin) <- list(paste("wrk", 0:3, sep=""),paste("inc", 1:4, sep=""),paste("size", 1:4, sep=""))
   wrk.prob.fin
   
}


# Define function for calculating joint size and income distribution
# ******************************************************************

calc.size.by.inc <- function (seed, size, income) {
   # The first argument is the distribution of size and income for the region.
   # This is the seed matrix for an iterative proportional fit (ipf) process for each zone.
   # The second argument is a matrix of household size distribution by zone.
   # This provides the row totals for the ipf.
   # The third argument is a matrix of household income distribution by zone.
   # This provides the column total for the ipf.
   
      # Set up an array to hold the size-income distribution for each zone
      numzones <- dim(size)[1]
      size.income <- array(1, c(4, 4, numzones))
      dimnames(size.income) <- list(rownames(seed), colnames(seed), taz.data$taz)
      # Do an ipf for each zone
      for(i in 1:numzones)
      {  
      	if(sum(size[i,])==0) {  size.income[,,i] <- size.income[,,i]*0  }
      	else {  
          # need to ensure that the totals of col and row control are equal
          # rounding can cause them to be different by a few percent which causes issues.
          rowcontrol <- size[i,] * mean(c(sum(size[i,]), sum(income[i,]))) / sum(size[i,])
          colcontrol <- income[i,] * mean(c(sum(size[i,]), sum(income[i,]))) / sum(income[i,])
          size.income[,,i] <- ipf(rowcontrol,colcontrol,seed) 
        } 
      }
      # Return the completed array.
      size.income
}

# Define Function for calculating joint household size and number of workers distribution
# ***************************************************************************************

calc.size.by.wkr <- function (size.inc, wkr.prob) {
   # This function computes a joint distribution of household size and number of workers by zone.
   # The first argument is a joint distribution of household size and income.
   # The second argument is an array of probabilities of household workers vs. household income and size.
   # In this table, workers are the first dimension, income the second, and size the third.
   
      # Set up an array to hold the results
      numzones <- dim(size.inc)[3]
      size.wkr <- array(1, c(4, 4, numzones))
      dimnames(size.wkr) <- list(rownames(size.inc), rownames(wkr.prob), dimnames(size.inc)[[3]])
      # Iterate through each zone
      for(i in 1:numzones)
      {
         # Calculate number of households by number of workers for households in each size category
         x1 <- apply(sweep(wkr.prob[,,1], 2, size.inc[1,,i],"*"),1,sum)
         x2 <- apply(sweep(wkr.prob[,,2], 2, size.inc[2,,i],"*"),1,sum)
         x3 <- apply(sweep(wkr.prob[,,3], 2, size.inc[3,,i],"*"),1,sum)
         x4 <- apply(sweep(wkr.prob[,,4], 2, size.inc[4,,i],"*"),1,sum)
         # Combine the resulting vectors and transpose so that size is the first dimension and workers the second
         size.wkr[,,i] <- t(cbind(x1, x2, x3, x4))
      }
      # Return the resulting array.
      size.wkr
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Call functions to calculate worker probabilities for household submodel
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create objects for household size distribution and household income distribution
hh.size.dist <- as.matrix(sweep(taz.data[,grep("^hhs",names(taz.data))], 1, taz.data$hhbase, "*")) 
colnames(hh.size.dist) <- c("1person", "2persons", "3persons", "4+persons")
hh.inc.dist <- as.matrix(sweep(taz.data[,grep("^hhi",names(taz.data))], 1, taz.data$hhbase, "*"))
colnames(hh.inc.dist) <- c("income1", "income2", "income3", "income4")

if(exists("newhhs")){
   hh.size.dist<-newhhs
   rm(newhhs)
}

# Put very small values in cells with zeros, except where all are zero in a row to get ipf closure
hh.size.dist[hh.size.dist==0] <- 0.00001
hh.size.dist[rowSums(hh.size.dist)==0.00004,] <- 0
hh.inc.dist[hh.inc.dist==0] <- 0.00001
hh.inc.dist[rowSums(hh.inc.dist)==0.00004,] <- 0

# Check for exixting workers per HH object (built from 1990 CTPP).
# Otherwise, use the estimated values from the hh submodel

if ( exists("workershh") ) { 
	#Multiply the number of workers per HH size percents by the total number of HHs in each zone
      cPos <- c(grep("0",names(workershh)), grep("1",names(workershh)), grep("2",names(workershh)), grep("3",names(workershh)))
      if(length(cPos) != 16) stop("workershh.csv does not have the proper column structure.  Needs 4 workers columns")
      if(length(grep("4", names(workershh))) > 0) workershh[,grep("3", names(workershh))] <- workershh[,grep("3", names(workershh))] + workershh[,grep("4", names(workershh))]
      if(length(grep("otal", names(workershh))) > 0) {
         workershh[,cPos] <- workershh[,cPos]*rowSums(workershh[,grep("otal", names(workershh))])/rowSums(workershh[,cPos])
      }
      blkGrp <- as.character(as.numeric(unlist(lapply(strsplit(rownames(workershh)," "),function(x) x[2])))*1000)
      workersHH.Zn <- workershh[match(taz.data$bgbase,blkGrp),cPos] / rowSums(workershh[match(taz.data$bgbase,blkGrp),cPos])
      rownames(workersHH.Zn) <- taz.data$taz


      size.by.wkr <- apply(workersHH.Zn, 2, function(x) x*taz.data$hhbase)

	
	#Create an array of HH size by number of workers per HH by TAZ
	size.by.wkr <- array(unlist(t(size.by.wkr)),c(4,4,nrow(size.by.wkr)))
	dimnames(size.by.wkr) <- list(c("hh1","hh2","hh3","hh4"),c("wrk0","wrk1","wrk2","wrk3"), taz.data$taz)
  rm(cPos, blkGrp, workershh, workersHH.Zn)
} else { 

	# Call function to calculate worker probabilities and assign to wkr.prob
	wkr.prob <- calc.wkr.prob()
	
	# Call the calc.size.by.inc function to produce a joint distribution of households 
	# by size and income for each zone "size.by.inc"
	size.by.inc <- calc.size.by.inc(size.inc.seed, hh.size.dist, hh.inc.dist)
	rm(size.inc.seed)
	
	# Call the calc.size.by.wkr function to produce a joint distribution of households 
	# by size and workers for each zone "size.by.wkr"
	size.by.wkr <- calc.size.by.wkr(size.by.inc, wkr.prob)
	rm(wkr.prob, size.by.inc)
	
}

# clean up one time functions
rm(calc.wkr.prob, calc.size.by.inc, calc.size.by.wkr)

# Regardless of the method for calculating workers per HH, create a table of the 
# distribution of number of households by number of workers for each zone 
hh.wkr.dist <- t(apply(size.by.wkr, c(2,3), sum))



