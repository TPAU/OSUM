# future_hhs.R
# Code Base (C) 2.2: 4-13-16 (AB)

# This program calculates the number of future HHs by TAZ for the Oregon small urban area model 
#
# Created: 5/14/02 Brian Gregor brian.j.gregor@odot.state.or.us 
# Updated: 12/18/02 Ben Stabler benjamin.stabler@odot.state.or.us
# Updated: 7/21/04  Ben Stabler benjamin.stabler@odot.state.or.us 
#    Added future hh or pop greater than zero test
# Updated: 2/08/07 Alex Bettinardi alexander.o.bettinardi@odot.state.or.us
# Updated: 1/13/14 Alex Bettinardi alexander.o.bettinardi@odot.state.or.us - to work with OSUM V2
# Update:  8/27/15 Alex Bettinardi alexander.o.bettinardi@odot.state.or.us - small update to ensure that row and col control totals are equal for the ipf to close properly.
# Update:  4/13/16 Alex Bettinardi - small update to allow for the vistor model
#
# Define function for predicting household size distributions
#Added a title parameter and changed the spline line width and color
#Added regional observed control points for HH distribution at the regional average HH size
predictHHsizes <- function(data, max.mean, title, controldist, controlavghh) {

     # Load the household data, attach the data
        hhsize <- data
        attach(hhsize)
        
     # Plot the data
        plot(ave.size, pct.1per, xlim=c(1,max.mean), ylim=c(0,1), 
          xlab="Average Persons per Household", ylab="Percent of Households", type="n", main=title)
        points(ave.size, pct.1per, pch=20)
        points(ave.size, pct.2per, pch=20, col="red")
        points(ave.size, pct.3per, pch=20, col="blue")
        points(ave.size, pct.4per, pch=20, col="green3")
        legend(1, 1, c("1 person", "2 persons", "3 persons", "4 persons"), 
          col=c("black", "red", "blue", "green"), lty=1, lwd=2)
     
     # Fit the initial splines
        per1.spline <- smooth.spline(ave.size, pct.1per, df=3)
        per2.spline <- smooth.spline(ave.size, pct.2per, df=3)
        per3.spline <- smooth.spline(ave.size, pct.3per, df=7)
        per4.spline <- smooth.spline(ave.size, pct.4per, df=5)
     # Set up a vector of mean household sizes for entire range
        mean.predict <- round(seq(1, max.mean, by=0.1),1)
     
     # Predict the values for the range using the initial splines
        per1.predict <- predict(per1.spline, mean.predict)
        per2.predict <- predict(per2.spline, mean.predict)
        per3.predict <- predict(per3.spline, mean.predict)
        per4.predict <- predict(per4.spline, mean.predict)

     # Go through 5 iterations to fit the splines and meet certain constraints
     for(i in 5:1){
          # No negative percentages (Note that predict returns a list of two elements. Use the second.)
             per1.predict[[2]][per1.predict[[2]] < 0] <- 0
             per2.predict[[2]][per2.predict[[2]] < 0] <- 0
             per3.predict[[2]][per3.predict[[2]] < 0] <- 0
             per4.predict[[2]][per4.predict[[2]] < 0] <- 0
             
          #Force the curves through the regional observed distribution at the regional average HH size
             per1.predict[[2]][mean.predict==round(controlavghh,1)] <- controldist[1]
             per2.predict[[2]][mean.predict==round(controlavghh,1)] <- controldist[2]
             per3.predict[[2]][mean.predict==round(controlavghh,1)] <- controldist[3]
             per4.predict[[2]][mean.predict==round(controlavghh,1)] <- controldist[4]
             
          # Force the one person households to equal one when mean household size is one
             per1.predict[[2]][1:i] <- seq(1, (1-(i*0.1-0.1)), length=i)
             per1.spline <- smooth.spline(mean.predict, per1.predict[[2]], df=(18))
             per1.predict <- predict(per1.spline, mean.predict)
          
          # Adjust the two and three person households so that the total equals one
             tot.predict <- per1.predict[[2]] + per2.predict[[2]] +per3.predict[[2]] +per4.predict[[2]]
             gap <- 1 - tot.predict
             per2.predict[[2]][mean.predict < 3] <- per2.predict[[2]][mean.predict < 3] + gap[mean.predict < 3]
             per3.predict[[2]][mean.predict >= 3] <- per3.predict[[2]][mean.predict >= 3] + gap[mean.predict >= 3]
          # Fit splines to adjusted data for two, three and four person households
             per2.spline <- smooth.spline(mean.predict, per2.predict[[2]], df=7)
             per3.spline <- smooth.spline(mean.predict, per3.predict[[2]], df=7)
             per4.spline <- smooth.spline(mean.predict, per4.predict[[2]], df=7)
          # Predict values again
             per2.predict <- predict(per2.spline, mean.predict)
             per3.predict <- predict(per3.spline, mean.predict)
             per4.predict <- predict(per4.spline, mean.predict)
     }
     
     # Do one final adjustment to make one person households equal to one at mean hhsize equal one
        per1.predict[[2]][1] <- 1
     # Adjust three and four person households so that they are not less than zero
        per3.predict[[2]][per3.predict[[2]] < 0] <- 0
        per4.predict[[2]][per4.predict[[2]] < 0] <- 0
     
     # Adjust the two person households once again so that the total adds to one
        tot.predict <- per1.predict[[2]] + per2.predict[[2]] +per3.predict[[2]] +per4.predict[[2]]
        gap <- 1 - tot.predict
        per2.predict[[2]] <- per2.predict[[2]] + gap
        
     # Plot the results
        lines(mean.predict, per1.predict[[2]], lwd=2)
        lines(mean.predict, per2.predict[[2]], lwd=2, col="red")
        lines(mean.predict, per3.predict[[2]], lwd=2, col="blue")
        lines(mean.predict, per4.predict[[2]], lwd=2, col="green3")
        tot.predict <- per1.predict[[2]] + per2.predict[[2]] +per3.predict[[2]] +per4.predict[[2]]
        lines(mean.predict, tot.predict, col="purple", lty=3)
        
     # Return the results to the calling object
     result <- cbind("Average_Size"=mean.predict, "One_Person"=round(per1.predict[[2]],4), 
          "Two_Person"=round(per2.predict[[2]],4), "Three_Person"=round(per3.predict[[2]],4), 
          "Four_Person"=round(per4.predict[[2]],4))
     detach("hhsize")
     result
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate Future Household Size by TAZ                           
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hhfutr <- function () {

	# Prepare block data for splines using families and non-families as a surrogate for HHs
	#--------------------------------------------------------------------------------------


  if(!file.exists(paste(dataDir,"\\census_block.csv",sep=""))) stop("census_block.csv file is missing")
    census.block <- read.csv(paste(dataDir,"\\census_block.csv",sep=""))
    names(census.block) <- tolower(c("BLOCK","TAZ","HHS1BASE","HHS2BASE","HHS3BASE","HHS4BASE","POPBASE"))
     #Remove blocks with zero population for spline fitting 	
     #Convert block ID to character
	#Creates block group ID from block ID
     #Calculate the average HH size - note that popbase does not include group quarters pop
	census.block <- census.block[census.block$popbase!=0,]
	census.block$block<-as.character(census.block$block)
	census.block$bgbase<-substr(as.character(census.block$block),1,12)
	census.block$ahhsbase<-census.block$popbase/rowSums(census.block[,3:6])
		
	#Calculate the regional observed distribution to use later as control points for spline fit
     #Calculates the percent of each HH size of the total HHs 
	#Removes NAs from the data set (where there is no population)
	controldist<-colSums(census.block[,3:6])/sum(rowSums(census.block[,3:6]))
	controlavghh<-sum(census.block$popbase)/sum(census.block[,3:6])
	census.block[,3:6]<-census.block[,3:6]/rowSums(census.block[,3:6])
	census.block<-census.block[(is.infinite(census.block$ahhsbase))==FALSE,]
		
     #Test to see if there is a significant difference between the family and HH average HH size
     #Check p value (p > 0.05 no significant difference 95% of the time)
	ttest<-t.test(census.block$ahhsbase,taz.data$ahhsbase)
	cat(noquote("\nt test p value of avg family size at the block level vs avg HH size at the taz level\n"))
	print(ttest$p.value)
     
     #Format object to be read by hhsize.R spline fit function
     #Renames the colnames to the names required by the predictHHsizes function
	census.block<-census.block[,c(1:6,9)]
	colnames(census.block)<-c("blockid","taz","pct.1per","pct.2per","pct.3per","pct.4per","ave.size")
	
     #Source in the spline fitting function
     #Fit splines using only the study area blocks
     #Create an object of the future HH size distribution lookup table for average HH sizes of 1-4.5
     #Save the plot as a pdf and close the plot window
	pdf(paste(dataDir,"\\census_block_splinefit.pdf",sep=""))
  splines<-predictHHsizes(census.block, 4.5, paste(city, " Study Area Household Size in ", year, sep=""), controldist, controlavghh)
	dev.off()
	
	#Calculate the future year Household size distribution  
	#-------------------------------------------------------
	
     #Check to ensure there is a future popuation or housing 
     #Calculate future year average regional HH size
     #Calculate HH size distribution for regional in future using the average regional future HH size
     
	#Changed ahhsfutr to an input found in the version file 3-22-13 AB
	futurehhdist<-splines[match(round(futrAvgHHsize,1), splines[,1]),2:5]
	futurehhtotal<-sum(taz.data$hhbase)*futurehhdist
  
  if(futureHHSflag == "orig"){   
  ###############################################3
  # Old logic as of 2-07-07
  
  # First obtain reference households
  hhref <- getAttTable(Visum,"Zones",c("TAZ","HHREF"),initalFilter=T)   
  hhref <- hhref[!hhref[,1] %in% as.numeric(row.names(ext.traffic)),]
  rownames(hhref) <- hhref[,1]
  hhref <- hhref[rownames(taz.data),2]
  
       #Calculate HH size distribution for regional in base year using the average regional HH size
	#Calculate base year total HHs	
	basehhdist<-splines[match(round(controlavghh,1), splines[,1]),2:5]
	basehhtotal<-sum(hhref)*basehhdist
  
  #Calculate distribution of change in new HHs (regionally)
	#Calculate the change in the number of HHs from the base to the future year
	#Declare a matrix to hold the results of future HH distribution by TAZ	
  newhhpct<-(futurehhtotal-basehhtotal)/sum((futurehhtotal-basehhtotal))
	taz.data$hhdiff<-taz.data$hhbase-hhref
	newhh<-matrix(NA,length(taz.data$taz),4)
	
  #Match the future average regional HH size with the corresponding predicted average HH size
	#distribution in the splines object created by the spline fit and multiply by the HHDIFF field
	#in the taz object
	for (i in 1:length(taz.data$taz)) { newhh[i,]<-taz.data$hhdiff[i]*newhhpct }

     #Create object of the distribution of the additional households
	#Return the new HHs by TAZ from the base to future year
	newhhs<-data.frame(taz.data$taz,newhh)
	colnames(newhhs)<-c("taz","hhnew1","hhnew2","hhnew3","hhnew4")
	
  hh.size.dist <- as.matrix(sweep(taz.data[,grep("^hhs",names(taz.data))], 1, hhref, "*")) 
  colnames(hh.size.dist) <- c("1person", "2persons", "3persons", "4+persons")
  newhhs <-as.matrix(hh.size.dist+newhhs[,2:5]) 
	
	########################################################################
	} else {
	#############################################
	# New Logic as of 2-7-07
  hh.size.dist <- splines[match(round(taz.data$ahhsbase,1), splines[,1]),2:5]
  badDist <- apply(hh.size.dist, 1, function(x) any(x < 0 || x> 1))
  badDist[is.na(badDist)] <- T
  hh.size.dist[badDist,] <- as.matrix(taz.data[badDist,grep("^hhs", names(taz.data))])
  temphh <- taz.data$hhbase
  #temphh[temphh==0] <- 0.001     # change on 1-13-2014 to remove unnessisary error ipf error message
  hh.size.dist <- as.matrix(sweep(hh.size.dist, 1, taz.data$hhbase, "*")) 
  colnames(hh.size.dist) <- c("1person", "2persons", "3persons", "4+persons")
  newhhs <- hh.size.dist     # change on 1-13-2014 to remove unnessisary error ipf error message
  newhhs[] <- 0              # change on 1-13-2014 to remove unnessisary error ipf error message
  futurehhtotal <- futurehhtotal*sum(temphh)/sum(futurehhtotal) # extra logic check to ensure that col control totals to row control AB 8-27-15
  newhhs[temphh>0,] <- ipf(temphh[temphh>0], futurehhtotal, hh.size.dist[temphh>0,] ) # remove zero fields before sending to ipf, to get rid of unnessisary error message
  rowtotal <- rowSums(newhhs)
  rowfactor <- taz.data$hhbase/rowtotal
  rowfactor[is.nan(rowfactor)] <- 0
  newhhs <- sweep(newhhs, 1, rowfactor, "*")
  
     # Quick addition to give warning if distribution is off
     probTAZs <- cbind(round(taz.data$hhbase), round(rowSums(newhhs)))[round(rowSums(newhhs)) != round(taz.data$hhbase),]
     rownames(probTAZs) <- taz.data$taz[round(rowSums(newhhs)) != round(taz.data$hhbase)]
     if(nrow(probTAZs) > 0) ePopUp(c("The Following Zones have Future HHs,\nbut have incorrect distributions:\n\nTAZ   Correct Number HHs   Assigned Number HHs",
                            paste(rownames(probTAZs), apply(probTAZs,1,function(x)paste(x[1],x[2],sep="             ")), sep="            ")))
	
	#############################################
	}
  newhhs
}

#Runs the newhhs function and saves the results as an object to the global workspace
#Remove function objects from the workspace
newhhs<-hhfutr()
rm(hhfutr)
rm(predictHHsizes)

