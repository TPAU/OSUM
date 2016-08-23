 # runReport.R
 # Code Base (C) 2.1.0: 3-7-16 (AB)
 #============ 
 #**Author:** Alex Bettinardi (AB) 
 #**Contact:** alexander.o.bettinardi@odot.state.or.us  
 #**Date:** 1/2/13  
 #**Revisions:** This is the first version  
 #**License:** GPL2  
 #
 # Creates standard Reports for OSUM under the html "runReport.html".
 # 
 # 2/10/2014 AB - updated to plot travel time distribution for all trips (previously, just by the 5 purposes, now there is a "total" plot)
 # 3/5/2014 AB - corrected minor text issue in diurnal plotter 
 # 2/11/2016 AB - Added a plot for intrazonal demand vs total OD demand

# First load opitional libraries for html tables:
   options(warn=-1)
   htmlTableFlag <- library("xtable", logical.return=T)
   options(warn=0)

# create folder for storing the runReports
reportFolder <- "runReport"
if(!file.exists(reportFolder)) dir.create(reportFolder)
# create place to update resoltion of plots
Png <- list(Width=1000, Height=1200, Res = 150)

 #Define function to load the reference space
 #====================

     # Function to load reference work space  
     assignWorkSpace <- function(filename){
        temp = new.env()
        load(filename, envir=temp)
        mget(ls(temp)[ls(temp) != "filename"], temp)
     }


 # Load the Reference Workspace (all under ref)
 #================================================
  
     # Pull the reference work space to be compared from the VISUM file
     refData <- comGetProperty(vbConv("Visum.Net"), "AttValue", "REFERENCE")
     
     # load reference work space
     ref <- assignWorkSpace(refData)

     # clean up work space
     ref$Path <- refData
     ref <- ref
     rm(refData, assignWorkSpace)

 # Plot trip generation plots
 #================================================

     # set up plotting device
     png(paste(reportFolder, "TripGen.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)

     layout(matrix(1:2, byrow=TRUE, ncol=1))
     par(mar=c(2,4.1,4.1,2.1), oma=c(2,2,2,2))

     # Check Work Distribution Results
     x=barplot(rbind(Current=apply(hh.wkr.dist,2,sum), Reference=apply(ref$hh.wkr.dist,2,sum)), beside=T, col=c("white", "grey"), legend.text=T,
             main="Comparison of Number of Workers per Household Between", ylab="Total Households")
     abline(h=0)
     text(x, rbind(Current=apply(hh.wkr.dist,2,sum), Reference=apply(ref$hh.wkr.dist,2,sum)), round(rbind(Current=apply(hh.wkr.dist,2,sum), Reference=apply(ref$hh.wkr.dist,2,sum))), cex=0.8, pos=1)      

     # Plot Trip Generation Summary Results
     x=barplot(rbind(Current=trip.prod$trip.data, Reference=ref$trip.prod$trip.data), beside=T, col=c("white", "grey"), legend.text=T,
             main="Trip Generation Summary Results", ylab="Measure")
     abline(h=0)
     text(x, 0, round(rbind(Current=trip.prod$trip.data, Reference=ref$trip.prod$trip.data)), pos=2, srt=270)      
     mtext("Trip Generation Summaries", side=3, line=0, outer=TRUE, cex=1.25)    
     
     dev.off()
     rm(x)  
     

 # Plot Intrazonal vs total demand by purpose
 #===========================================     
# 2-11-16 AB
# new plot to look at intrazonal demand vs total demand

     # set up plotting device
     png(paste(reportFolder, "TripVeh.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)

     layout(matrix(1:2, byrow=TRUE, ncol=1))
     par(mar=c(2,4.1,4.1,2.1), oma=c(2,2,2,2))

     # Plot II Vehicle Generation by purpose 
     y=rbind(Current=c(trip.dist$summary.int.stats$est.trips/occ, sum(ext.traffic[,"adt"]*(1-(ext.traffic[,"ee.pct"]/2)))), 
             Reference=c(ref$trip.dist$summary.int.stats$est.trips/ref$occ, sum(ref$ext.traffic[,"adt"]*(1-(ref$ext.traffic[,"ee.pct"]/2)))))
     x=barplot(y,beside=T, col=c("white", "grey"), legend.text=T, main="Vehicle Trip Generation Totals by Purpose", 
             ylab="Total Trips by Purpose", names.arg = c(row.names(trip.dist$summary.int.stats), "externals"), args.legend=list(x="topleft"))
     abline(h=0)
     text(x,ifelse(y<max(y)*.5,y+(max(y)*.2),y-(max(y)*.2)),round(y), pos=1, srt=270)   

   
     # Plot Intrazonal Vehicles and percentages by purpose 
     y=rbind(Current=c(apply(trip.dist$daily.int.persontrips.od,3,function(x) sum(diag(x)))/occ,sum(diag(trip.dist$daily.vehicles))), 
             Reference=c(apply(ref$trip.dist$daily.int.persontrips.od,3,function(x) sum(diag(x)))/occ,sum(diag(ref$trip.dist$daily.vehicles))))
     colnames(y)[6] <- "Total"
     x=barplot(y,beside=T, col=c("white", "grey"), legend.text=T, main="Intra-zonal Vehicle Trip Totals by Purpose", 
             ylab="Vehicle Trips by Purpose",  args.legend=list(x="topleft"))
     abline(h=0)
     z=rbind(Current=c(apply(trip.dist$daily.int.persontrips.od,3,sum)/occ,sum(trip.dist$daily.vehicles)), 
         Reference=c(apply(ref$trip.dist$daily.int.persontrips.od,3,sum)/occ,sum(ref$trip.dist$daily.vehicles)))
     text(x,ifelse(y<max(y)*.5,y+(max(y)*.2),y-(max(y)*.2)),paste(round(y), ":  ", round(100*y/z,1),"%",sep=""), pos=1, srt=270)   
     mtext("Vehicles by Purpose Summaries", side=3, line=0, outer=TRUE, cex=1.25)    
     
     rm(x,y,z)
     dev.off()       

 # Create/Plot PA Trip Time Frequency Distribution
 #================================================

 #Function to tabulate trips by time intervals to produce travel time distribution plots
 #--------------------------------------------

 #This function takes a zone to zone matrix travel time matrix and a zone to zone matrix of trips
 #and sums up the number of trips by a specified time interval. This function correctly accounts for fractional trips.
 #Other methods of doing this by repeating the travel times by the number of trips give the wrong
 #result because "rep" drops fractional trips. Trip fractions can add up to a sizable proportion of trips.
 #This was resolved in some code by multiplying the repeats by some number (e.g. 40) and then dividing the
 #binned result by the number. This however creates very long vectors and memory problems.
 #The following function bins the time data and then sums the number of trips by time bin. There are no memory problems.

 #The function returns a list that contains vectors identifying the breaks for the time bins (Breaks),
 #the number of trips in each time bin (Trips) and the midpoint values of each time bin (Mids).
 #The results can then be graphed directly using the midpoints and number of trips.
 #If it is desired to plot a smooth density distribution, then set the interval to sum small value (e.g. 0.25)
 #and then send to the density function a vector created by repeating the Mids by the Trips.
 # Brian Gregor 4/2006
 #(Brian's original code was designed to produce a historgram and used TimeInterval=1)

 #Example:
 #TimeTab_ <- tabulateTripTimes(OffPkTime.ZnZn, HbsTrips.ZnZn, TimeInterval=0.25)
 #plot(TimeTab_$Mids, TimeTab_$Trips, type="l")
 #plot(density(rep(TimeTab_$Mids, TimeTab_$Trips)))

     tabulateTripTimes <- function(Times.ZnZn, Trips.ZnZn, TimeInterval=.1){

        #Check that the Times.ZnZn and Trips.ZnZn are conforming matrices
        if(!is.matrix(Times.ZnZn)) stop("Times.ZnZn must be a matrix.")
        if(!is.matrix(Trips.ZnZn)) stop("Trips.ZnZn must be a matrix.")
        if(any(dim(Times.ZnZn) != dim(Trips.ZnZn))){
           stop("Times.ZnZn and Trips.ZnZn don't have the same dimensions")
           }
        if(any(rownames(Times.ZnZn) != rownames(Trips.ZnZn))){
           warning("rownames(Times.ZnZn) != rownames(Trips.ZnZn)")
           }
        if(any(colnames(Times.ZnZn) != colnames(Trips.ZnZn))){
           warning("colnames(Times.ZnZn) != colnames(Trips.ZnZn)")
           }

        #Unravel Times.ZnZn and Trips.ZnZn into vectors
        Times.X <- as.vector(Times.ZnZn)
        Trips.X <- as.vector(Trips.ZnZn)

        #Cut times into time intervals
        MaxTime <- max(Times.ZnZn)
        MaxTimeInterval <- TimeInterval * ceiling(MaxTime / TimeInterval)
        TimeIntervals.Tm <- seq(0, MaxTimeInterval, TimeInterval)
        NumIntervals <- length(TimeIntervals.Tm)
        TimeCut.X <- cut(Times.X, breaks=TimeIntervals.Tm, include.lowest=TRUE)

        #Tabulate trips by time interval
        Trips.Tm <- tapply(Trips.X, TimeCut.X, sum)
        Trips.Tm[is.na(Trips.Tm)] <- 0

        Time.Tm <- tapply(Times.X, TimeCut.X, mean)
        rep(Time.Tm,Trips.Tm)
        #removed Brian's histogram code to make way for plot

     }
     
     # set up plotting device
     png(paste(reportFolder, "TripDist.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)
     
     # Set up 3 X 2 plot layout
     layout(matrix(1:6, byrow=TRUE, ncol=2))
     # Change inner and outer margins
     par(mar=c(1,2,2,1), oma=c(2,2,2,2))

 #Create mean travel times for the survey trips and the model trips to include on the travel time distrubution plots
 #-------------------------------------------------------------------------------

     for(pu in c("hbw", "hbsch", "hbshp", "hbro", "nhb")) {
        plot(density(tabulateTripTimes(pk.time[rownames(trip.dist$daily.int.persontrips.pa), colnames(trip.dist$daily.int.persontrips.pa)],
             trip.dist$daily.int.persontrips.pa[,,pu]), bw=2, from=0, to=round(max(pk.time)*1.2)), ylim=c(0,0.2), xlab="", ylab="", main="") 
        lines(density(tabulateTripTimes(ref$pk.time[rownames(ref$trip.dist$daily.int.persontrips.pa), colnames(ref$trip.dist$daily.int.persontrips.pa)],
              ref$trip.dist$daily.int.persontrips.pa[,,pu]), bw=2, from=0, to=round(max(ref$pk.time)*1.2)),lty=2, col="grey")
        abline(v=weighted.mean(pk.time[rownames(trip.dist$daily.int.persontrips.pa), colnames(trip.dist$daily.int.persontrips.pa)],
               trip.dist$daily.int.persontrips.pa[,,pu]), lty=1, col="black")
        abline(v=weighted.mean(ref$pk.time[rownames(ref$trip.dist$daily.int.persontrips.pa), colnames(ref$trip.dist$daily.int.persontrips.pa)],
               ref$trip.dist$daily.int.persontrips.pa[,,pu]), lty=2, col="grey")
        text(round(max(ref$pk.time)*.1), 0.18, pu, cex=1.2)
        meantime <- round(weighted.mean(pk.time[rownames(trip.dist$daily.int.persontrips.pa), colnames(trip.dist$daily.int.persontrips.pa)],trip.dist$daily.int.persontrips.pa[,,pu]),2)
        text(meantime, 0.04, paste("mean =", meantime))
        meantime <- round(weighted.mean(ref$pk.time[rownames(ref$trip.dist$daily.int.persontrips.pa), colnames(ref$trip.dist$daily.int.persontrips.pa)],ref$trip.dist$daily.int.persontrips.pa[,,pu]),2)
        text(meantime, 0.02, paste("mean =", meantime), col="grey")
        legend("topright", legend=c("Current", "Reference"), lty=c(1,2), col=c("black", "grey"), bty="n")
     }
     
     # plot total distribution:
     
        plot(density(tabulateTripTimes(pk.time,trip.dist$daily.vehicles), bw=2, from=0, to=round(max(pk.time)*1.2)), ylim=c(0,0.2), xlab="", ylab="", main="") 
        lines(density(tabulateTripTimes(ref$pk.time, ref$trip.dist$daily.vehicles), bw=2, from=0, to=round(max(ref$pk.time)*1.2)),lty=2, col="grey")
        abline(v=weighted.mean(pk.time, trip.dist$daily.vehicles), lty=1, col="black")
        abline(v=weighted.mean(ref$pk.time, ref$trip.dist$daily.vehicles), lty=2, col="grey")
        text(round(max(ref$pk.time)*.1), 0.18, "All\nVehicle\nTrips", cex=1.2)
        meantime <- round(weighted.mean(pk.time,trip.dist$daily.vehicles),2)
        text(meantime, 0.04, paste("mean =", meantime))
        meantime <- round(weighted.mean(ref$pk.time,ref$trip.dist$daily.vehicles),2)
        text(meantime, 0.02, paste("mean =", meantime), col="grey")
        legend("topright", legend=c("Current", "Reference"), lty=c(1,2), col=c("black", "grey"), bty="n")
    
     mtext("Travel Time Frequency Distributions Comparison", side=3, line=0, outer=TRUE, cex=1.25)
     
     # clean work space
     rm(pu, meantime, tabulateTripTimes) 
     dev.off()    
     
     # Plot Hourly distribution
     png(paste(reportFolder, "HourlyDist.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)
     plot(1:24,apply(trip.dist$hourly.vehicles,3,sum),type="l",lty=1, col="black", xlab="Hour", ylab="Hourly Vehicle Totals", main="Hourly Distribtuion Comparison")
     lines(1:24,apply(ref$trip.dist$hourly.vehicles,3,sum),lty=2, col="grey")
     abline(v=pkHr,lty=1,col="black")
     abline(v=ref$pkHr,lty=2,col="grey")
     Max <- max(apply(trip.dist$hourly.vehicles,3,sum))
     text(pkHr,Max*.5,paste("PkHr =",pkHr),pos=2)
     text(ref$pkHr,Max*.4,paste("PkHr =",ref$pkHr),pos=2 , col="grey")
     abline(v=amPk,lty=1,col="black")
     abline(v=ref$amPk,lty=2,col="grey")
     text(amPk,Max*.3,paste("AM PkHr =",amPk), pos=4)
     text(ref$amPk,Max*.2,paste("AM PkHr =",ref$amPk), pos=4, col="grey")
     legend("topleft", legend=c("Current", "Reference"), lty=c(1,2), col=c("black", "grey"), bty="n")
     dev.off()
     rm(Max)

 # Create/Plot Link/assignment Comparison plots
 #================================================

     #Data Prep
     hourNames <- c("12AM", "1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM", "9AM", "10AM", "11AM", "12PM", "1PM", "2PM", "3PM", "4PM", "5PM", "6PM", "7PM", "8PM", "9PM", "10PM", "11PM", "12AM")
     link.. <- Link..[intersect(rownames(Link..), rownames(ref$Link..)),c("PLANNO", "LENGTH", paste("VOL", hourNames[pkHr], hourNames[pkHr+1], sep="_"), "DAILY_VOLUME")]
     link..[,5:6] <- ref$Link..[rownames(link..),c(paste("VOL", hourNames[pkHr], hourNames[pkHr+1], sep="_"), "DAILY_VOLUME")]
     names(link..) <- c("FC", "Length", "PkVol", "DailyVol", "PkRef", "DailyRef")
     
     # clean FC if needed
     link..$FC[!(link..$FC %in% c(1:5,30))] <- 0
     
     # remove zero volume links
     link.. <- link..[rowSums(link..[,3:6]) > 0,]     

 #Link Scatterplot - daily validation
 #-----------------------------------
 if(nrow(link..) > 0){ # if statement in case last network had a different network
     # set up plotting device
     png(paste(reportFolder, "LinkScatter.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)
     # Set up 1 X 2 plot layout
     layout(matrix(1:2, byrow=TRUE, ncol=1))
     # Change inner and outer margins
     par(mar=c(5,4.1,2.1,2.1), oma=c(0,0,2,0))
     
     # turn off plotting warnings
     options(warn=-1)

     reglink <- summary(lm(link..$DailyVol~link..$DailyRef))
     plot(link..$DailyRef,link..$DailyVol, xlab="Refenece Daily Volume", ylab="Current Daily Volume", main="Daily Comparison")
     text( 0, max(link..$DailyVol)*.9,
          paste("y =", round(reglink$coefficients[2,1],2), "* x +",round(reglink$coefficients[1,1])),pos=4)
     text(0, max(link..$DailyVol)*.81, paste("R-squared =", round(reglink$r.square,2)), pos=4)
     lines(c(-10000,max(link..$DailyVol*2)), c(-10000,max(link..$DailyVol*2)))
     abline(reglink, col="red", lwd=2)
     polygon(c(0,rep(max(link..$DailyVol)*.4,2),0,0), c(rep(max(link..$DailyVol)*.75,2), rep(max(link..$DailyVol)*.95,2), max(link..$DailyVol)*.75)) 
     
     reglink <- summary(lm(link..$PkVol~link..$PkRef))
     plot(link..$PkRef,link..$PkVol, xlab= "Refenece Peak Volume", ylab="Current Peak Volume", main=paste("Peak Comparison: ", hourNames[pkHr], "-", hourNames[pkHr+1], sep=""))
     text( 0, max(link..$PkVol)*.9,
          paste("y =", round(reglink$coefficients[2,1],2), "* x +",round(reglink$coefficients[1,1])),pos=4)
     text(0, max(link..$PkVol)*.81, paste("R-squared =", round(reglink$r.square,2)), pos=4)
     lines(c(-10000,max(link..$PkVol*2)), c(-10000,max(link..$PkVol*2)))
     abline(reglink, col="red", lwd=2)
     polygon(c(0,rep(max(link..$PkVol)*.4,2),0,0), c(rep(max(link..$PkVol)*.75,2), rep(max(link..$PkVol)*.95,2), max(link..$PkVol)*.75))    
     
     mtext("Link Assignment Scatterplots", side=3, line=0, outer=TRUE, cex=1.25)
     options(warn=0)
     
     dev.off()

     rm(reglink)


 #Maximum Desirable Errors for Link Volumes
 #-----------------------------------------

     link..$per_dev <- abs(100*((link..$DailyVol-link..$DailyRef)/link..$DailyRef))
     #add color and shape plot - see Rpad ref card for shape examples
     Col <- c("brown", "blue", "red","green","orange","purple", "black")
     Shape <- c(15:19,8,20)
     names(Col) <- names(Shape) <- c(1:5,30, 0)
     # create an index to remove very low volume links (Daily volume needs to be greater than 10 vehicles)
     ind <- link..$DailyRef > 10

     # set up plotting device
     png(paste(reportFolder, "DesireableError.png", sep="/"), width=Png$Width, height=800, res=Png$Res)

     # Restore graphic parameters
     par(mar=c(5.1,4.1,4.1,2.1), oma=rep(0,4))
     layout(1)

     plot(link..$DailyRef[ind], link..$per_dev[ind] , col=Col[as.character(link..$FC[ind])], pch=Shape[as.character(link..$FC[ind])],xlab="Reference Daily Volume", ylab="Percent Deviation", main="Maximum Desirable Errors for Daily Link Volumes")

     #NCHPR 255 graph and the usage of a curve fitting software program to defined the curve equation: y = a * (x -b)^c, where a = 2067.56, b = -1357.62, and c = -0.43.

     lines(1:max(link..$DailyRef),2067.56* (1:max(link..$DailyRef)+1357.62)^ -0.43,lwd=2)
     legend("topright", c("Max desirable Error", "Interstate", "Principal Arterial", "Minor Arterial","Collector", "Local", "Ramps", "No FC"),
       lwd= c(2,rep(NA,length(Col))),lty=c(1, rep(NA,length(Col))),
       pch=  c(NA,Shape),
       col= c("black", Col))
     link..$Max_Dev <- 2067.56* (link..$DailyRef+1357.62)^ -0.43

     dev.off()

 # Write html to store everything
 #===============================
 writeLines(c("<html>", "<head>",
           "<title>OSUM Run Comparison</title>",
           "</head>","<body>",
           " <p>The following are comparisons of this model run vs. the Reference file which is defined in the VISUM version as:", 
           paste(ref$Path,"</p>",sep=""),
           paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"",
                  paste(reportFolder, "TripGen.png", sep="/"),
                  "\" title=\"plot of chunk reporting\" alt=\"plot of chunk reporting\" class=\"plot\" /></div>",
                  "<div class=\"rimage default\"><img src=\"",
                  paste(reportFolder, "TripVeh.png", sep="/"),
                  "\" title=\"plot of chunk reporting\" alt=\"plot of chunk reporting\" class=\"plot\" /></div>",
                  "<div class=\"rimage default\"><img src=\"",
                  paste(reportFolder, "TripDist.png", sep="/"),
                  "\" title=\"plot of chunk reporting\" alt=\"plot of chunk reporting\" class=\"plot\" /></div>",
                  "<div class=\"rimage default\"><img src=\"",
                  paste(reportFolder, "HourlyDist.png", sep="/"),
                  "\" title=\"plot of chunk reporting\" alt=\"plot of chunk reporting\" class=\"plot\" /></div>",
                  "<div class=\"rimage default\"><img src=\"",
                  paste(reportFolder, "LinkScatter.png", sep="/"),
                  "\" title=\"plot of chunk reporting\" alt=\"plot of chunk reporting\" class=\"plot\" /></div>",
                  "<div class=\"rimage default\"><img src=\"",
                  paste(reportFolder, "DesireableError.png", sep="/"),
                  "\" title=\"plot of chunk reporting\" alt=\"plot of chunk reporting\" class=\"plot\" /></div><div class=\"rcode\">",sep=""),
           paste("<div class=\"output\"><pre class=\"knitr r\">",
                  paste("Percent of links below the max deviation line: ", round(sum(link..$Max_Dev[ind] > link..$per_dev[ind])/sum(ind)*100,2), "%", sep=""))
                  ),paste(reportFolder, ".html", sep=""))        

           rm(Col, Shape, ind)
           
  # now build htlm tables if flag is true and append them to the html
  #-------------------------------------------------------------------           

  if(htmlTableFlag){
     
     sink("temp.html")
     
     #Maximum Desirable Errors tables
     #-----------------------------------------
     
     dev_FC <- cbind(c("Interstate", "Principal Arterial", "Minor Arterial", "Collector", "Local", "Ramps", "No FC", "All Links"), rep(NA,8), rep(NA,8))
     rownames(dev_FC) <- c(1:5,30, 0, "All")
     Dev <-  round(abs(100*c((tapply(link..$DailyRef, link..$FC, sum)-tapply(link..$DailyVol, link..$FC, sum))/tapply(link..$DailyRef, link..$FC, sum), 
                   All=(sum(link..$DailyVol)-sum(link..$DailyVol))/sum(link..$DailyVol))))
     dev_FC[names(Dev),2] <- paste(Dev, "%", sep="")
     Dev <-  round(abs(100*c((tapply(link..$PkRef, link..$FC, sum)-tapply(link..$PkVol, link..$FC, sum))/tapply(link..$PkRef, link..$FC, sum), 
                   All=(sum(link..$PkVol)-sum(link..$PkVol))/sum(link..$PkVol))))
     dev_FC[names(Dev),3] <- paste(Dev, "%", sep="")               

     colnames(dev_FC)= c("Functional Classification" , "Daily Comparison", "Peak Comparison")
     CompXtable <- xtable( dev_FC, align=rep( "c", ncol(dev_FC)+1 ), caption="Percent Deviation, Functional Classification" )
     print(CompXtable, type="html", caption.placement="top", include.colnames=TRUE, include.rownames=FALSE)
     
     #Create Volume Binds
     link..$VolBin<- cut(link..$DailyVol/1000,breaks= c(0,1,2.5,5,10,25,50,1000), right=FALSE, labels=1:7)
    	
     dev_Vol <- cbind(X=c("<1000", "1000-2,499", "2500-4999", "5000-9,999", "10,000-24,999", "25000-49,999", ">50,000", "All Links"),rep(NA,8), rep(NA,8))
     rownames(dev_Vol) <- c(1:7, "All")
     Dev <-  round(abs(100*c((tapply(link..$DailyRef, link..$VolBin, sum)-tapply(link..$DailyVol, link..$VolBin, sum))/tapply(link..$DailyRef, link..$VolBin, sum), 
                   All=(sum(link..$DailyVol)-sum(link..$DailyVol))/sum(link..$DailyVol))))
     dev_Vol[names(Dev),2] <- paste(Dev, "%", sep="")
     Dev <-  round(abs(100*c((tapply(link..$PkRef, link..$VolBin, sum)-tapply(link..$PkVol, link..$VolBin, sum))/tapply(link..$PkRef, link..$VolBin, sum), 
                   All=(sum(link..$PkVol)-sum(link..$PkVol))/sum(link..$PkVol))))
     dev_Vol[names(Dev),3] <- paste(Dev, "%", sep="")                

     colnames(dev_Vol)= c("Link Volume Group", "Daily Comparison", "Peak Comparison")
     CompXtable <- xtable( dev_Vol, align=rep( "c", ncol(dev_Vol)+1 ), caption="Percent Deviation, Volume Group Range" )
		 print(CompXtable, type="html", caption.placement="top", include.colnames=TRUE, include.rownames=FALSE)
           
     rm(dev_Vol, dev_FC, Dev)	

 # RMSE Tables
 #--------------

     # create standard error field
     link..$SE<- (link..$DailyVol-link..$DailyRef)^2
     
     #Target %RMSE by FC
     
     RMSE_FC <- cbind(c("Interstate", "Principal Arterial", "Minor Arterial", "Collector", "Local", "Ramps", "No FC", "All Links"), rep(0,8), rep(NA,8), rep(NA,8), rep(NA,8))
     rownames(RMSE_FC) <- c(1:5,30, 0, "All")
     colnames(RMSE_FC)= c("Functional Classification" , "Number of Obs.", "Mean Volume", "RSME","%RMSE")
     tempCol <-  c(table(link..$FC), All=nrow(link..))
     RMSE_FC[names(tempCol),2] <- tempCol
     tempCol <- round(c(tapply(link..$DailyVol, link..$FC, mean), All=mean(link..$DailyVol)))
     RMSE_FC[names(tempCol),3] <- tempCol
     tempCol <-  round(c(sqrt(tapply(link..$SE, link..$FC, sum)/table(link..$FC)), All=sqrt(sum(link..$SE)/nrow(link..))))
     RMSE_FC[names(tempCol),4] <- tempCol
     tempCol <-  round(100*c(sqrt(tapply(link..$SE, link..$FC, sum)/table(link..$FC)), All=sqrt(sum(link..$SE)/nrow(link..))) /  
                    c(tapply(link..$DailyVol, link..$FC, mean), All=mean(link..$DailyVol)))
     RMSE_FC[names(tempCol),5] <- paste(tempCol, "%", sep="")     
     
     CompXtable <- xtable( RMSE_FC, align=rep( "c", ncol(RMSE_FC)+1 ), caption="RMSE & %RMSE – Functional Classification Daily Volume Comparison")
     print(CompXtable, type="html", caption.placement="top", include.colnames=TRUE, include.rownames=FALSE)     
     
     #Target %RMSE by Link Volume
     RMSE_Vol <- cbind(X=c("<1000", "1000-2,499", "2500-4999", "5000-9,999", "10,000-24,999", "25000-49,999", ">50,000", "All Links"),
                    c(table(link..$VolBin),nrow(link..)),
                    round(c(tapply(link..$DailyVol, link..$VolBin, mean), mean(link..$DailyVol))),
                    round(c(sqrt(tapply(link..$SE, link..$VolBin, sum)/table(link..$VolBin)), sqrt(sum(link..$SE)/nrow(link..)))),
                    paste(round(100*c(sqrt(tapply(link..$SE, link..$VolBin, sum)/table(link..$VolBin)), sqrt(sum(link..$SE)/nrow(link..))) /  
                    c(tapply(link..$DailyVol, link..$VolBin, mean), mean(link..$DailyVol))),"%",sep= ""))

     colnames(RMSE_Vol)= c("Link Volume Group" , "Number of Obs.", "Mean Volume", "RSME","%RMSE")
     CompXtable <- xtable( RMSE_Vol, align=rep( "c", ncol(RMSE_Vol)+1 ), #digits=c( 0, 0, 0, 0,0),
		         caption="RMSE & %RMSE – Volume Group Range Daily Volume Comparison")
     print(CompXtable, type="html", caption.placement="top", include.colnames=TRUE, include.rownames=FALSE)
     
     # close the "temp.html" sink
     sink()
     
     # append tables to html
     temp <- readLines(paste(reportFolder, ".html", sep=""))
     writeLines(c(temp,readLines("temp.html")),paste(reportFolder, ".html", sep=""))
     file.remove("temp.html")
          # clean up work space
     rm(RMSE_Vol, RMSE_FC, CompXtable, tempCol,temp)
  }
  
  # Finalize the html
  #------------------
  temp <- readLines(paste(reportFolder, ".html", sep=""))
  writeLines(c(temp, "</body>","</html>"),paste(reportFolder, ".html", sep=""))             
  
  
  # quick step to export reference volumes back to VISUM for review
  #----------------------------------------------------------------
  
     link.. <- Link..[intersect(rownames(Link..), rownames(ref$Link..)),c("VISUM_ID",names(Link..)[1:3])]
     link..[,5:6] <- ref$Link..[rownames(link..),c(paste("VOL", hourNames[pkHr], hourNames[pkHr+1], sep="_"), "DAILY_VOLUME")]
     names(link..)[5:6] <- c(paste("REF", hourNames[pkHr], hourNames[pkHr+1], sep="_"), "REF_DAILY_VOL")
     
     Lists <- comGetProperty(vbConv("Visum.Net"), "Links")
     AddFieldsFlag <- sapply(names(link..)[5:length(names(link..))], function(x) is.null(comGetProperty(Lists, "GetMultiAttValues", x))) 
     AddFields <- as.data.frame(link..[,(5:length(names(link..)))[AddFieldsFlag]])
     names(AddFields) <- names(AddFieldsFlag)[AddFieldsFlag]
   
     # Adding fields that don't exits, currently numeric fields are the only ones coded   
     if(ncol(AddFields)>0) addNetAtt(Visum,"Links",AddFields)

     # add Link data to VISUM
     sapply(names(link..)[5:length(names(link..))], function(x) setNetAtt(Visum,"Links",x,link..[,c("VISUM_ID",x)],initalFilter=T)) 

     comInvoke(Visum, "SaveVersion", paste(attr(Visum, "Path"),attr(Visum, "Name"),sep="/"))
   
     graphic <- comGetProperty(Visum, "Graphic")
     comInvoke(graphic, "ShowNormal")
     
     # clean up work space
     rm(link.., hourNames, reportFolder, htmlTableFlag, Png, Lists, AddFieldsFlag, AddFields, temp, graphic) 
} else {# close check for un-common case where reference networks have a different structure 
     # clean up without network analysis
     rm(link.., hourNames, reportFolder, htmlTableFlag, Png)
}



