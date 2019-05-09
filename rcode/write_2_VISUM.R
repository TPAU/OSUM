# write_2_VISUM.R
# Code Base (C) 2.8: 10-22-18 (AB)

# This program writes OD trip tables as VISUM binary text files
# The trip tables represent vehicle trip distributions for specified hours of the day (1 to 24).
#
#
# Re-Created:             6/7/07 Alex Bettinardi
#    Now uses functions to write matrices strait to VISUM. Also, this script now    
#    calls Visum and runs the assignment and saves a large number of variables to Visum.
#    This script is now used to communicate with Visum from R.
# Updated:  5/29/13 Alex Bettinardi
#    Updated for OSUM V2 - now uses Martin Mann's VisumFunctions Library
# Updated:  10/26/16 Alex Bettinardi
#    Updated all the calls to ".par" files to ".xml" due to the format change in Visum 16.
# Updated:  5/25/18 Alex Bettinardi
#    Updated, now exports connector table as well as link table to be used in the runReport script
# Updated: 5/26/18 Alex Bettinardi
#    Now exports link travel time as an extra field for VHT calculations
# Updated: 10/22/18 Alex Bettinardi
#    Updated to work with Visum 18

#
# Copyright (C) 2003  Oregon Department of Transportation, Innovative Transportation Concepts Inc.
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
# Main Function - write2Visum
# used to so that all the objects generated are removed at the end
# ***************************************************************************************


write2Visum <- function() {

assignment <- paste(getwd(), "/ParameterFiles/hrAssign.xml",sep="")
dAssignment <- paste(getwd(), "/ParameterFiles/dailyAssign.xml",sep="")

# set up link volumes dataSet
# **************************************************************************************
Link.. <- getAttTable(Visum, "Links", c("NO", "FROMNODENO", "TONODENO", "VOLVEHPRT(AP)"))
names(Link..)[c(1,4)] <- c("$LINK:NO", "LAST_RUN")

# set up connector volumes dataSet
# **************************************************************************************
Conn.. <- getAttTable(Visum, "Connectors", c("ZONENO", "NODENO", "DIRECTION", "VOLPERSPRT(AP)"))
#Conn..$DIRECTION <- sapply(Conn..$DIRECTION,  function(x) ifelse(x==1, x <- "O", x <- "D"))  # 5-25-18 AB, removed unnessisary correction step that is no longer used
names(Conn..)[c(1,4)] <- c("$CONNECTOR:ZONENO", "LAST_RUN")

#
#
# Call the function to write and run 24 files and assign in Visum
# ****************************************************************************************
hourNames <- c("12AM", "1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM", "9AM", "10AM", "11AM", "12PM", "1PM", "2PM", "3PM", "4PM", "5PM", "6PM", "7PM", "8PM", "9PM", "10PM", "11PM", "12AM")
# get VISUM matrice list
matrices <- getMatList(Visum)
for(hour in 1:24){
   
   setDSegMatrix(Visum, aMode, trip.dist$hourly.vehicles[,,hour])
   if(vVer>17){ # AB 10-22-18 Updated to work with Visum 18
      setODMatrix(Visum, as.numeric(matrices$Matrices[matrices$Matrices[,"CODE"]==paste("hour", hour, sep="_"),"NO"]), trip.dist$hourly.vehicles[,,hour])      
   } else {   
      setODMatrix(Visum, as.numeric(matrices$ODMatrices[matrices$ODMatrices[,"CODE"]==paste("hour", hour, sep="_"),"NO"]), trip.dist$hourly.vehicles[,,hour])
   }
      
   # run assignment to get hourly volumes
   comInvoke(vbConv("Visum.Procedures"), "Open", assignment)
   comInvoke(vbConv("Visum.Procedures"), "Execute")
   
   # save hourly volumes
   Link..[,hour+4] <- getNetAtt(Visum, "Links", "VOLVEHPRT(AP)")[,2]
   names(Link..)[hour+4] <- paste("VOL", hourNames[hour], hourNames[hour+1], sep="_")
   Conn..[,hour+4] <- getNetAtt(Visum, "Connectors", "VOLPERSPRT(AP)")[,2]  
   names(Conn..)[hour+4] <- paste("VOL", hourNames[hour], hourNames[hour+1], sep="_")
}

   # write out additional peak OD matrices
   for(pu in dimnames(trip.dist$peak.od.purp)[[3]]){
      setDSegMatrix(Visum, pu, trip.dist$peak.od.purp[,,pu])
   }

# perpares the link and zone data to input into visum
# ****************************************************************
   Link..$DAILY_VOLUME <- rowSums(Link..[,5:length(names(Link..))])
   Conn..$DAILY_VOLUME <- rowSums(Conn..[,5:length(names(Conn..))])
   
   # for daily assignment
   setDSegMatrix(Visum, aMode, trip.dist$daily.vehicles)
   if(vVer>17){ # AB 10-22-18 Updated to work with Visum 18
      setODMatrix(Visum, as.numeric(matrices$Matrices[matrices$Matrices[,"CODE"]=="daily","NO"]), trip.dist$daily.vehicles)
   } else {
      setODMatrix(Visum, as.numeric(matrices$ODMatrices[matrices$ODMatrices[,"CODE"]=="daily","NO"]), trip.dist$daily.vehicles)
   }
   
   comInvoke(vbConv("Visum.Procedures"), "Open", dAssignment)
   comInvoke(vbConv("Visum.Procedures"), "Execute")
   Link..$DAILY_ASSIGN <- getNetAtt(Visum, "Links", "VOLVEHPRT(AP)")[,2]
   Link..$VISUM_ID <- getNetAtt(Visum, "Links", "VOLVEHPRT(AP)")[,1]
   Conn..$DAILY_ASSIGN <- getNetAtt(Visum, "Connectors", "VOLPERSPRT(AP)")[,2]
   Conn..$VISUM_ID <- getNetAtt(Visum, "Connectors", "VOLPERSPRT(AP)")[,1]

   # ---------------------------
   # The link and connector attributes to add to VISUM are all ready
   # now just check to see what fields currently exist in VISUM and add the ones that don't exist
   # --------------------------------------------------------------------------------------------
   Lists <- comGetProperty(vbConv("Visum.Net"), "Links")
   AddFieldsFlag <- sapply(names(Link..)[4:(length(names(Link..))-1)], function(x) is.null(comGetProperty(Lists, "GetMultiAttValues", x))) 
   AddFields <- as.data.frame(Link..[,(4:(length(names(Link..))-1))[AddFieldsFlag]])
   names(AddFields) <- names(AddFieldsFlag)[AddFieldsFlag]
   
   # Adding fields that don't exits, currently numeric fields are the only ones coded   
   if(ncol(AddFields)>0) addNetAtt(Visum,"Links",AddFields)

   # add Link data to VISUM
   sapply(names(Link..)[4:(length(names(Link..))-1)], function(x) setNetAtt(Visum,"Links",x,Link..[,c("VISUM_ID",x)],initalFilter=T)) 
   
   # add additional link information and save out in the R space for result comparison (runReport.r)   
   Link..[,1:6+length(Link..)] <- getAttTable(Visum,"Links",c("TSYSSET", "NUMLANES", "CAPPRT", "V0PRT", "PLANNO", "LENGTH"),initalFilter=T)
   Link..$Mode <- as.character(Link..$TSYSSET)
   Link..$Cap <- Link..$CAPPRT / Link..$NUMLANES
   Link..$Cap[is.nan(Link..$Cap)] <- 0
   # 5-26-18 AB - adding Tcur
   Link..$TCUR <- getNetAtt(Visum, "Links", "TCUR_PRTSYS(C)")[,2]
   rownames(Link..) <- paste(Link..$FROMNODENO, Link..$TONODENO, sep="-")
   
   #Save Link data to R workspace
   Link.. <<- Link..
   
   # ---------------------------
   # The connector attribute table
   # --------------------------------------------------------------------------------------------
   Lists <- comGetProperty(vbConv("Visum.Net"), "Connectors")
   AddFieldsFlag <- sapply(names(Conn..)[4:(length(names(Conn..))-1)], function(x) is.null(comGetProperty(Lists, "GetMultiAttValues", x))) 
   AddFields <- as.data.frame(Conn..[,(4:(length(names(Conn..))-1))[AddFieldsFlag]])
   names(AddFields) <- names(AddFieldsFlag)[AddFieldsFlag]

   # Adding fields that don't exits, currently numeric fields are the only ones coded   
   if(ncol(AddFields)>0) addNetAtt(Visum,"Connectors",AddFields)

   # add Connector data to VISUM
   sapply(names(Conn..)[4:(length(names(Conn..))-1)], function(x) setNetAtt(Visum,"Connectors",x,Conn..[,c("VISUM_ID",x)],initalFilter=T))   

   # 5-25-18 Alex Bettinardi
   # add additional connector information and save out in the R space for result comparison (runReport.r)   
   Conn..[,1:2+length(Conn..)] <- getAttTable(Visum,"Connectors",c("TSYSSET",  "LENGTH"),initalFilter=T)
   Conn..$T0 <- getNetAtt(Visum, "Connectors", "T0_TSYS(C)")[,2] 
   rownames(Conn..) <- apply(Conn..[,1:3],1,paste, collapse="-")
   
   #Save Link data to R workspace
   Conn.. <<- Conn..

   # ---------------------------
   # Prepare the zone attributes to be added to VISUM
   # --------------------------------------------------------------------------------------------  
   zone <- as.data.frame(taz.data$taz)
   names(zone) <- "TAZ"
   # Adding addtional zone Information - worker distribution
   wkr.dist <- as.data.frame(round(hh.wkr.dist[as.character(zone$TAZ),]),2)
   names(wkr.dist) <- gsub("wrk", "HW", names(wkr.dist))
   zone <- cbind(zone,wkr.dist)
   # Total Daily Production
   zone$TOT_REG  <- round(apply(trip.prod$regular, 1, sum)[as.character(zone$TAZ)],2)
   if(sum(trip.prod$special) == 0 ){
     zone$TOT_SPEC <- 0
   } else {
     zone$TOT_SPEC <- round(apply(trip.prod$special, 1, sum)[as.character(zone$TAZ)],2)
   }   
   zone$TOT_IE <- round(trip.dist$ext.ie.prod[as.character(zone$TAZ)],2)
   zone$TOT_PROD <- zone$TOT_REG + zone$TOT_SPEC + zone$TOT_IE# Daily Production by Type
   # Daily Production by Type
   prod.Pu <- as.data.frame(round(apply(trip.prod$regular, c(1,3), sum)[as.character(zone$TAZ),],2))
   names(prod.Pu) <- paste("DAILY",toupper(names(prod.Pu)),sep="_")
   zone <- cbind(zone, prod.Pu)
   
   # hourly trip production information
   # Same issue on matching and indexing
   for(hr in colnames(trip.prod$regular)) {
       hr.Pr <- as.data.frame(round(trip.prod$regular[as.character(zone$TAZ),hr,],2))
       zone[,length(zone)+1] <- round(rowSums(hr.Pr),2)
       names(zone)[length(zone)] <- paste("TOT_PROD_HR",hr,sep="_")
       names(hr.Pr) <- paste("HR",hr,toupper(names(hr.Pr)),sep="_")
       zone <- cbind(zone, hr.Pr)
   } # end of hour for loop

   # ---------------------------
   # Add the extra zone attribute table
   # --------------------------------------------------------------------------------------------
   Lists <- comGetProperty(vbConv("Visum.Net"), "Zones")
   AddFieldsFlag <- sapply(names(zone), function(x) is.null(comGetProperty(Lists, "GetMultiAttValues", x))) 
   AddFields <- as.data.frame(zone[,names(AddFieldsFlag)[AddFieldsFlag]])
   names(AddFields) <- names(AddFieldsFlag)[AddFieldsFlag]

   # Adding fields that don't exits, currently numeric fields are the only ones coded   
   if(ncol(AddFields)>0) addNetAtt(Visum,"Zones",AddFields)

   # add the VISUM_ID to the the zone table
   id <- getNetAtt(Visum, "Zones", "TAZ")
   rownames(id) <- id$TAZ
   zone$VISUM_ID <- id[rownames(zone),1]

   # add Link data to VISUM
   sapply(names(zone)[2:(ncol(zone)-1)], function(x) setNetAtt(Visum,"Zones",x,zone[,c("VISUM_ID",x)],initalFilter=T))   

# runs the last calls to visum
# ***************************************************************
   # set the default vehicle demand segment back to peak
   setDSegMatrix(Visum, aMode, trip.dist$hourly.vehicles[,,pkHr])
   
   # write out peak OD matrices by purpose
   # get purposes to load into VISUM
   purposes <- dimnames(trip.dist$peak.od.purp)[[3]]
   purposes <- purposes[purposes %in% unlist(getMatList(Visum)[["DemandSegments"]][,"NAME"])]
   for(pu in purposes){
      setDSegMatrix(Visum, pu, trip.dist$peak.od.purp[,,pu])
   }
   
   # update the assignment procedure depending on which assignment is specified
   if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "MULTICLASSASSIGNMENT"))){   
      if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "MULTICLASSASSIGNMENT"))=="TRUE"){
         assignment <- paste(getwd(), "/ParameterFiles/pkAssign.xml",sep="")
      }
      if(ifelse(is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL")),FALSE, toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "SWIMEXTERNALMODEL"))=="TRUE")){
         if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "MULTICLASSASSIGNMENT"))=="SWIM"){
            assignment <- paste(getwd(), "/ParameterFiles/pkAssign_SWIM.xml",sep="")
         }
      }   
   } 
   
   # run the final peak assignment
   comInvoke(vbConv("Visum.Procedures"), "Open", assignment)
   comInvoke(vbConv("Visum.Procedures"), "Execute")
   
   comInvoke(Visum, "SaveVersion", paste(attr(Visum, "Path"),attr(Visum, "Name"),sep="/"))
}
 
write2Visum()
rm(write2Visum)
gc()
