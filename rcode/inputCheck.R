# inputCheck.R
# Code Base (C) 2.2.1: 4-25-16 (AB)

# author Alex Bettinardi
# version 2.0
# date 12/24/13 - Merry Christmas Eve
# Revised 1-15-14 AB, correct Capacity check (was only checking high side, not low side)
# Revised 1-23-14 AB, correct Capacity check to allow any value in PLANNO, also added a PLANNO (FC)check
# Revised 2-10-14 AB, added special generator checks
# Revised 1-14-16 AB, Added an external model check to try and verify that the e-e matrix can be balanced.
#                     Also improved the "max speed" check to allow for custom max speed per area (for new 75 mph increases), 
#                     and to lower the max speed from 65mph to 55mph where there is no interstates present.
# Revised 4-1-16 AB,  realized that the user has to code both the "NO" and "TAZ" field when adding a zone.
#                     Building in a check to double check that those fields are identical.
# Revised 4-25-16 AB, Adding a check to verify that Total employment is always greater than retl + serv
#
######################################################
# Checks Inputs for Coding Errors and Inconsitentcies.
######################################################

   ePopUp <- function(eText) {

           #  Only run interactive buttons if wantQ=T
           if(wantQ){
                     
                    eQ <- tktoplevel()
                    e1 <- tklabel(eQ, text=paste(eText,collapse="\n"))
                    NoP <- tkbutton(eQ, text="Close Warning")
                    blank <- tklabel(eQ, text="") 
                    Stop <- tklabel(eQ, text="Press ESC to STOP the OSUM run")
                    blank2 <- tklabel(eQ, text="") 
                    tkpack(e1, blank, Stop, blank2, NoP)
                    #  Function that reacts to user pressing "NoP"

                    NOP <- function() {
                           tkdestroy(eQ)                                       
                        }

                    tkconfigure(NoP, command = NOP)
                    
            }# end of if wantQ statement
            
            if(file.exists("errorReport.txt")) {
               error <- readLines("errorReport.txt")
            } else {
               error <- c()
            }
            error <-c(date(), "",eText, "","", error)
            writeLines(error, "errorReport.txt")
            
   } # end of axisOpt question

# special flag to make this script work with OSUM V1 and V2
V2Flag <- F
if(!exists("scenario")){
   V2Flag <- T
   scenario <- "base"
} 

# Warning if Block Group numbers are NA
if(!is.null(taz.data$bgbase)){
if(any(is.na(taz.data$bgbase))) {
   ePopUp(c("The Following Block Groups are not Populated", taz.data$taz[is.na(taz.data$bgbase)]))
}
# Warning if Block Group numbers are all the same
if(length(unique(taz.data$bgbase))<3) {
   ePopUp(c("It looks like you have lost your Block Group data", " - Excel will ruin us all"))
}
}

# cutting out legacy AHHS checks - this field is now calculated by the code
if(F){
# Warning if average house size is less than 1
if(any(taz.data$ahhsbase<1 & taz.data[,paste("hh", scenario, sep="")]>0)) {
   ePopUp(c("Check the Following Zones (ahhs is less than 1.0):", taz.data$taz[taz.data$ahhsbase<1 & taz.data[,paste("hh", scenario, sep="")]>0]))
}
# Check for average household size that is more than 10% different than household size distribution
index <- (abs(taz.data$ahhsbase - colSums(t(taz.data[,paste("hhs",1:4, "base", sep="")])*1:4))/ taz.data$ahhsbase) > 0.1
if(any(index)) {
   ePopUp(c("There is an Avg HH Size and Size Distribution mis-match in the Following Zones", "TAZ: AHHS, Min AHHS Calculated", paste(taz.data$taz[index], ": ", round(taz.data$ahhsbase[index],2), ", ", round(colSums(t(taz.data[,paste("hhs",1:4, "base", sep="")])*1:4)[index],2) ,sep="")))
}
rm(index)
}

# Warning if income distribution does not sum to 100%
if(any(round(rowSums(taz.data[,paste("hhi",1:4, "base", sep="")])*10)!=10)) {
   ePopUp(c("Check Income Distribution in the Following Zones (they don't sum to 100%)", taz.data$taz[round(rowSums(taz.data[,paste("hhi",1:4, "base", sep="")])*10)!=10]))
}
# Warning if size distribution does not sum to 100%
if(any(round(rowSums(taz.data[,paste("hhs",1:4, "base", sep="")])*10)!=10)) {
   ePopUp(c("Check Size Distribution in the Following Zones (they don't sum to 100%)", taz.data$taz[round(rowSums(taz.data[,paste("hhs",1:4, "base", sep="")])*10)!=10]))
}

# Warning if employment by industry doesn't match total
# Only run for V1 of OSUM
if(!V2Flag){
if(any(rowSums(taz.data[,paste(c("agri", "indy", "retl", "serv", "educ", "govt", "othr"),scenario, sep="")])!= taz.data[,paste("emp", scenario, sep="")])) {
   ePopUp(c("Total Emp and Emp-by-Industry don't match for these Zones", taz.data$taz[rowSums(taz.data[,paste(c("agri", "indy", "retl", "serv", "educ", "govt", "othr"),scenario, sep="")])!= taz.data[,paste("emp", scenario, sep="")]]))
}

# Warning if total employment is less than retl + serv - 4-25-16
if(any(rowSums(taz.data[,paste(c("retl", "serv"),scenario, sep="")]) > taz.data[,paste("emp", scenario, sep="")])) {
   ePopUp(c("Total Emp is less than retl+serv for these Zones", taz.data$taz[rowSums(taz.data[,paste(c("retl", "serv"),scenario, sep="")])> taz.data[,paste("emp", scenario, sep="")]]))
}

}
# Warning if districts and district.bias don't match
if(any(sort(unique(taz.data$district)) != sort(as.numeric(names(district.bias[[1]]))))){
   ePopUp(c("Districts coded in taz.data don't match dist.bias input", "taz.data", unique(taz.data$district), "dist.bias", sort(as.numeric(names(district.bias[[1]])))))
}

# Warning to give user warning that not enough employment (or HH or sche) is availalbe for Spec Gen
if(exists("spec.gen")){
   SpecEmpCheck <- sapply(c("retlbase", "servbase", "empbase", "hhbase", "schebase"), function(x) tapply(taz.data[as.character(spec.gen$zone),x], spec.gen$zone, sum))
   SpecEmpCheck[,"empbase"] <- SpecEmpCheck[,"empbase"] - rowSums(SpecEmpCheck[,c("retlbase", "servbase")])
   colnames(SpecEmpCheck) <- c("retl.emp", "serv.emp", "othr.emp", "hh", "sch.enrl")
   SpecEmpCheck <- SpecEmpCheck - sapply(c("retl.emp", "serv.emp", "othr.emp", "hh", "sch.enrl"), function(x) tapply(spec.gen[,x], spec.gen$zone, sum))
   for(tz in rownames(SpecEmpCheck)){
      if(any(SpecEmpCheck[tz,] <0)){
         ePopUp(c(paste("Spec Gen:", tz, "has more Employment Coded than Available"), "Coded in Spec Gen:",
                  paste("retl.emp", "serv.emp", "othr.emp", "hh", "sch.enrl", sep=", "),
                  paste(spec.gen[spec.gen$zone==as.numeric(tz),c("retl.emp", "serv.emp", "othr.emp", "hh", "sch.enrl")], collapse=",          "),
                  "","Coded in TAZ fields",
                  paste("emp","retl", "serv", "hh","sche", sep=", "),
                  paste(taz.data[tz,c("empbase","retlbase", "servbase", "hhbase", "schebase")], collapse=",  ")))
      }
   }
   rm(SpecEmpCheck,tz)
}
# set of network checks for OSUM V2
if(exists("getAttTable")){
   # Get network information to check
   Net.. <- getAttTable(Visum,"Links",c("NO","FROMNODENO", "TONODENO", "TSYSSET", "NUMLANES", "CAPPRT", "V0PRT", "PLANNO"),initalFilter=T)
   Net..$Mode <- as.character(Net..$TSYSSET)
   Net..$Cap <- Net..$CAPPRT / Net..$NUMLANES
   Net..$Cap[is.nan(Net..$Cap)] <- 0
   Net.. <- Net..[Net..$Mode == aMode,]
   Net..$LinkID <- paste(Net..$NO,": ", Net..$FROMNODENO, "-", Net..$TONODENO, ": ", Net..$PLANNO, ", ", Net..$NUMLANES, ", ", Net..$Cap, ", ", Net..$V0PRT, sep="")
   Net..$Flag <- (Net..$V0PRT > 0) & (Net..$Cap == 0) 
   if(sum(Net..$Flag)>0){
      ePopUp(c("The Following Links Look like they should have capacity, but don't.", "Link #: FromNode-ToNode: Functional Class, #Lanes, Per Lane Capacity, MPH", Net..$LinkID[Net..$Flag]))
   }
   # number of lanes check
   Net..$Flag <- Net..$NUMLANES > 3
   if(sum(Net..$Flag)>0){
      ePopUp(c("The Following Links have more then 3 lanes a direction - Really!!!", "Link #: FromNode-ToNode: Functional Class, #Lanes, Per Lane Capacity, MPH", Net..$LinkID[Net..$Flag]))
   }
   # basic speed checks
   minSpeed <-comGetProperty(vbConv("Visum.Net"), "AttValue", "NETCHECKMINSPEED")
   if(is.null(minSpeed)) minSpeed <- 20
   Net..$Flag <- Net..$V0PRT < minSpeed
   if(sum(Net..$Flag)>0){
      ePopUp(c(paste("The Following Links have a Speed less than", minSpeed, "MPH."), "Link #: FromNode-ToNode: Functional Class, #Lanes, Per Lane Capacity, MPH", Net..$LinkID[Net..$Flag]))   
   }
   rm(minSpeed)
   maxSpeed <-comGetProperty(vbConv("Visum.Net"), "AttValue", "NETCHECKMAXSPEED")
   if(is.null(maxSpeed)) maxSpeed <- ifelse(sum(Net..$PLANNO==1)>0,65,55)
   Net..$Flag <- Net..$V0PRT > maxSpeed
   if(sum(Net..$Flag)>0){
      ePopUp(c(paste("The Following Links have a Speed greater than", maxSpeed, "MPH."), "Link #: FromNode-ToNode: Functional Class, #Lanes, Per Lane Capacity, MPH", Net..$LinkID[Net..$Flag]))   
   }
   rm(maxSpeed)
   
   # functional class check, should be 1-5 or 30
   Net..$Flag  <- !Net..$PLANNO %in% c(1:5,30)
   if(sum(Net..$Flag)>0){
      ePopUp(c("The Following Links have a Functional Class Other than 1,2,3,4,5, or 30", "Link #: FromNode-ToNode: Functional Class, #Lanes, Per Lane Capacity, MPH", Net..$LinkID[Net..$Flag]))
   } 
   # Capacity Checks by speed
   Net..$Flag <- Net..$V0PRT < 30 & Net..$Cap > 1000
   if(sum(Net..$Flag)>0){
      ePopUp(c("The Following Links have a speed less than 30mph and a Capacity greater than 1000.", "Link #: FromNode-ToNode: Functional Class, #Lanes, Per Lane Capacity, MPH", Net..$LinkID[Net..$Flag]))
   }   
   # capacity check by functional class
   Net..$PLANNO[!Net..$PLANNO %in% c(1:5,30)] <- 0
   Net..$PLANNO <- as.character(Net..$PLANNO)
   upperLimit <- c(1900, 950, 760, 650, 625, 1000, 1900)
   lowerLimit <- c(1800, 700, 575, 450, 400, 700, 400)
   names(upperLimit) <- names(lowerLimit) <- c(1:5,30, 0)
   Net..$Flag <- Net..$Cap > upperLimit[Net..$PLANNO] | Net..$Cap < lowerLimit[Net..$PLANNO]                              
   if(sum(Net..$Flag)>0){
      ePopUp(c("The Following Links have a capacity that is outside the OSUM defined range", "See: \\Instructions\\  Attribution_ReferenceSheet.xls", "Link #: FromNode-ToNode: Functional Class, #Lanes, Per Lane Capacity, MPH", Net..$LinkID[Net..$Flag]))   
   }
   
   # 4-1-16 Check that "NO" and "TAZ" zone fields are identical, required
   Zone.. <- getAttTable(Visum,"Zones",c("NO","TAZ"),initalFilter=T)
   Check <- Zone..$NO != Zone..$TAZ
   if(sum(Check)>0){
      ePopUp(c("The TAZ field and the NO Field do not match (required) for Following Zones",Zone..$NO[Check]))
   }
   rm(Net.., upperLimit, lowerLimit, Zone.., Check)
}


# check to determine if EE matrix can be balanced 
# (you can't have one external station that has a larger EE volume than all other external locations combined).
eeVol <- ext.traffic[,"adt"] * ext.traffic[,"ee.pct"]
if(max(eeVol)>(sum(eeVol)-max(eeVol))){
      ePopUp(c(paste("External Station -",names(eeVol)[eeVol==max(eeVol)] ,"- has more EE volume than all other external stations combined"),
               paste("Here is the EE Volume for Station -",names(eeVol)[eeVol==max(eeVol)], "=",round(max(eeVol))),
               paste("All other stations have a combined EE Volume of -",round(sum(eeVol)-max(eeVol)))))   
}
rm(eeVol)

# clean up special V1 / V2 flag
if(V2Flag) rm(scenario)
rm(V2Flag)
