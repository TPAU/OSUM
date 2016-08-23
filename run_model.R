# run_model.R
# Code Base (C) 2.2: 6-1-16 (AB)

# This script controls the overall running of the small urban area model

# Created: 10/24/02 Brian Gregor brian.j.gregor@odot.state.or.us
# Updated: 11/13/02 Ben Stabler benjamin.stabler@odot.state.or.us
# Updated: 2/14/03 Brian Gregor
# Updated: 3/31/03 Ben Stabler
# Updated: 6/2/03 Ben Stabler
# Updated / file name changed:  9/6/05 Alex Bettinardi    
# Updated: 12/24/2013 Alex Bettinardi for OSUM 2
# Updated: 12/16/2015 Alex Bettinardi for Visum 15
# Updated: 4/13/2016 Alex Bettianrdi to allow for the "data" folder to be specified prior to Model_Inputs.R and future_hhs.R, additionally a series of if statements were added to run the visitor model
# Updated: 6/1/16 Alex Bettinardi - corrected spelling of "vistorModel" due to edits needs in trip_distribution.r

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
# Load a number of basic functions to make model operations easier.

# Clear Work Space
rm(list=ls(all=TRUE))

# Load the R functions for OSUM processing
source("rcode\\load_model_functions.R")

# Define the Visum version to be used - AB, 12-16-15
vVer <- 15 # The current release of OSUM uses Visum 15 

# load and open VISUM connection
Visum <- getFile()

# Read all input data (model parameters, traffic inputs, socioeconomic inputs and VISUM travel times).
dataDir <- "data" # adding in the location of the data folder 4-12-16 AB - to better handle new Newport logic.
source("rcode\\Model_Inputs.R")


# run inputCheck.R
serverLocalSource("\\\\s6000e\\6420only\\Tools\\ModelingTools\\OSUM\\inputCheck.R", "rcode\\inputCheck.R")

# opitional step to run a pre processor for future household characteristics (adjust household demographics from the base)
# Requires census_block.csv be in the data folder and futrAvgHHsize be stored in the version file
# Additionally, the AHHS size zone field needs to be populated
# Lastly, only if the "orig" logic is used, the zone attribute - HHREF (base year households) needs to be included
if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "FUTURE_HH_PREPROCESSOR"))){
   futureHHSflag <- tolower(comGetProperty(vbConv("Visum.Net"), "AttValue", "FUTURE_HH_PREPROCESSOR"))
      if(futureHHSflag %in% c("new", "orig")){            
         futrAvgHHsize <- comGetProperty(vbConv("Visum.Net"), "AttValue", "FUTRAVGHHSIZE")
         source("rcode\\future_hhs.R")
         rm(futrAvgHHsize)
      }
      rm(futureHHSflag)
   }
rm(dataDir)
 
# Calculate household data needed for trip generation.
source("rcode\\hhsubmodel.R")

# Calculate trip generation by trip purpose and hour of the day.
source("rcode\\trip_generation.R")

# if VISITORMODEL - run stage 1 of the visitor model
if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORMODEL"))){
   if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORMODEL"))=="TRUE"){
   
   vStage = 1 # runs the first phase of the visitor model (inputs and trip generation)
   source("rcode\\visitorModel.R") 

}}

save.TT(amPk,init=T)

# While loop to run trip distribution to convergence
# Set starting points for the closure measures
RMSE <- 3
aChange <- 1
iter <- 0

while(any(c(RMSE[length(RMSE)] > 1, aChange[length(aChange)] > .01)) & iter < 10 ){

   iter <- iter + 1
   # Calculates peak and off-peak trip distribution.
   if(!exists("tTime.ZnZn")){
      tTime.ZnZn <- pk.time
   } else {
      if(nrow(tTime.ZnZn) != nrow(pk.time)){
        tTime.ZnZn <- pk.time
      } else {
        pk.time <- (tTime.ZnZn + pk.time)/2
        tTime.ZnZn <- pk.time
      }  
    }
    dataDir <- "data" # 4-13-16 AB - to accomidate visitors model
    source("rcode\\trip_distribution.R")
    rm(dataDir)
    
    # if VISITORMODEL - run stage 2 of the visitor model
    if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORMODEL"))){
       if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORMODEL"))=="TRUE"){
       
          vStage = 2 # runs the first phase of the visitor model (inputs and trip generation)
          source("rcode\\visitorModel.R") 
    }}
    
    save.TT(amPk)

    tt.new <- rowSums(pk.time)

    if(!exists("tt.old")) { tt.old <- tt.new*.9; RMSE <- c(); aChange <- c()}
    if(length(tt.old) != length(tt.new)) { tt.old <- tt.new*.9; RMSE <- c(); aChange <- c()}
    RMSE <- c(RMSE, calcRmse(tt.old, tt.new))
    aChange <- c(aChange, max(abs(tt.new-tt.old)[tt.old !=0 ]/tt.old[tt.old != 0]))
    cat(paste("iter=", iter, "RMSE=", signif(RMSE[iter],5), "Absolute Travel Time Change=", signif(aChange[iter],5),"\n"))
    tt.old <- tt.new
} # end the closure while loop
rm(tt.old)

# if VISITORMODEL - run stage 3 of the visitor model
if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORMODEL"))){
   if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORMODEL"))=="TRUE"){
   
   vStage = 3 # runs the first phase of the visitor model (inputs and trip generation)
   source("rcode\\visitorModel.R") 

}}

# Write matrices, link, and zone infroamtion to VISUM
source("rcode\\write_2_VISUM.R")

# before saving dettach VISUM functions and clean space
rm(tt.new, tTime.ZnZn)
save.image(file=gsub("\\.ver$", "\\.Rdata", paste(attr(Visum, "Path"),attr(Visum, "Name"),sep="/")))

# run runReport.R - model run assessment and documentation (if reference location exists)
if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "REFERENCE"))){
if(file.exists(comGetProperty(vbConv("Visum.Net"), "AttValue", "REFERENCE"))){
   cat("\nRunning Comparison to Reference Reporting Script - runReport.R\n")
   serverLocalSource("\\\\s6000e\\6420only\\Tools\\ModelingTools\\OSUM\\runReport.R", "rcode\\runReport.R")
} else {
   cat(paste("\nThe reference file coded in the version file under\nNetwork Parameters -> User-Defined Attributes does not exist.\nTherefore the a comparsion to the given reference was not made.\nThis is the refernce file coded (Note - there is a 255 char max limit):\n",comGetProperty(vbConv("Visum.Net"), "AttValue", "REFERENCE"),"\n\n"))
}}
