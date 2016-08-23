# visitorModel.R
# Code Base (C) 2.2: 6-1-16 (AB)

# Written by Alex Bettinardi
# April 18, 2016
# Re-run OSUM for a visitors subset of traverls if VISITORMODEL = TRUE.

#######################################
#  visitor model for Newport
#######################################

# The visitor model runs in three "Stages" 
# 1. inputs and generation (done once, doesn't iterate)
# 2. trip distribution - iterates
# 3. combines 2 remaining files for comparisons to a reference case and runs a instance of Visum with just the visitors


##########################
# Stage 1 - inputs and trip generation

if(vStage == 1){
   
      rm(vStage)
      # first save everything in the working directory with the exception of the functions and the Visum connection
      workspaceKeepList <- c("workspaceKeepList","ePopUp","osumFun", "serverLocalSource", "VisFun", "Visum")
      save(list=ls()[!(ls() %in% workspaceKeepList)], file="data/residents.Rdata")

      # Clear Work Space of standard OSUM (residence) results
      rm(list=ls()[!(ls() %in% workspaceKeepList)]) 
      
      # re run OSUM with visitors as opposed to residents
      
      # Read all input data (model parameters, traffic inputs, socioeconomic inputs and VISUM travel times).
      dataDir <- "data_visitor" # adding in the location of the data folder 4-12-16 AB - to better handle new Newport logic.
      visitorModel <- TRUE
      source("rcode\\Model_Inputs.R")

      hh.trip.rate <- comGetProperty(vbConv("Visum.Net"), "AttValue", "V_HHTRIPRATE") 
      if(is.null(hh.trip.rate)) hh.trip.rate <- 0                      
            
      if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "V_FUTURE_HH_PREPROCESSOR"))){
         futureHHSflag <- tolower(comGetProperty(vbConv("Visum.Net"), "AttValue", "V_FUTURE_HH_PREPROCESSOR"))
         if(futureHHSflag %in% c("new", "orig")){            
            futrAvgHHsize <- comGetProperty(vbConv("Visum.Net"), "AttValue", "V_FUTRAVGHHSIZE")
            source("rcode\\future_hhs.R")
            rm(futrAvgHHsize)
         }
         rm(futureHHSflag)
      }
      rm(dataDir,visitorModel)
 
      # Calculate household data needed for trip generation.
      source("rcode\\hhsubmodel.R")

      # Calculate trip generation by trip purpose and hour of the day.
      source("rcode\\trip_generation.R")
      
      # save the visitor workspace
      save(list=ls()[!(ls() %in% workspaceKeepList)], file="data_visitor/visitors.Rdata")
      
      # Clear Work Space of visitor results
      rm(list=ls()[!(ls() %in% c(workspaceKeepList))])
      
      # load orginal (resident) data back into workspace
      load("data/residents.Rdata")
      
      rm(workspaceKeepList)

}  # end of visitor model for Newport Stage 1

########################
# Stage 2 - distribution

if(exists("vStage")){
if(vStage == 2){
   
      rm(vStage)
      # first save everything in the working directory with the exception of the functions and the Visum connection
      workspaceKeepList <- c("workspaceKeepList","visitorModel","pk.time","offpk.time","ePopUp","osumFun", "serverLocalSource", "VisFun", "Visum")
      save(list=ls()[!(ls() %in% workspaceKeepList)], file="data/residents.Rdata")

      # Clear Work Space of standard OSUM (residence) results
      rm(list=ls()[!(ls() %in% workspaceKeepList)]) 

      # load visitor data back into workspace
      load("data_visitor/visitors.Rdata")

      # special statement to overwrite pk travel skims with offpeak, if the user has choosen to 
      # have the visitors not care about congestion in their travel routing.
      if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORS_CONSIDER_CONGESTION"))){
         if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORS_CONSIDER_CONGESTION")) == "FALSE"){      
            save.pk.time <- pk.time
            pk.time <- offpk.time
      }}
                     
      # Run Trip distribution for the visitors
      dataDir <- "data_visitor"
      visitorModel <- TRUE
      source("rcode\\trip_distribution.R")
      rm(dataDir,visitorModel)
      vtrip.dist <- trip.dist
      
      if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORS_CONSIDER_CONGESTION"))){
         if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "VISITORS_CONSIDER_CONGESTION")) == "FALSE"){      
            pk.time <- save.pk.time
            rm(save.pk.time)
      }}
      
      # save the visitor workspace
      save(list=ls()[!(ls() %in% c(workspaceKeepList,"vtrip.dist"))], file="data_visitor/visitors.Rdata")
      
      # Clear Work Space of visitor results
      rm(list=ls()[!(ls() %in% c(workspaceKeepList,"vtrip.dist"))])
      
      # load orginal (resident) data back into workspace
      load("data/residents.Rdata")
      
      # update all the trip dist arrays and matrices to include both residents and visitor travel
      for(n in names(trip.dist))  trip.dist[[n]] <- trip.dist[[n]] + vtrip.dist[[n]]
      
      # clean workspace
      rm(workspaceKeepList,n,vtrip.dist)        
      
}} # end of phase 2      
      
if(exists("vStage")){      
if(vStage == 3){      
      
      rm(vStage)
      # first save everything in the working directory with the exception of the functions and the Visum connection
      workspaceKeepList <- c("workspaceKeepList","visitorModel","pk.time","offpk.time","ePopUp","osumFun", "serverLocalSource", "VisFun", "Visum")
      save(list=ls()[!(ls() %in% workspaceKeepList)], file="data/residents_temp.Rdata")

      # Clear Work Space of standard OSUM (residence) results
      rm(list=ls()[!(ls() %in% workspaceKeepList)]) 

      # load visitor data back into workspace
      load("data_visitor/visitors.Rdata")

      ###################
      # extra option to allow a user to see the visitors routed on the network. 
      if(!is.null(comGetProperty(vbConv("Visum.Net"), "AttValue", "SAVE_VISITOR_ROUTING"))){
         if(toupper(comGetProperty(vbConv("Visum.Net"), "AttValue", "SAVE_VISITOR_ROUTING")) == "TRUE"){ 
                          
         # Write matrices, link, and zone infroamtion to VISUM
         source("rcode\\write_2_VISUM.R")
         file.copy(paste(attr(Visum, "Path"),attr(Visum, "Name"),sep="/"),paste(attr(Visum, "Path"),gsub("\\.ver$", "_visitorResult.ver",attr(Visum, "Name")),sep="/"),overwrite=T)
      }}
      
      
      # save out important files for the runReport comparison scipt
      vhh.wkr.dist <- hh.wkr.dist
      vtrip.prod <- trip.prod
      vext.traffic <- ext.traffic[,"adt"]
      
      # Clear Work Space of visitor results
      rm(list=ls()[!(ls() %in% c(workspaceKeepList, "vhh.wkr.dist","vtrip.prod", "vext.traffic"))])
      
      # load orginal (resident) data back into workspace
      load("data/residents_temp.Rdata")
      file.remove("data/residents_temp.Rdata")      
      
      # update all the trip dist arrays and matrices to include both residents and visitor travel
      hh.wkr.dist <- hh.wkr.dist + vhh.wkr.dist
      for(n in names(trip.prod))  trip.prod[[n]] <- trip.prod[[n]] + vtrip.prod[[n]]
      ext.traffic[,"adt"] <- ext.traffic[,"adt"] + vext.traffic
      
      # clean workspace
      rm(workspaceKeepList,n,vhh.wkr.dist,vtrip.prod)     
}}      
      