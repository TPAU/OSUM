#
#VISUM FUNCTIONS
#   
#:Author: Martin Mann
#:Credit: Ben Stabler (provided some of the original code constructs)
#:Contact: martin.a.mann@odot.state.or.us
#:Date: 6/20/2011
#:Updated - 8/1/2013 Alex Bettinardi to work with VISUM 13 for OSUM applications 
#:Updated - 9/18/2014 by Alex Bettinardi to work with VISUM 14
#:Updated - 1/5/16 by Alex Bettinardi to work with VISUM 15
#:Updated - 1/13/16 by Alex Bettinardi to properly point project directories to the gpa and par locations
#:Updated - 1/8/17 by Alex Bettinardi to block meaningless warnings and messages when loading libraries - rcom
#:Updated - 10/22/18 by Alex Bettinardi to work with Visum 18
#:Updated - 12/5/18 by Alex Bettinardi to remove the "POOP" reference 
#:Copyright: 2009, Oregon Department of Transportation
#:License: GPL2
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.  
#
#DESCRIP: Provides functions for accessing data from visum
#  
########################################################## DETAILS #############################################################
#
#   These are a collection of functions that allow a person to open a visume verion file and pull different types of network and 
#   zonal tables and OD and skim matrices without knowing how to make COM calls using the rcom.
#   leave file on desktop and use the following lines in your script:
              #library(rcom) 
              #source("VisFun_05_15_12.r") include path if needed
              #attach(VisFun)
# 

# load library 
options(warn=-1)
suppressMessages(library(rcom))
options(warn=0)
# and set ODOT license
try(comSetLicenseInfo("R","ODoT"), silent=T)  

# 1-5-16 - AB
# From this point on the user will have to specify which version they want to use
# with the object vVer
if(!exists("vVer")) vVer <- 15 

######################################################### START  ###############################################################

    VisFun<-list()
    
################################################ FUNCTIONS CALLED BY USER ####################################################
                     
               
    #Opens Visum Ver file and set Project Directories
    #PARAMETERS: optional file path
    #EXAMPLE: Visum <- getFile() 
    #OUTPUT: A COM object with the version file name and path as attributes
    #NOTE:
    VisFun$getFile <- function(fPath=NULL) {
                  if(is.null(fPath))fPath <- choose.files(default="*.ver", caption="Visum Version File", multi=F, filters = c("ver"))
                  stringLen <- length(strsplit(fPath,"\\\\|/")[[1]])
                  vFP <- paste(strsplit(fPath,"\\\\|/")[[1]][1:(stringLen-1)], collapse="/")
                  vF <- paste(strsplit(fPath,"\\\\|/")[[1]][stringLen], collapse="/")
                  temp <- OpenVisum(vFP,vF)
                  attr(temp, "Path") <- vFP
                  attr(temp, "Name") <- vF                  
                  temp
                }
                
    #Saves Visum Version file           
    #PARAMETERS:Visum Object, New file name and/or Path(optional, must include full path and name)
    #EXAMPLE:(Visum,"ModifedDCM.ver")
    #OUTPUT:NONE
    #NOTE:               
    VisFun$saveVisum <- function(visObj,newName=NULL) {
                  if(!is.null(newName)) {
                      comInvoke(visObj, "SaveVersion",newName)
                  }else{
                      comInvoke(visObj, "SaveVersion", paste(attr(visObj,"Path"),attr(visObj,"Name"),sep="/")) 
                  } 
                }
                                
##################################################### NET ATTRIBUTES ########################################################                
    
    
    #Creates a dataframe for Zone, Link or Connector attributes
    #PARAMETERS:Visum Object, Net object Name(eg "Zones","Links","Connectors","Nodes"), Attribute Names
    #EXAMPLE:(Visum,"Zones",c("NO","HHBASE","EMPBASE")) 
    #OUTPUT: a data frame with for the network object with the requested fields
    #NOTE: Works with one net object at a time            
    VisFun$getAttTable <- function(visObj,netObjName,fields,initalFilter=T) {
                  if(initalFilter) comInvoke(comGetProperty(visObj,"Filters"), "InitAll")
                  index <- match(netObjName,c("Zones","Links","Connectors","Nodes"))
                  listType <- c("CreateZoneList","CreateLinkList","CreateConnectorList","CreateNodeList")                               
                  listObj <- comGetProperty(comGetProperty(visObj,"Lists"), listType[index])
                  comInvoke(listObj, "Show")          
                  sapply(fields,function(x)comInvoke(listObj, "AddColumn", toupper(x)))   # 12-5-18 AB, removed the "poop" reference
                  SFA <- comInvoke(listObj,"SaveToArray")
                  listObj <- NULL
                  rm(listObj)
                  gc()
                  if(dim(SFA)[2]%%length(fields)!=0 ) stop("One of the requested fields may not exist")      
                  DF <- SFAtoTable(SFA,fields)
                  return(DF)
                }
    
    #Creates a dataframe for POI attributes
    #PARAMETERS:Visum Object, POI Object Number, POI Attribute Names
    #EXAMPLE:(Visum1, 27, c("TAZ","ROOMS")) 
    #OUTPUT: a data frame with for the network object with the requested fields 
    #NOTE: Works with one POI at a time           
    VisFun$getPOITable <- function(visObj,itemNo,fields) {                           
                  listObj <- comGetProperty(comGetProperty(visObj,"Lists"),"CreatePOIList")
                  comObj <- comGetProperty(comGetProperty(visObj,"Net"),"POICategories")  
                  comObj <- comGetProperty(comObj,"ItemByKey",itemNo)         
                  comInvoke(listObj,"SetObjects",FALSE,comObj)                 
                  sapply(fields,function(x)comInvoke(listObj, "AddColumn", toupper(x))) 
                  SFA <- comInvoke(listObj,"SaveToArray")
                  if(dim(SFA)[2]%%length(fields)!=0 ) stop("One of the requested fields may not exist")      
                  DF <- SFAtoTable(SFA,fields)
                  return(DF)
                }  
                                 
    #Returns a dataframe with net ID and a single net attribute for Zone, Link or Connector objects
    #PARAMETERS:Visum Object, Net object name:("Zones","Links","Connectors","Nodes","POICategories"), Attribute Name
    #EXAMPLE:(Visum1,"Zones","EMPBASE") 
    #OUTPUT: a data frame with for the network object with the requested field                  
    VisFun$getNetAtt <- function(visObj,netObjName,attName,itemNo=NULL,initalFilter=F) {
    		          if(initalFilter) comInvoke(comGetProperty(visObj,"Filters"), "InitAll")
                  comObj <- comGetProperty(comGetProperty(visObj,"Net"),netObjName)
                  if(netObjName=="POICategories"){
                      comObj <- comGetProperty(comObj,"ItemByKey",itemNo)
                      comObj <- comGetProperty(comObj, "POIs")                   
                  }
                  SFA <- comGetProperty(comObj, "GetMultiAttValues", attName)
                  DF <- SFAtoTable(SFA,c("ID",attName))
                  return(DF)
                }
                
    #Modifies a single Ver file net attribute for Zone, Link or Connector objects.
    #PARAMETERS:Visum Object, Net object name("Zones","Links","Connectors","POICategories"), Attribute, New Attribute dataFrame)
    #EXAMPLE:(Visum1,"Zones","EMPBASE", newValuesTable ) 
    #OUTPUT: None
    #NOTE: New values table must contain a column with the ID and one with 
    #      the attribute value as found when in the output of getNetAtt()
    VisFun$setNetAtt <- function(visObj,netObjName,attName,newValues,itemNo=NULL,initalFilter=F) {
                  newValues <- newValues[order(newValues[,1]),]
                  if(initalFilter) comInvoke(comGetProperty(visObj,"Filters"), "InitAll")
                  comObj <- comGetProperty(comGetProperty(visObj,"Net"),netObjName)
                  if(netObjName=="POICategories"){
                      comObj <- comGetProperty(comObj,"ItemByKey",itemNo)
                      comObj <- comGetProperty(comObj, "POIs")                   
                  }
                  SFA <- TabletoSFA(newValues)
                  comInvoke(comObj, "SetMultiAttValues",attName,SFA)
                }

    #Add multiple attributes read from a dataFrame
    #PARAMETERS:Visum Object,Net object name("Zones","Links","Connectors","POICategories"),dataframe of new attributes)
    #EXAMPLE:(Visum1,"Zones","EMPBASE",dataFrame) 
    #OUTPUT: None
    #NOTE: dataFrame must contain only attributes you wish to add
    VisFun$addNetAtt <- function(visObj,netObjName,DF,itemNo=NULL) {
                  attTable <- BldAttTable(DF)
                  comObj <- comGetProperty(comGetProperty(visObj,"Net"),netObjName)
                  if(netObjName=="POICategories"){
                      comObj <- comGetProperty(comObj,"ItemByKey",itemNo)
                      comObj <- comGetProperty(comObj, "POIs")                   
                  }
                  for(i in 1:nrow(attTable)) {
                      curAtt <- attTable[i,]                      
                      comInvoke(comObj,"AddUserDefinedAttribute",curAtt[[1]],curAtt[[2]],curAtt[[3]],curAtt[[4]],curAtt[[5]])    
                  }
                }      
                           
    #Returns a dataframe with net ID and a single net attribute for Zone, Link or Connector objects
    #PARAMETERS:Visum Object,Net object name:("Zones","Links","Connectors","Nodes","POICategories"),Attribute name
    #EXAMPLE:(Visum1,"Zones","EMPBASE") 
    #OUTPUT: a data frame with for the network object with the requested field                  
    VisFun$delNetAtt <- function(visObj,netObjName,attVec,itemNo=NULL) {
                  comObj <- comGetProperty(comGetProperty(visObj,"Net"),netObjName)
                  if(netObjName=="POICategories"){
                      comObj <- comGetProperty(comObj,"ItemByKey",itemNo)
                      comObj <- comGetProperty(comObj, "POIs")                   
                  }
                  sapply(attVec,function(x) comInvoke(comObj, "DeleteUserDefinedAttribute", x))
                }
                

################################################### MATRICES ################################################################                

    #Retrieves a list of OD/Skim Matrices for a Version file 
    #PARAMETERS:Visum object
    #EXAMPLE:(Visum1) 
    #OUTPUT: list of OD/Skim Matrices dataframes with name and number              
    #NOTE:
    VisFun$getMatList <- function(visObj) {
                  matList_ <- list()
                  matList. <- c("ODMatrices", "SkimMatrices","DemandSegments")  # AB 10-22-18 list changed from Visum 16 to 18
                  if(!exists("vVer")) vVer <- 16 # AB 10-22-18 Updated to work with Visum 18
                  if(vVer > 17) matList. <- c("Matrices","DemandSegments") 
                  for(matType in matList.) {
                      MatObj <- comGetProperty(comGetProperty(visObj,"Net"),matType)
                      if(matType == "DemandSegments"){
                         temp <- do.call(cbind, lapply(c("CODE","NAME"),function(x)comGetProperty(MatObj,"GetMultiAttValues",x)[,2]))
                         colnames(temp) <- c("CODE","NAME")
                      } else {
                         temp <- do.call(cbind, lapply(c("NO","CODE","NAME"),function(x)comGetProperty(MatObj,"GetMultiAttValues",x)[,2]))
                         colnames(temp) <- c("NO","CODE","NAME")
                      }
                      matList_[[matType]] <- temp 
                  }
                  return(matList_ )
                }
                
    #Retrieves a specified OD Matrix
    #PARAMETERS:Visum object, Matrix number
    #EXAMPLE:(Visum1,2) 
    #OUTPUT: An OD Matrix          
    #NOTE:
    VisFun$getODMatrix <- function(visObj, matNum) {
                  if(!exists("vVer")) vVer <- 16 # AB 10-22-18 Updated to work with Visum 18
                  MatInd <- ifelse(vVer>17,"Matrices","ODMatrices") # AB 10-22-18 Updated to work with Visum 18
                  MatObj <- comGetProperty(comGetProperty(comGetProperty(visObj,"Net"),MatInd),"ItemByKey",matNum)                	                
                  mat = comInvoke(MatObj, "GetValues")
	                mat = matrix(unlist(mat),sqrt(length(mat)),sqrt(length(mat)))
	                zone = comGetProperty(vbConv("Visum.Net"), "Zones")
                  rownames(mat) <- colnames(mat) <- comGetProperty(zone, "GetMultiAttValues", "NO")[,2] 
                  mat
                }

    #Writes a specified OD Matrix to Vium
    #PARAMETERS:Visum object, Matrix number, Replacement Matrix
    #EXAMPLE:(Visum1,1,AutoODMatrix)
    #OUTPUT: none
    #NOTE:
    VisFun$setODMatrix <- function(visObj, matNum, inMat) {
                  if(!exists("vVer")) vVer <- 16 # AB 10-22-18 Updated to work with Visum 18
                  MatInd <- ifelse(vVer>17,"Matrices","ODMatrices") # AB 10-22-18 Updated to work with Visum 18
                  MatObj <- comGetProperty(comGetProperty(comGetProperty(visObj,"Net"),MatInd),"ItemByKey",matNum)
	                inMat = as.list(inMat)
	                dim(inMat) <- rep(sqrt(length(inMat)),2)
                  comInvoke(MatObj,"SetValues", inMat)
                }
    
    #Retrieves a specified Skim Matrix
    #PARAMETERS:Visum object, Matrix number
    #EXAMPLE:(Visum1,2)
    #OUTPUT: A Skim Matrix   
    #NOTE:
    VisFun$getSkimMatrix <- function(visObj, matNum) {
                  if(!exists("vVer")) vVer <- 16 # AB 10-22-18 Updated to work with Visum 18
                  MatInd <- ifelse(vVer>17,"Matrices","SkimMatrices") # AB 10-22-18 Updated to work with Visum 18	                
                  MatObj <- comGetProperty(comGetProperty(comGetProperty(visObj,"Net"),MatInd),"ItemByKey",matNum)                
	                mat = comInvoke(MatObj, "GetValues")
	                mat = matrix(unlist(mat),sqrt(length(mat)),sqrt(length(mat)))
	                zone = comGetProperty(vbConv("Visum.Net"), "Zones")
                  rownames(mat) <- colnames(mat) <- comGetProperty(zone, "GetMultiAttValues", "NO")[,2] 
                  mat
                }
                    
    #Writes a specified Skim Matrix to Vsium
    #PARAMETERS:Visum object, Matrix number, Replacement Matrix
    #EXAMPLE:(Visum1,1, AutoODMatrix)
    #OUTPUT: none
    #NOTE:
    VisFun$setSkimMatrix <- function(visObj, matNum, inMat) {
                  if(!exists("vVer")) vVer <- 16 # AB 10-22-18 Updated to work with Visum 18
                  MatInd <- ifelse(vVer>17,"Matrices","SkimMatrices") # AB 10-22-18 Updated to work with Visum 18                  
                  MatObj <- comGetProperty(comGetProperty(comGetProperty(visObj,"Net"),MatInd),"ItemByKey",matNum)
	                inMat = as.list(inMat)
	                dim(inMat) <- rep(sqrt(length(inMat)),2)
                  comInvoke(MatObj,"SetValues", inMat)
                }
    
    #Deletes either an OD or Skim Matrix
    #PARAMETERS:Visum object, Matrix number, matType (either "O" or "S")
    #EXAMPLE:(Visum1,22, "O")
    #OUTPUT: none
    #NOTE:                
    VisFun$deleteMatrix <- function(visObj,matNum,matType) {
                  if(matType=="O") comInvoke(comGetProperty(visObj, "Net"),"RemoveODMatrix",matNum)
                  if(matType=="S") comInvoke(comGetProperty(visObj, "Net"),"RemoveSkimMatrix",matNum)
                }     
                
    #Sets a demand segment by name to an OD Matrix specified by code 
    #PARAMETERS:Visum object, Demand Segment Name, Matrix Code
    #EXAMPLE:(Visum1,"EI","EI") 
    #OUTPUT: NONE         
    #NOTE:                 
    VisFun$setDmdSeg = function(visObj, dsName, matCode) {
                  comObj <- comGetProperty(visObj,"Net")
                  comObj <- comGetProperty(comObj,"DemandSegments")
                  ds <- comGetProperty(comObj, "ItemByKey", dsName)
                  dsmat <- comGetProperty(ds, "ODMatrix")
                  # AB 10-22-18 Updated to work with Visum 18
                  matList_ <- getMatList(visObj)
                  if(!exists("vVer")) vVer <- 16
                  if(vVer>17){
                     matNum <- as.numeric(matList_$Matrices[matList_$Matrices[,"CODE"]==matCode,"NO"])
                  } else {
                     matNum <- as.numeric(matList_$ODMatrices[matList_$ODMatrices[,"CODE"]==matCode,"NO"])
                  }
                  inMat <- getODMatrix(visObj,matNum)
                  inMat <- TabletoSFA(inMat)
                  comInvoke(dsmat, "SetValues", inMat)		
                }
                
    #Sets an OD Matrix by Demand Segmenent name 
    #PARAMETERS:Visum object, Demand Segment Name, Matrix Name
    #EXAMPLE:(Visum,"C",getODMatrix(Visum, 1)) 
    #OUTPUT: NONE         
    #NOTE: Different than "setDmdSeg" in that it requires a matrix be provided as opposed to pointing to an existing OD matrix 
    VisFun$setDSegMatrix = function(Visum, dSegName, inMat) {
                  mat = comGetProperty(vbConv("Visum.Net.DemandSegments"), "ItemByKey", dSegName)
                  mat = comGetProperty(mat, "ODMatrix")
	                inMat = as.list(inMat)
	                dim(inMat) <- rep(sqrt(length(inMat)),2)
	                comInvoke(mat, "SetValues", inMat)		
                 }            
                
################################################ ASSIGNMENT ################################################################
    
    #Runs and assignment on a demand seqment
    #If more than one demand seqment to be run enter as a single string separated by commas eg.("ee,ei,hbw,hbshp")    
    #PARAMETERS:Visum Object,Procedure file path and name (either .par or .xml),Demand Segment string
    #EXAMPLE:(curVis,procFile=paste(getwd(),"Assignment.par",sep="/"),DMS="auto") 
    #OUTPUT: None    
    VisFun$assignDMS <- function(visObj,procFile=NULL,DMS=NULL,XMLfile=NULL) {                    
                  Proc <- comGetProperty(visObj,"Procedures") 
                  if(!is.null(procFile)) comInvoke(Proc,"Open",procFile)
                  if(!is.null(XMLfile)) comInvoke(Proc,"Open",XMLfile)
                  if(!is.null(DMS)){   
                      Opers <- comGetProperty(Proc,"Operations")
                      assignOper <-  comGetProperty(Opers, "ItemByKey","2")              
                      comSetProperty(assignOper,"AttValue","DSEGSETCODE",DMS)
                  }
                  comInvoke(Proc,"Execute")
              }

    #Retrieves the standard Visum assignment results 
    #PARAMETERS:Visum Object
    #EXAMPLE:(Visum1) 
    #OUTPUT: a data frame with for the network object with the assignment results    
    VisFun$getAssignResults <-  function(visObj) {
                              Proc <- comGetProperty(visObj,"Lists")
                              assignList <- comGetProperty(comGetProperty(visObj,"Lists"),"CreatePrTAssQualityList")
                              fields <- c("DSegCode","Iteration","InnerIteration","EndDay","EndTime","MeanAbsVolDiffLinks",
                                          "MeanAbsVolDiffTurns","MeanAbsVolDiffConnectors","MeanAbsVolDiffTotal","MeanRelVolDiffLinks",
                                          "MeanRelVolDiffTurns","MeanRelVolDiffConnectors","MeanRelVolDiffTotal","ShareVolDiffLess5PercLinks",
                                          "ShareVolDiffLess5PercTurns","ShareVolDiffLess5PercConnectors","ShareVolDiffLess5PercTotal",
                                          "AssignedDemand","NumRoutes","NumLinks","NumTurns","NumConnectors","VehKmTravPrT","VehMiTravPrT",
                                          "VehHourTravt0","VehHourTravtCur","VehHourImp","HypoShortPathVehHourImp","TargetEquiFunc",
                                          "TotalExcessCost","AvgExcessCost","DualityGap","RelativeGap") 
                              sapply(fields,function(x)comInvoke(assignList, "AddColumn", toupper(x)))
                              SFA <- comInvoke(assignList,"SaveToArray")
                              DF <- SFAtoTable(SFA,fields)
                              return(DF)
                          }
                          
    #Runs a single select link or zone on a single demand segment
    #Do not supply a tNode for Select Zone[s]
    #PARAMETERS:Visum Object,Net Single fNode, Single tNode, Single Demand Segment
    #EXAMPLE:(curVis,1303,1958,"auto") 
    #OUTPUT: a data frame with for the network object with the flow bundle results     
     VisFun$flowBundle <- function(visObj,fNode,tNode=NULL,DMS) {
                            netElem <- comInvoke(visObj,"CreateNetElements")
                            DmdObj <- comGetProperty(comGetProperty(visObj,"Net"),"DemandSegments")
                            carDS <- comGetProperty(DmdObj, "ItemByKey",DMS)
                            FB <- comGetProperty(carDS, "Flowbundle")
                            comInvoke(FB,"Clear")
                            if(!is.null(tNode)){                                    
                                netObj <- comGetProperty(comGetProperty(visObj,"Net"), "Links")
                                SLZ <- comGetProperty(netObj, "ItemByKey",fNode,tNode)
                            }else{
                                netObj <- comGetProperty(comGetProperty(visObj,"Net"), "Zones")
                                SLZ <- comGetProperty(netObj, "ItemByKey", fNode)                                
                            }                            
                            comInvoke(netElem,"Add",SLZ )
                            comInvoke(FB,"Execute",netElem)
                            #Retrieve Values
                            #Links
                            FBAtts <- c("VolFlowBundle(Prt)","FromNodeNo","ToNodeNo")
                            FBAtts_ <- lapply(FBAtts,function(x) getNetAtt(visObj,"Links",x))
                            for(i in 1:(length(FBAtts_)-1)) FBAtts_[[i+1]] <- merge(FBAtts_[[i]],FBAtts_[[i+1]],"ID",all=T)
                            FB_Links <- FBAtts_[[i+1]]
                            #Connectors
                            FBAtts <- c("VolFlowBundle(Prt)","ZoneNo","NodeNo","Direction")
                            FBAtts_ <- lapply(FBAtts,function(x) getNetAtt(visObj,"Connectors",x))
                            for(i in 1:(length(FBAtts_)-1)) FBAtts_[[i+1]] <- merge(FBAtts_[[i]],FBAtts_[[i+1]],"ID",all=T)
                            FB_Conn <- FBAtts_[[i+1]]
                            #close COM Objects
                            netElem <- DmdObj <- carDS <- FB <- netObj <- SLZ <- NULL
                            rm(netElem,DmdObj,carDS,FB,netObj,SLZ)
                            return(FB_)
                          } 
                          
################################################## RUN EXCEL VB SCRIPT ################################################
                                    
    VisFun$runExcel <- function(visObj,funName,...) {
              
              objExcel<-comCreateObject("Excel.Application")
              comSetProperty(objExcel,"Visible",T)
              wkb <- comInvoke(comGetProperty(objExcel,"Workbooks"),"Open",paste(getwd(),"VBScripts.xls",sep="/"))
              sheet <- comGetProperty(wkb,"ActiveSheet")
              #Clear sheet
              range1 <- comGetProperty(sheet,"Range","A1","IV65536")
              comInvoke(range1,"ClearContents")
              #set Ver File Location
              range1 <- comGetProperty(sheet,"Cells",1,1)
              comSetProperty(range1,"Value",paste(attr(visObj,"Path"),attr(visObj,"Name"),sep="/"))             
              #Set needed script parameters
              funPara <- list(...)
              if(is.object(funPara)){ 
                  lapply(1:length(funPara),function(y) {
                      range1 <- comGetProperty(sheet,"Cells",y,1)
                      comSetProperty(range1,"Value",funPara[[y]])
                  })
              }              
              #Run VB Script 
              VBRun <-comGetProperty(wkb,"Application")                            
              comInvoke(VBRun,"Run",paste("VBScripts.xls!",funName,sep=""))             
              #Return Script Results if requested
              endRow <- comGetProperty(sheet,"Cells",65535,256)
              endRowVal <- comGetProperty(endRow,"Value")
              endCol <- comGetProperty(sheet,"Cells",65536,256)
              endColVal <- comGetProperty(endCol,"Value")              
              if(endRowVal==""&endColVal=="")return()
              result <- data.frame()
              for(i in 1:endRowVal) {
                  for(k in 1:endColVal) {
                      range1 <- comGetProperty(sheet,"Cells",i,k)
                      result[i,k] <- comGetProperty(range1,"Value")
                  }
              }
              objExcel=wkb=sheet=range1=VBRun=endCol=endRow=NULL
              rm(objExcel,wkb,sheet,range1,VBRun,endCol,endRow)
              gc()
              return(result)
          }
           
########################################## FUNCTIONS CALLED BY OTHER FUNCTIONS ################################################            
    
    #Converts VB Style string call into an rcom call
    #PARAMETERS:  VB Style string. eg "Visum.Net"
    #OUTPUT: COM object             
    VisFun$vbConv <-  function(comCall) {
                  calls = unlist(strsplit(comCall, "\\."))
                  for(i in 1:(length(calls)-1)) assign(calls[i+1], comGetProperty(get(calls[i]), calls[i+1]))
                  get(calls[length(calls)])
                } 

    #PFDScript <- readLines("correctDir.pfd")
    #PFDScript <- lapply(as.list(PFDScript),function(x) gsub(" + ","  ", x))
    #PDFString <-do.call('paste',c(PFDScript,sep="\n")) 
    VisFun$PDFString <- "﻿* Networks\nNetze  %MYDOCUMENTS%\\  net  \n* Versions\nVersionen  %MYDOCUMENTS%\\  ver  \n* OD matrices\nQuelle-Ziel-Matrizen  %MYDOCUMENTS%\\  mtx  \n* Skim matrices\nKenngroessenmatrizen  %MYDOCUMENTS%\\  *  \n* OD demand data\nNachfragedaten  %MYDOCUMENTS%\\  dmd  \n* MultiUser networks\nMultiUserNetze  %MYDOCUMENTS%\\  NotEditable:[edf;idf]  \n* Project directories\nProjektverzeichnisse  %APPDATA%\\Visum\\100\\  pfd  \n* Graphic parameters\nGrafikparameter  %MYDOCUMENTS%\\  gpa  \n* Texts\nTexte  %MYDOCUMENTS%\\  txt  \n* Proced. parameters\nVerfahrensparameter  %MYDOCUMENTS%\\  par;xml  \n* RASW-Scen\nRASW-Fall  %MYDOCUMENTS%\\  rwf  \n* Attributes\nAttribute  %MYDOCUMENTS%\\  att  \n* Environmental param.\nUmweltparameter  %MYDOCUMENTS%\\  upa  \n* List Layout\nListen-Layout  %MYDOCUMENTS%\\  lla  \n* Filter\nFilter  %MYDOCUMENTS%\\  fil  \n* Active Network Objects\nAktive Netzelemente  %MYDOCUMENTS%\\  ane  \n* Shapefile\nShapefile  %MYDOCUMENTS%\\  NotEditable:[shp]  \n* Access database\nAccess-Datenbank  %MYDOCUMENTS%\\  NotEditable:[mdb]  \n* EMME/2\nEmme  %MYDOCUMENTS%\\  *  \n* References\nVerknuepfungen  %MYDOCUMENTS%\\  *  \n* Image files\nBilddateien  ..\\Data\\Symbols\\  NotEditable:[bmp;jpg;wmf;emf;gif;tiff;png]  \n* SVG files\nSVG-Dateien  %MYDOCUMENTS%\\  NotEditable:[svg]  \n* NEMA files\nNEMA-Dateien  %MYDOCUMENTS%\\  NotEditable:[nse]  \n* TModel files\nTModel-Dateien  %MYDOCUMENTS%\\  tla  \n* Script files\nSkript-Dateien  %MYDOCUMENTS%\\  NotEditable:[vbs;js;pys;py]  \n* TLY files\nTLY-Dateien  %MYDOCUMENTS%\\  tly  \n* HAFAS project files\nHAFAS-Projektdateien  %MYDOCUMENTS%\\  NotEditable:[haf]  \n* MUULI Log files\nMUULI-Log-Dateien  %MYDOCUMENTS%\\  mlg  \n* Screenshots\nScreenshots  %MYDOCUMENTS%\\  NotEditable:[jpg;wmf;emf;bmp;gif;tiff;png]  \n* Connection file\nVerbindungsdatei  %MYDOCUMENTS%\\  con  \n* Timetable Editor Graphic parameters\nGrafikparameter Fahrplaneditor  %MYDOCUMENTS%\\  gpt  \n* Timetable Editor Network Graph\nFahrplaneditor Netzgraph  %MYDOCUMENTS%\\  *  \n* Route import\nRouten-Import  %MYDOCUMENTS%\\  rim  \n* Legend parameters\nLegenden-Parameter  %MYDOCUMENTS%\\  lgd  \n* Backgrounds\nHintergruende  %MYDOCUMENTS%\\  NotEditable:[emf;wmf;bmp;dwg;dxf;ecw;jp2;jpg;png;shp;sid;svg;tga;tif;gif]\n* ICA files\nICA-Dateien  %MYDOCUMENTS%\\  *  \n* Additive network reading Parameters\nAdditives-Netzlesen-Para  %MYDOCUMENTS%\\  anrp  \n* Script menu files\nSkriptmenue-Dateien  %MYDOCUMENTS%\\  xml  \n* DXF files\nDXF-Dateien  %MYDOCUMENTS%\\  NotEditable:[dxf]  \n* Difference Network\nDifferenznetz  %MYDOCUMENTS%\\  diffnet  \n* ANM networks\nANM-Netze  %MYDOCUMENTS%\\  anm  \n* Additive-DemandReading-Para\nAdditives-Nachfragelesen-Para  %MYDOCUMENTS%\\  adrp  \n* Log files\nLog-Dateien  %APPDATA%\\Visum\\100\\Log\\  txt;log  \n* ANM - Export parameter files\nANM - Exportparameterdateien  %MYDOCUMENTS%\\  anmp  \n* ANM - Routes\nANM - Routen  %MYDOCUMENTS%\\  anmRoutes  \n* UserVDF-DLLs\nUserVDF-DLLs  %APPDATA%\\Visum\\100\\UserVDF-Dlls\\  NotEditable:[dll]  \n* EVA weighting matrices\nEVA-Bewertungsmatrizen  %MYDOCUMENTS%\\  wmt  \n* Projections\nProjections  %APPDATA%\\Visum\\100\\Projections\\  NotEditable:[prj]  \n* Quickview Layout\nSchnellansichts-Layout  %APPDATA%\\Visum\\100\\QuickViewLayout\\  qla  "

    #Set Project Directories for Visum File from list result of getFile()
    #PARAMETERS: Visum object, version file path string, pfd file path and name string 
    #OUTPUT: Modifies Visum Project Directories   
    #COMMENT: PDFString is made by the following code               
    VisFun$changePDString <- function(PDFString,vFP){
                  writeLines(PDFString,"temp.txt")
                  PFDScript <- readLines("temp.txt")
                  file.remove("temp.txt")    
                  editLines <- grep("\\\\", PFDScript)    
                  #Remove APPDATA locations except log and layout locations
                  noChange <- grep("%APPDATA%", PFDScript[editLines])
                  noChange <- noChange[grep("NotEditable|pfd",  PFDScript[editLines][noChange])]
                  PDs <- PFDScript[editLines][noChange]
                  PDs <- gsub(" + ","  ",PDs) #Remove extra spaces for clarity      
                  #change lines with folder location to current Ver file location
                  editList <- lapply(PFDScript[editLines],function(x)strsplit(x," + "))
                  editList <- unlist(editList,recursive=FALSE)    
                  editList <-  unlist(lapply(editList, function(x) {
                      x[grep("\\\\",x)] <- paste(vFP, "/",sep="")
                      paste(x, collapse="  ")
                  }))  
                  #add "ParameterFiles" subfolder for parameter and graphic par locations
                  parDir <- editList[grep("par;xml$", editList)]
                  
                  # code to search for the ParameterFiles folder
                  i=0
                  Check <- ("ParameterFiles" %in% list.files(path=paste(unlist(strsplit(vFP,"/"))[1:(length(unlist(strsplit(vFP,"/")))-i)],collapse="/")))
                  while(!Check & i<(length(unlist(strsplit(vFP,"/"))))){
                     i = i +1
                     Check <- ("ParameterFiles" %in% list.files(path=paste(unlist(strsplit(vFP,"/"))[1:(length(unlist(strsplit(vFP,"/")))-i)],collapse="/"))) 
                  }
                  if(!Check) i=0
                              
                  editList[grep("par;xml$", editList)] <- gsub(vFP, paste(paste(unlist(strsplit(vFP,"/"))[1:(length(unlist(strsplit(vFP,"/")))-i)],collapse="/"), "/ParameterFiles",sep=""), parDir)
                  parDir <- editList[grep("gpa$", editList)]
                  editList[grep("gpa$", editList)] <- gsub(vFP, paste(vFP, "/gpas",sep=""), parDir)
                  editList <- gsub("/", "\\\\", editList)
                  #Replace edited lines into PFD file    
                  PFDScript[editLines] <- editList
                  PFDScript[editLines][noChange] <- PDs
                  PFDScript
                }
    
    #Set Project Directories for Visum File from list result of getFile()
    #PARAMETERS: Visum object, version file path string, pfd file path and name string 
    #OUTPUT: Modifies Visum Project Directories paste(vFP, "temp.txt",sep="/")                
    VisFun$setPD <- function(visObj,vFP) { 
                  PDFMod <- changePDString(PDFString,vFP)
                  writeLines(PDFMod, "temp.txt")                   
                  comInvoke(visObj, "LoadPathFile",paste(getwd(), "temp.txt",sep="/")      )
                  file.remove("temp.txt") 
                }
                
    #Opens Visum version file from list result of getFile()
    #PARAMETERS: version file path string, version file name string 
    #OUTPUT: a Visum object for the specified version file   
    VisFun$OpenVisum <- function(vFP,vF) { 
                  vFFP <- paste(vFP,vF,sep="/")         
                  temp <- comCreateObject(paste("Visum.Visum.",vVer,sep=""))
                  comInvoke(temp, "LoadVersion",vFFP)
                  comInvoke(comGetProperty(temp, "Graphic"), "ShowMinimized")
                  setPD(temp,vFP)
                  return(temp)
                }
                
    #Converts a Visum Safe Array format to a dataframe
    #PARAMETERS: Visum SafeArray object, fields for table
    #OUTPUT: a dataframe format for safeArray with column header of supplied fields 
    VisFun$SFAtoTable <- function(SFAObj,fldNames) {    
                  Skel <- list(rep(0,dim(SFAObj)[1]))      
                  result_ <- list()
                  for(i in 1:dim(SFAObj)[2]) result_[[i]] <- relist(unlist(SFAObj[,i]), Skel)
                  result <- as.data.frame(result_, stringsAsFactors=F)
                  colnames(result) <- fldNames
                  return(result)
                }
    
    #Converts a Visum Safe Array format to a dataframe
    #PARAMETERS: Visum SafeArray object, fields for table
    #OUTPUT: a dataframe format for safeArray with column header of supplied fields 
    VisFun$TabletoSFA <- function(DF) {
                  result_ <- unlist(lapply(DF,function(x)as.list(x)),recursive=FALSE)
                  dim(result_) <-  c(nrow(DF),ncol(DF))    
                  return(result_)
                }
                
    #Builds an attribute table for adding attributes of a dataframe
    #PARAMETERS: dataframe with new attributes only
    #OUTPUT: a dataframe format attribute table for function addNetAtt()
    #NOTE:    
      # use the following codes in the 4th input:
      # Decimal = "2"
      # Text = "5"
      # integer = "1"   
    VisFun$BldAttTable <- function(DF) {
                            valType <- as.character(c(1,2,5))
                            names(valType) <- c("integer","numeric","character")    
                            Lnktyp <- lapply(DF,function(x) class(x))
                            Lnkdec <- lapply(DF,function(x) 0)
                            Lnkdec[names(Lnktyp)[Lnktyp=="numeric"]] <- lapply(DF[unlist(Lnktyp)=="numeric"],function(x) max(sapply(x,function(y){
                                                                          if(nchar(y)==nchar(trunc(y))){
                                                                              0
                                                                          }else{
                                                                              (nchar(y)-nchar(trunc(y)))-1
                                                                          }})))                                                   
                            attTable <- data.frame(names(Lnktyp),names(Lnktyp),names(Lnktyp),valType[unlist(Lnktyp)],unlist(Lnkdec),stringsAsFactors = FALSE)
                            colnames(attTable) <- c("AttID","Code","Name","Type","DecPl")
                            return(attTable)
                          }

################################################# END #####################################################################
