### Read mental rotation data
#     Copyright (C) 2019  Leonardo Jost
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

source("functions/helpers.R")
source("functions/readDataPresentation.R", encoding="utf-8")
source("functions/readDataOpenSesame.R", encoding="utf-8")
source("functions/calculateData.R", encoding="utf-8")

#get questionaireData
#experimentalSoftware: which software was used to generate data? Presentation or OpenSesame
#verbose: detail of output
#folder: folder to search in for data
getQuestionaireData=function(experimentalSoftware,verbose,folder){
  if (verbose>1) {
    print("Reading questionaire data from files ...")
  }
  if (experimentalSoftware=="OpenSesame") {
    questionaireData=getOpenSesameQuestionaireData(verbose,folder, preText="", part="questionaire",ending="csv")
  }
  if (experimentalSoftware=="Presentation") {
    questionaireData=getPresentationQuestionaireData(verbose,folder)
  }
  if (verbose>1) {
    print(paste("Questionaire data from",nrow(questionaireData),"participants was read."))
  }
  return(questionaireData)
}

#get mental rotation data
#experimentalSoftware: which software was used to generate data? Presentation or OpenSesame
#verbose: detail of output
#folder: folder to search in for data
#block: name of block of interest
getMRData=function(experimentalSoftware,verbose,folder,block="main"){
  if (verbose>1) {
    print(paste("Reading mental rotation data for block",block,"from files"))
  }
  if (experimentalSoftware=="OpenSesame") {
    MRData=getOpenSesameMRData(verbose,folder,part=block)
  }
  if (experimentalSoftware=="Presentation") {
    MRData=getPresentationMRData(verbose,folder,block)
  }
  if (verbose>1) {
    print(paste("Mental rotation data from",length(unique(MRData$ID)),"participants was read. (",nrow(MRData),"trials in total)"))
  }
  return(MRData)
}

#modifies the questionairedata, calculates some additional information
#experimentalSoftware: which software was used to generate data? Presentation or OpenSesame
#questionaireData: dataset
modifyQuestionaireData=function(experimentalSoftware,questionaireData) {
  if (verbose>1) {
    print("Doing calculations on questionaire data ...")
  }
  if (experimentalSoftware=="OpenSesame") {
    questionaireData=modifyOpenSesameQuestionaireData(questionaireData)
  }
  if (experimentalSoftware=="Presentation") {
    questionaireData=modifyPresentationQuestionaireData(questionaireData)
  }
  if (verbose>1) {
    print("Calculations on questionaire data finished.")
  }
  return(questionaireData)
}

#modifies the mental rotation data, calculates some additional information
#experimentalSoftware: which software was used to generate data? Presentation or OpenSesame
#verbose: detail of output
#MRData: dataset
#outlierFactor: trials deviating by more than outlierFactor*sd from mean will be classified as outliers
modifyMRData=function(experimentalSoftware,verbose,MRData,outlierFactor) {
  if (verbose>1) {
    print("Doing calculations on mental rotation data ...")
  }
  if (experimentalSoftware=="OpenSesame") {
    MRData=modifyOpenSesameMRData(verbose,MRData,outlierFactor)
    #name end for each stimulus
    MRData$endTime=MRData$duration+MRData$reactionTime #use time_Stimulus instead of duration to account for framerate of monitor?
  }
  if (experimentalSoftware=="Presentation") {
    MRData=modifyPresentationMRData(verbose,MRData,outlierFactor)
    #name end for each stimulus
    MRData$endTime=MRData$absTime
  }
  #sort data by endTime
  MRData=MRData[order(MRData$ID,MRData$endTime),]
  #name startTime for each stimulus
  MRData$startTime=MRData$endTime-MRData$reactionTime
  #calculate time between end of stimulus and start of next stimulus
  MRData$pauseTime=NA
  for (thisID in levels(as.factor(MRData$ID))) {
    MRDataWithThisID=MRData[which(MRData$ID==thisID),]
    MRDataWithThisID$pauseTime=NA
    MRDataWithThisID$pauseTime[2:nrow(MRDataWithThisID)]=
      (MRDataWithThisID$startTime[2:nrow(MRDataWithThisID)]
       -MRDataWithThisID$endTime[1:(nrow(MRDataWithThisID)-1)])
    MRData$pauseTime[which(MRData$ID==thisID)]=MRDataWithThisID$pauseTime
    if (verbose>2)
      print(paste("break time for ID ", thisID, 
                  " mean: ",mean(MRDataWithThisID$pauseTime,na.rm=T),
                  " sd: ", sd(MRDataWithThisID$pauseTime,na.rm=T)))
  }
  if(verbose>1) {
    print(paste("break time for all IDs ", 
                " mean: ",mean(MRData$pauseTime,na.rm=T),
                " sd: ", sd(MRData$pauseTime,na.rm=T)))
    print("Calculations on mental rotation data finished.")
  }
  return(MRData)
}