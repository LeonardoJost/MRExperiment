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

getQuestionaireData=function(experimentalSoftware,verbose,folder){
  if (experimentalSoftware=="OpenSesame") {
    questionaireData=getOpenSesameQuestionaireData(verbose,folder, preText="", part="questionaire",ending="csv")
  }
  if (experimentalSoftware=="Presentation") {
    questionaireData=getPresentationQuestionaireData(verbose,folder)
  }
  return(questionaireData)
}

getMRData=function(experimentalSoftware,verbose,folder,block="main"){
  if (experimentalSoftware=="OpenSesame") {
    MRData=getOpenSesameMRData(verbose,folder,part=block)
  }
  if (experimentalSoftware=="Presentation") {
    MRData=getPresentationMRData(verbose,folder,block)
  }
  return(MRData)
}

modifyQuestionaireData=function(experimentalSoftware,questionaireData) {
  if (experimentalSoftware=="OpenSesame") {
    questionaireData=modifyOpenSesameQuestionaireData(questionaireData)
  }
  if (experimentalSoftware=="Presentation") {
    questionaireData=modifyPresentationQuestionaireData(questionaireData)
  }
  return(questionaireData)
}

modifyMRData=function(experimentalSoftware,verbose,MRData,outlierFactor) {
  if (experimentalSoftware=="OpenSesame") {
    MRData=modifyOpenSesameMRData(MRData,outlierFactor)
    #name end for each stimulus
    MRData$endTime=MRData$duration+MRData$reactionTime
  }
  if (experimentalSoftware=="Presentation") {
    MRData=modifyPresentationMRData(MRData,outlierFactor)
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
  if(verbose>1)
    print(paste("break time for all IDs ", 
                " mean: ",mean(MRData$pauseTime,na.rm=T),
                " sd: ", sd(MRData$pauseTime,na.rm=T)))
  return(MRData)
}