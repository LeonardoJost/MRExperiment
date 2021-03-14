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

#get questionnaireData
#experimentalSoftware: which software was used to generate data? Presentation or OpenSesame
#verbose: detail of output
#folder: folder to search in for data
getQuestionnaireData=function(experimentalSoftware,verbose,folder){
  if (verbose>1) {
    print("Reading questionnaire data from files ...")
  }
  if (experimentalSoftware=="OpenSesame") {
    questionnaireData=getOpenSesameQuestionnaireData(verbose,folder, preText="", part="questionnaire",ending="csv")
  }
  if (experimentalSoftware=="Presentation") {
    questionnaireData=getPresentationQuestionnaireData(verbose,folder)
  }
  if (verbose>1) {
    print(paste("Questionnaire data from",nrow(questionnaireData),"participants was read."))
  }
  return(questionnaireData)
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

#modifies the questionnairedata, calculates some additional information
#experimentalSoftware: which software was used to generate data? Presentation or OpenSesame
#questionnaireData: dataset
modifyQuestionnaireData=function(experimentalSoftware,questionnaireData) {
  if (verbose>1) {
    print("Doing calculations on questionnaire data ...")
  }
  if (experimentalSoftware=="OpenSesame") {
    questionnaireData=modifyOpenSesameQuestionnaireData(questionnaireData)
  }
  if (experimentalSoftware=="Presentation") {
    questionnaireData=modifyPresentationQuestionnaireData(questionnaireData)
  }
  if (verbose>1) {
    print("Calculations on questionnaire data finished.")
  }
  return(questionnaireData)
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
  MRData$IDblock=paste(MRData$ID,MRData$block,sep="")
  for (thisIDblock in levels(as.factor(MRData$IDblock))) {
    MRDataWithThisIDblock=MRData[which(MRData$IDblock==thisIDblock),]
    MRDataWithThisIDblock$pauseTime=NA
    MRDataWithThisIDblock$pauseTime[2:nrow(MRDataWithThisIDblock)]=
      (MRDataWithThisIDblock$startTime[2:nrow(MRDataWithThisIDblock)]
       -MRDataWithThisIDblock$endTime[1:(nrow(MRDataWithThisIDblock)-1)])
    MRData$pauseTime[which(MRData$IDblock==thisIDblock)]=MRDataWithThisIDblock$pauseTime
    if (verbose>2)
      print(paste("break time for ID and block ", thisIDblock, 
                  " mean: ",mean(MRDataWithThisIDblock$pauseTime,na.rm=T),
                  " sd: ", sd(MRDataWithThisIDblock$pauseTime,na.rm=T)))
  }
  if(verbose>1) {
    print(paste("break time for all IDs ", 
                " mean: ",mean(MRData$pauseTime,na.rm=T),
                " sd: ", sd(MRData$pauseTime,na.rm=T)))
    print("Calculations on mental rotation data finished.")
  }
  MRData$IDblock=NULL
  return(MRData)
}