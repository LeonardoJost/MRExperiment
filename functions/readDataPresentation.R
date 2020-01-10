### Read mental rotation data from Presentation
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
source("functions/readQuestionairePresentation.R", encoding="utf-8")

#get all questionaire data from Presentation
getPresentationQuestionaireData=function(verbose,folder) {
  #read data from files
  questionaireData=getQuestionaireDataByDatePresentation(verbose, folder,"","q2")
  return(questionaireData)
}

#get all MR data from Presentation
getPresentationMRData=function(verbose,folder,block="main") {
  ##get MR Data
  MRData=getDataByDatePresentation(verbose,folder,"",block)
  return(MRData)
}

#modify questionaire Data from Presentation
modifyPresentationQuestionaireData=function(questionaireData) {
  #calculate handedness
  questionaireData=getHandedness(verbose,questionaireData,11,10)
  #set names
  names(questionaireData)[1:10]=c("MRexperience","Age","Gender","Pill","Period","Sport","Endurance","Strength","Play","Music")
  #transform values to numeric, remove white spaces, unify gender
  questionaireData=cleanData(questionaireData,c("Gender"),c("Age","Period","Endurance","Strength","Play","Music"),c("MRexperience","Pill","Sport"))
  #unify some data
  questionaireData$sportAkt=questionaireData[,"Endurance"]+questionaireData[,"Strength"]+questionaireData[,"Play"]
  #handedness to factor
  questionaireData$handFactor=as.factor(questionaireData$hand)
  return(questionaireData)
}

#modify MR data from Presentation
#MRData: dataset
#outlierFactor: trials deviating by more than outlierFactor*sd from mean will be classified as outliers
modifyPresentationMRData=function(verbose,MRData,outlierFactor) {
  MRData=addDataMRPresentation(MRData)
  #rename
  MRData$reactionTime=MRData$diff
  MRData$diff=NULL
  #mark outliers
  MRData=sortOutliers(verbose,MRData,outlierFactor)
  if (verbose>1) {
    print(paste(sum(MRData$outlier),"outliers detected (deviating by more than",
                outlierFactor,"standard deviations from mean (by degree)"))
  }
  MRData$type=as.factor(substring(toChar(MRData$type),4))  #remove rm_
  MRData$typeOutlier=ifelse(MRData$outlier,paste(toChar(MRData$type),"Outlier",sep=""),toChar(MRData$type))
  #save original degrees of rotation
  MRData$originalDegrees=MRData$deg
  #modify angles to 360-angle if angle>180, but keep information
  MRData$direction=ifelse(MRData$deg>180,"-",ifelse(MRData$deg==0 | MRData$deg==180,"0","+"))
  MRData$deg=ifelse(MRData$deg>180,360-MRData$deg,MRData$deg)
  return(MRData)
}

#reads data from files
#verbose: detail of output
#folder: folder to search in for questionaire data
#preText: Filter, only get files which start with preText
#idLength: length of id, directly after preText
getDataByIdPresentation=function(verbose, folder, preText, idLength) {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,paste(preText,".*Mentale.*",sep=""),"txt")
  if (verbose>2) {print(fileNames)}
  datas=rep("",length(fileNames))
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.table(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE)
    dataset=subset(rawData,diff>0,select = c(block,model,mirror,angle,diff,type,time2))
    dataset$orig=grepl("orig",dataset$mirror,fixed=TRUE)
    dataset$XYZ=ifelse(grepl("X",dataset$mirror,fixed=TRUE),"X",
                       ifelse(grepl("Y",dataset$mirror,fixed=TRUE),"Y","Z"))
    dataset$deg=as.numeric(gsub("\\D", "", dataset$mirror))
    dataset$number=1:nrow(dataset)
    dataset$absTime=toNumeric(dataset$time2)-toNumeric(rawData$time[1])
    dataset$time2=NULL
    #add pseudonym to dataset
    dataset$ID=substr(fileNames[fileIndex],nchar(preText)+1,nchar(preText)+idLength)
    #add day and group data to dataset (positioned by id)
    #dataset$day=substr(fileNames[fileIndex],nchar(preText)+idLength+2,nchar(preText)+idLength+2)
    #dataset$group=substr(fileNames[fileIndex],nchar(preText)+idLength+4,nchar(preText)+idLength+4)
    dat=rbind(dat,dataset)
  }
  return(dat)
}

#reads data from files
#verbose: detail of output
#folder: folder to search in for questionaire data
#preText: Filter, only get files which start with preText
#part: Filter, only get part of data in block, that contains part in the name
#items are sorted by date, id is order of dates
#fileNames are sorted alphabetically if no id is given -> date is alphabetical
getDataByDatePresentation=function(verbose, folder, preText, part="") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,paste(preText,".*Mentale.*",sep=""),"txt")
  if (verbose>2) {print(fileNames)}
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.table(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE)
    dataset=subset(rawData,diff>0 & block %in% part,select = c(block,model,mirror,diff,type,time2))
    dataset$orientation=ifelse(grepl("orig",dataset$mirror,fixed=TRUE),"a","b")
    dataset$orientationLeftBase=ifelse(grepl("A",dataset$model,fixed=TRUE),"a","b")
    dataset$axis=ifelse(grepl("X",dataset$mirror,fixed=TRUE),"X",
                     ifelse(grepl("Y",dataset$mirror,fixed=TRUE),"Y","Z"))
    dataset$deg=as.numeric(gsub("\\D", "", dataset$mirror))
    dataset$number=1:nrow(dataset)
    dataset$absTime=toNumeric(dataset$time2)-toNumeric(dataset$time2[1])+toNumeric(dataset$diff[1])
    dataset$time2=NULL
    dataset$mirror=NULL
    #add dateOrder as ID to dataset
    dataset$ID=fileIndex
    dat=rbind(dat,dataset)
  }
  return(dat)
}

#adds information about model and correct side of answer to dataset
#dat: dataset to by modified
addDataMRPresentation=function(dat) {
  dat$modelNumber=paste("m",stringToNum(dat$model),sep="")
  dat$correctSide=ifelse(dat$orientation==dat$orientationLeftBase,"Left","Right")
  #mirrored stimuli -> other side is correct
  return(dat)
}
