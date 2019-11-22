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

##main function to get all presentation data
getOpenSesameData=function(processQuestionaireData=TRUE, processMRData=TRUE) {
  ##get questionaire data
  questionaireData=getQuestionaireDataByDateOpenSesame(verbose,folder)
  if(processQuestionaireData) {
    #handedness data
    questionaireData=getHandedness(verbose,questionaireData,which(names(questionaireData) %in% 'Hand')[1],length(which(names(questionaireData) %in% 'Hand')))
    #transform values to numeric, remove white spaces, unify gender
    questionaireData=cleanData(questionaireData,c("Gender"),c("Alter","Zyklustag","Ausdauer","Kraft","Spiel","Musik"),c())
    #unify some data
    questionaireData$sportAkt=questionaireData[,"Ausdauer"]+questionaireData[,"Kraft"]+questionaireData[,"Spiel"]
    #handedness to factor
    questionaireData$handFactor=as.factor(questionaireData$hand)
  }
  #rename columns to different names
  colnames(questionaireData) = make.unique(names(questionaireData))
  ##calculate descriptive statistics of questionaire data
  
  #calculate means and modes by gender and save to csv
  questionaireDataMeansByGender=data.frame(lapply(questionaireData[which(questionaireData$Gender==levels(as.factor(questionaireData$Gender))[1]),],meanMode),stringsAsFactors = FALSE)
  for (genderNumber in 1:length(levels(as.factor(questionaireData$Gender))))
    questionaireDataMeansByGender[genderNumber,]=lapply(questionaireData[which(questionaireData$Gender==levels(as.factor(questionaireData$Gender))[genderNumber]),],meanMode)
  questionaireDataMeansByGender$ID=levels(as.factor(questionaireData$Gender))
  #means overall
  questionaireDataMeans=data.frame(lapply(questionaireData,meanMode),stringsAsFactors = FALSE)
  
  #save to csv
  if (questionaireOutFile!="") {
    write.table(questionaireDataMeansByGender,file=paste(questionaireOutFile,"MeansByGender.csv", sep=""),sep=";", col.names=NA)
    write.table(questionaireDataMeans,file=paste(questionaireOutFile,"Means.csv", sep=""),sep=";", col.names=NA)
    write.table(questionaireData,file=paste(questionaireOutFile,".csv", sep=""),sep=";", col.names=NA)
  }
  if (handednessGraphFile!="") {
    #plot handedness
    library(ggplot2)
    if(length(levels(as.factor(questionaireData$Gender)))>1)
      ggplot(questionaireData,aes(hand)) + geom_histogram(binwidth=0.5,aes(fill=Gender)) +xlab("Handedness") + ylab("Count") + theme_bw()
    else
      ggplot(questionaireData,aes(hand)) + geom_histogram(binwidth=0.5) +xlab("Handedness") + ylab("Count") + theme_bw()
    
    ggsave(handednessGraphFile)
  }
  
  ##get MR data
  MRData=getDataByDateOpenSesame(verbose,folder)
  #modify/calculate MR data
  if(processMRData) {
    #rename variables
    MRData$deg=toNumeric(MRData$angle)
    MRData$reactionTime=MRData$response_time
    MRData=sortOutliers(MRData,outlierFactor)
    MRData$type=ifelse(MRData$correct==1,"hit","miss")
    MRData$typeOutlier=ifelse(MRData$outlier,paste(toChar(MRData$type),"Outlier",sep=""),toChar(MRData$type))
    MRData$correctSide=ifelse(MRData$correct_response==1,"left","right")
    MRData$absTime=MRData$duration
    #save original degrees of rotation
    MRData$originalDegrees=MRData$deg
    #modify angles to 360-angle if angle>180, but keep information
    MRData$direction=ifelse(MRData$deg>180,"-",ifelse(MRData$deg==0 | MRData$deg==180,"0","+"))
    MRData$deg=ifelse(MRData$deg>180,360-MRData$deg,MRData$deg)
  }
  dataset=merge(MRData,questionaireData,by="ID")
  return(dataset)
}
#verbose: detail of output
#folder: folder to search in for files
#preText: Filter, only get files which start with preText
#part: Filter, only get part of data in block, that contains part in the name
#items are sorted by date, id is order of dates
#fileNames are sorted alphabetically if no id is given -> date is alphabetical
getDataByDateOpenSesame=function(verbose, folder, preText="", part="main",ending="csv") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {print(fileNames)}
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    dataset=subset(rawData,grepl(part,aaBlock))
    #add dateOrder as ID to dataset
    dataset$ID=fileIndex
    dat=rbind(dat,dataset)
  }
  dat$block=dat$aaBlock
  dat$aaBlock=NULL
  return(dat)
}

#verbose: detail of output
#folder: folder to search in for files
#preText: Filter, only get files which start with preText
#part: Filter, only get part of data in block, that contains part in the name
#items are sorted by date, id is order of dates
#fileNames are sorted alphabetically if no id is given -> date is alphabetical
getQuestionaireDataByDateOpenSesame=function(verbose, folder, preText="", part="questionaire",ending="csv") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {print(fileNames)}
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    dataset=subset(rawData,grepl(part,aaBlock))
    values=append(toChar(dataset$angle),fileIndex)
    if (verbose>1) {print(values)}
    dat=rbind(dat,values,stringsAsFactors = FALSE)
    if (fileIndex==1) {
      names(dat)=append(toChar(dataset$axis),"ID")
    }
  }
  return(dat)
}
