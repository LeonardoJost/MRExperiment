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

#verbose: detail of output
#folder: folder to search in for questionaire data
#preText: Filter, only get files which start with preText
#idLength: length of id, directly after preText
getDataById=function(verbose, folder, preText, idLength) {
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

#verbose: detail of output
#folder: folder to search in for questionaire data
#preText: Filter, only get files which start with preText
#part: Filter, only get part of data in block, that contains part in the name
#items are sorted by date, id is order of dates
#fileNames are sorted alphabetically if no id is given -> date is alphabetical
getDataByDate=function(verbose, folder, preText, part="") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,paste(preText,".*Mentale.*",sep=""),"txt")
  if (verbose>2) {print(fileNames)}
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.table(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE)
    dataset=subset(rawData,diff>0 & grepl(part,block),select = c(block,model,mirror,angle,diff,type,time2))
    dataset$orig=grepl("orig",dataset$mirror,fixed=TRUE)
    dataset$XYZ=ifelse(grepl("X",dataset$mirror,fixed=TRUE),"X",
                     ifelse(grepl("Y",dataset$mirror,fixed=TRUE),"Y","Z"))
    dataset$deg=as.numeric(gsub("\\D", "", dataset$mirror))
    dataset$number=1:nrow(dataset)
    dataset$absTime=toNumeric(dataset$time2)-toNumeric(dataset$time2[1])+toNumeric(dataset$diff[1])
    dataset$time2=NULL
    #add dateOrder as ID to dataset
    dataset$ID=fileIndex
    dat=rbind(dat,dataset)
  }
  return(dat)
}

addDataMR=function(dat) {
  dat$modelNumber=paste("m",stringToNum(dat$model),sep="")
  dat$correctSide=ifelse(dat$orig==TRUE,ifelse(grepl("A",dat$model,fixed=TRUE),"Left","Right"),
                         ifelse(grepl("A",dat$model,fixed=TRUE),"Right","Left"))
  #mirrored stimuli -> other side is correct
  return(dat)
}
