### Read questionaire data from Presentation logfiles
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
#firstQuestion: label of first question
#idLength: length of id, directly after preText
getQuestionaireDataByIDPresentation=function(verbose, folder, preText,firstQuestion, idLength) {
  #get files in folder (Questionaire Data)
  fileNames=getFileNames(folder,paste(preText,".*Question.*",sep=""),"txt")
  if (verbose>2) {print(fileNames)}
  #get number of questions from first file
  rawData=read.table(paste(folder,fileNames[1],sep=""),sep="\n",header=FALSE,fill=TRUE,blank.lines.skip=TRUE,encoding="UTF-8")
  #remove intro questions
  rawData=removeRows(rawData,firstQuestion)
  #number of question entries (one extra field for ID added, one subtracted for finish question)
  rows=length(grep("q\\d+$",rawData))
  #create matrix for reading/saving data
  dataMatrix=matrix(nrow=rows,ncol=length(fileNames))
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.table(paste(folder,fileName,sep=""),sep="\n",header=FALSE,fill=TRUE,blank.lines.skip=TRUE,encoding="UTF-8")
    rawData=removeRows(rawData,firstQuestion)
    for (j in 1:rows-1) {
      dataMatrix[j,fileIndex]=as.character(rawData[2*j])
    }
    #add pseudonym to matrix
    dataMatrix[rows,fileIndex]=substr(fileNames[fileIndex],nchar(preText)+1,nchar(preText)+idLength)
    if (verbose>1) {print(dataMatrix[rows,fileIndex])}
  }
  dat=as.data.frame(t(dataMatrix))
  names(dat)[rows]="ID"
  return(dat)
}

#verbose: detail of output
#folder: folder to search in for questionaire data
#preText: Filter, only get files which start with preText
#firstQuestion: label of first question
#items are sorted by date, id is order of dates
#fileNames are sorted alphabetically if no id is given -> date is alphabetical
getQuestionaireDataByDatePresentation=function(verbose, folder, preText,firstQuestion) {
  #get files in folder (Questionaire Data)
  fileNames=getFileNames(folder,paste(preText,".*Question.*",sep=""),"txt")
  if (verbose>2) {print(fileNames)}
  #get number of questions from first file
  rawData=read.table(paste(folder,fileNames[1],sep=""),sep="\n",header=FALSE,fill=TRUE,blank.lines.skip=TRUE,encoding="UTF-8")
  #remove intro questions
  rawData=removeRows(rawData,firstQuestion)
  #number of question entries (one extra field for ID added, one subtracted for finish question)
  rows=length(grep("q\\d+$",rawData))
  #create matrix for reading/saving data
  dataMatrix=matrix(nrow=rows,ncol=length(fileNames))
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.table(paste(folder,fileName,sep=""),sep="\n",header=FALSE,fill=TRUE,blank.lines.skip=TRUE,encoding="UTF-8")
    rawData=removeRows(rawData,firstQuestion)
    for (j in 1:rows-1) {
      dataMatrix[j,fileIndex]=as.character(rawData[2*j])
    }
    #add pseudonym to matrix
    dataMatrix[rows,fileIndex]=as.character(fileIndex)
    if (verbose>2) {print(paste("read ID",dataMatrix[rows,fileIndex]))}
  }
  dat=as.data.frame(t(dataMatrix))
  names(dat)[rows]="ID"
  return(dat)
}

#remove all rows from vec before first occurence of start
removeRows=function(vec,start) {
  return(vec[min(which(vec==start)):nrow(vec),])
}

# addDataByID=function(verbose,dat,dat2){
#   datID=as.vector(dat[,"ID"])
#   dat2ID=as.vector(dat2[,"ID"])
#   if("ID2" %in% colnames(dat2)) {
#     dat2ID2=as.vector(dat2[,"ID2"])
#     imax=2
#   } else {
#     dat2ID2=rep("",length(datID))
#     imax=1
#   }
#   #rewrite IDs (save both IDs in data frame, save ID2 first to preserve original ID)
#   for (i in imax:1) {
#     #helpvector
#     vec=c(1:nrow(dat))
#     for (j in 1:nrow(dat)) {
#       #if id is not found, print id
#       if (length(dat2[dat2ID==datID[j] | dat2ID2==datID[j],i])==0) {
#         print(datID[j])
#       }
#       #add data
#       vec[j]=paste(dat2[dat2ID==datID[j] | dat2ID2==datID[j],i])
#     }
#     if (verbose>3) {print(vec)}
#     dat[[names(dat2)[i]]]=vec
#   }
#   #get data from other columns
#   for (i in (1+imax):ncol(dat2)) {
#     #helpvector
#     vec=c(1:nrow(dat))
#     for (j in 1:nrow(dat)) {
#       #if id is not found, print id
#       if (length(dat2[dat2ID==datID[j] | dat2ID2==datID[j],i])==0) {
#         print(datID[j])
#         vec[j]=""
#       } else {
#       #add data
#         vec[j]=paste(dat2[dat2ID==datID[j] | dat2ID2==datID[j],i])
#       }
#     }
#     if (verbose>3) {print(vec)}
#     dat[[names(dat2)[i]]]=vec
#   }
#   return(dat)
# }
# 
# addDataByIDFromCSV=function(verbose,dat, csvFile){
#   #read csv data for hfmax and pmax
#   csvdat=read.csv(csvFile, sep=";",header=TRUE)
#   return(addDataById(verbose,dat,csvdat))
# }


