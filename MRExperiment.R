### main program
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
source("functions/readQuestionaire.R")
source("functions/readData.R", encoding="utf-8")
source("functions/calculateData.R", encoding="utf-8")
source("functions/generateGraphsAndTables.R", encoding="utf-8")

#create output directories, if they don't exist (outputs warnings otherwise)
dir.create("figs")
dir.create("output")
dir.create("figs/MR")
dir.create("figs/MR/allData")
dir.create("figs/MR/meanData")
dir.create("figs/MR/meanDataWeighted")
dir.create("figs/MR/Timed/")
dir.create("figs/MR/accData/")

#options, parameters
options(digits=6)
#set data folder
folder="data\\"
verbose=1 #detail of output

##get questionaire data and save to csv
#read data from files
questionaireData=getQuestionaireDataByDate(verbose, folder,"","q2")
#do the following, according to measured values
#calculate handedness
questionaireData=getHandedness(verbose,questionaireData,11,10)
#set names
names(questionaireData)[1:10]=c("MRErfahrung","Alter","Geschlecht","Pille","Periode","Sport","Ausdauer","Kraft","Spiel","Musik")
#transform values to numeric, remove white spaces, unify gender
questionaireData=cleanData(questionaireData,c("Alter","Periode","Ausdauer","Kraft","Spiel","Musik"),c("MRErfahrung","Pille","Sport"))
#unify some data
questionaireData$sportAkt=questionaireData[,"Ausdauer"]+questionaireData[,"Kraft"]+questionaireData[,"Spiel"]
#save to csv
write.table(questionaireData,file="output\\questionaire.csv",sep=";", col.names=NA)

##calculate descriptive statistics of questionaire data
#handedness to factor
questionaireData$handFactor=as.factor(questionaireData$hand)
#calculate means and modes by gender and save to csv
questionaireDataMeansByGender=data.frame(lapply(questionaireData[which(questionaireData$Geschlecht=="m"),],meanMode),stringsAsFactors = FALSE)
questionaireDataMeansByGender[2,]=lapply(questionaireData[which(questionaireData$Geschlecht=="w"),],meanMode)
questionaireDataMeansByGender$ID=c("m","w")
write.table(questionaireDataMeansByGender,file="output\\questionaireMeansByGender.csv",sep=";", col.names=NA)
#means overall
questionaireDataMeans=data.frame(lapply(questionaireData,meanMode),stringsAsFactors = FALSE)
write.table(questionaireDataMeans,file="output\\questionaireMeans.csv",sep=";", col.names=NA)
#plot handedness
library(ggplot2)
ggplot(questionaireData,aes(hand)) + geom_histogram(binwidth=0.5,aes(fill=Geschlecht)) +xlab("Händigkeit") + ylab("Anzahl") + theme_bw()
ggsave(paste("figs/","HandednessMW.png",sep=""))

##get MR Data
#all data
MRData.all=getDataByDate(verbose,folder,"")
MRData.all=addDataMR(MRData.all)
#get data by blocks
MRData.practice=getDataByDate(verbose,folder,"","practice")
MRData.practice=addDataMR(MRData.practice)
MRData=getDataByDate(verbose,folder,"","main")
MRData=addDataMR(MRData)
#mark outliers
MRData=sortOutliers(MRData,3)
MRData$type=as.factor(substring(toChar(MRData$type),4))  #remove rm_
MRData$typeOutlier=ifelse(MRData$outlier,paste(toChar(MRData$type),"Slow",sep=""),toChar(MRData$type))
#save original degrees of rotation
MRData$originalDegrees=MRData$deg
#modify angles to 360-angle if angle>180, but keep information
MRData$direction=ifelse(MRData$deg>180,"-",ifelse(MRData$deg==0 | MRData$deg==180,"0","+"))
MRData$deg=ifelse(MRData$deg>180,360-MRData$deg,MRData$deg)
#save to csv
write.table(MRData,file="output\\MRData.csv",sep=";", col.names=NA)

##plot reaction time and accuracy by interesting conditions
#rename interesting variable to cond and generate plots
MRData$cond=MRData$correctSide
generateTableAndGraphsForCondition("side")
MRData$cond=paste(MRData$correctSide,MRData$XYZ,sep="*")
generateTableAndGraphsForCondition("sideXaxis")
MRData$cond=paste(MRData$correctSide,MRData$orig,sep="*")
generateTableAndGraphsForCondition("sideXmirror")
MRData$cond=MRData$modelNumber
generateTableAndGraphsForCondition("model")
MRData$cond=as.factor(MRData$deg)
generateTableAndGraphsForCondition("deg",FALSE)
MRData$cond=paste(MRData$deg,MRData$correctSide,sep="*")
generateTableAndGraphsForCondition("degXside",FALSE)
MRData$cond=MRData$direction
generateTableAndGraphsForCondition("direction")
MRData$cond=paste(MRData$direction,MRData$XYZ,sep="*")
generateTableAndGraphsForCondition("directionXaxis")

#inspect dataset (output is only in console)
temp=as.data.frame(table(floor(MRData$absTime/60000)))
temp$Freq


