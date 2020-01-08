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
source("functions/readData.R", encoding="utf-8")
source("functions/generateGraphsAndTables.R", encoding="utf-8")

##create output directories, if they don't exist (outputs warnings otherwise)
dir.create("figs")
dir.create("output")
dir.create("figs/MR")
dir.create("figs/MR/allData")
dir.create("figs/MR/meanData")
dir.create("figs/MR/Timed/")
dir.create("figs/MR/accData/")

##options, parameters
options(digits=6)
#set data folder
folder="data\\MRlibrary\\"
verbose=2 #detail of output
experimentalSoftware="Presentation" #"OpenSesame" or "Presentation"
questionaireOutFile="output\\questionaire" #.csv added at end, leave empty if no output desired
handednessGraphFile="figs\\HandednessMW.png" #leave empty if no output desired
outlierFactor=3 #factor of sd to define outliers in MR
block="main"#name of intersting block of data
questionaireDataCols=c("ID","Gender","MRexperience") #which questionaire columns shall be kept for statistical analysis

##read and write data
#read data
questionaireData=getQuestionaireData(experimentalSoftware,verbose,folder)
MRData=getMRData(experimentalSoftware,verbose,folder,block)
#modify data #adapt to own data
questionaireData=modifyQuestionaireData(experimentalSoftware,questionaireData)
MRData=modifyMRData(experimentalSoftware,verbose,MRData,outlierFactor)
#calculate means from questionaire (and save to csv)
calculateMeansQuestionaire(verbose,questionaireData,questionaireOutFile,handednessGraphFile)
#remove not analyzed questionaire data to protect participant identity
questionaireData=subset(questionaireData,select=questionaireDataCols)
#unify data
dataset=merge(MRData,questionaireData,by="ID")
#anonymise IDs to protect participant identity
dataset$ID=as.factor(dataset$ID)
levels(dataset$ID)=paste("id",sample.int(length(levels(dataset$ID))),sep="")

#save full dataset to csv
write.table(dataset,file="output\\dataset.csv",sep=";", col.names=NA)

##plot reaction time and accuracy by interesting conditions
#rename interesting variable to cond and generate plots
dataset$cond=dataset$correctSide
generateTableAndGraphsForCondition(dataset,"side")
dataset$cond=paste(dataset$correctSide,dataset$axis,sep="*")
generateTableAndGraphsForCondition(dataset,"sideXaxis")
dataset$cond=paste(dataset$correctSide,dataset$orientation,sep="*")
generateTableAndGraphsForCondition(dataset,"sideXorientation")
dataset$cond=dataset$modelNumber
generateTableAndGraphsForCondition(dataset,"modelNumber")
dataset$cond=as.factor(dataset$deg)
generateTableAndGraphsForCondition(dataset,"deg",FALSE)
dataset$cond=paste(dataset$deg,dataset$correctSide,sep="*")
generateTableAndGraphsForCondition(dataset,"degXside",FALSE)
dataset$cond=dataset$direction
generateTableAndGraphsForCondition(dataset,"direction")
dataset$cond=paste(dataset$direction,dataset$axis,sep="*")
generateTableAndGraphsForCondition(dataset,"directionXaxis")


