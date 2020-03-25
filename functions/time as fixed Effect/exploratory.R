### exploratory analysis of time as fixed effect
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

library(lme4)
library(optimx)
source("functions/helpers.R")

### functions
getAccuracyStatistics=function(myData){
  datasetForLMM=myData
  #scaling
  datasetForLMM$deg=datasetForLMM$deg/100
  datasetForLMM$endTime=datasetForLMM$endTime/1800000 #30 minutes= 1000(ms/s)*60(s/min)*30min=1800000
  #prepare dataset
  dataset.noOutlier=datasetForLMM[which(!datasetForLMM$outlier),]
  dataset.acc=dataset.noOutlier
  a0=glmer((type=="hit")~deg*block+(deg+block|ID)+(1|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  a0.summary=modelSummary(a0,0)
  a1=glmer((type=="hit")~deg+block+(deg+block|ID)+(1|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  a1.summary=modelSummary(a1,0)
  return(list(a0.summary,a1.summary))
}

getStats=function(myData){
  datasetForLMM=myData
  #scaling
  datasetForLMM$deg=datasetForLMM$deg/100
  datasetForLMM$endTime=datasetForLMM$endTime/1800000 #30 minutes= 1000(ms/s)*60(s/min)*30min=1800000
  #prepare dataset
  dataset.noOutlier=datasetForLMM[which(!datasetForLMM$outlier),]
  dataset.rt=dataset.noOutlier[which(dataset.noOutlier$typeOutlier=="hit"),]
  #model with only block
  mBase1=lmer(reactionTime~deg*block+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase1.summary=modelSummary(mBase1,0)
  #model with triple interaction
  mBase2=lmer(reactionTime~deg*endTime*block+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase2.summary=modelSummary(mBase2,0)
  #model with split interaction
  mBase3=lmer(reactionTime~deg*endTime+deg*block+endTime*block+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase3.summary=modelSummary(mBase3,0)
  #model with split interaction but isolated block effect
  mBase4=lmer(reactionTime~deg*endTime+block+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase4.summary=modelSummary(mBase4,0)
  return(list(mBase1.summary,mBase2.summary,mBase3.summary,mBase4.summary))
}

getStatsGroups=function(myData,testControl=FALSE){
  datasetForLMM=myData
  #scaling
  datasetForLMM$deg=datasetForLMM$deg/100
  datasetForLMM$endTime=datasetForLMM$endTime/1800000 #30 minutes= 1000(ms/s)*60(s/min)*30min=1800000
  #prepare dataset
  dataset.noOutlier=datasetForLMM[which(!datasetForLMM$outlier),]
  dataset.rt=dataset.noOutlier[which(dataset.noOutlier$typeOutlier=="hit"),]
  #model with only block
  mBase1=lmer(reactionTime~deg*block*group+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase1.summary=modelSummary(mBase1,0)
  #without degree
  mBase1a=lmer(reactionTime~block*group+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase1a.summary=modelSummary(mBase1a,0)
  #model with triple interaction
  mBase2=lmer(reactionTime~deg*endTime*block*group+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase2.summary=modelSummary(mBase2,0)
  #model with split interaction
  mBase3=lmer(reactionTime~deg*endTime*group+deg*block*group+endTime*block*group+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase3.summary=modelSummary(mBase3,0)
  #model with split interaction but isolated block effect
  mBase4=lmer(reactionTime~deg*endTime*group+block*group+endTime*block+deg*correctSide+MRexperience+(deg+endTime|ID)+(1|modelNumber),data=dataset.rt,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  mBase4.summary=modelSummary(mBase4,0)
  return(list(mBase1.summary,mBase1a.summary,mBase2.summary,mBase3.summary,mBase4.summary))
}

### main script
#load dataset
myData=dataset
#split block by time of 10 minutes
myData$block=toChar(myData$block)
myData$block=ifelse(myData$endTime>10*60*1000,ifelse(myData$endTime>20*60*1000,"main3","main2"),"main1")

myData$cond=myData$block
generateTableAndGraphsForCondition(myData,"block")
dataset$cond=paste(dataset$deg,dataset$correctSide,sep="*")
myData$cond=paste(myData$deg,myData$block,sep="*")
generateTableAndGraphsForCondition(myData,"blockXdeg",FALSE)

##testing for accuracy differences between blocks
accStats=getAccuracyStatistics(myData)

#comparing different parts of data using time interactions
#all blocks
comparisonDataSummaryAll=getStats(myData)

#blocks 1 and 3 but move block 3 in time to block 2
myData13=myData[which(myData$block!="main2"),]
myData13$endTime[which(myData13$block=="main3")]=myData13$endTime[which(myData13$block=="main3")]-10*60*1000

comparisonDataSummary13=getStats(myData13)

#add duplicated control group
myDataTest=myData13
myDataTest$group="treatment"
#rename main3 to main2 to compare with control
myDataTest$block[which(myDataTest$block=="main3")]="main2"
myDataControl=myData[which(myData$block!="main3"),]
myDataControl$group="control"
#create new ids for control
levels(myDataControl$ID)=paste("idControl",sample.int(length(levels(myDataControl$ID))),sep="")
myDataTestControl=rbind(myDataTest,myDataControl)

comparisonDataSummaryTestControl=getStatsGroups(myDataTestControl)
#calculate average performance in first block
myDataTestControl$meanPerformance=0
for(id in levels(as.factor(myDataTestControl$ID))){
  myDataTestControl$meanPerformance[which(myDataTestControl$ID==id)]=mean(myDataTestControl$reactionTime[which(myDataTestControl$ID==id & myDataTestControl$block=="main1")])
}
#select by performance and group (better group as treatment)
myDataTestControl2=myDataTestControl[which((myDataTestControl$group=="treatment" & myDataTestControl$meanPerformance<median(myDataTestControl$meanPerformance)) |
                                           (myDataTestControl$group=="control" & myDataTestControl$meanPerformance>=median(myDataTestControl$meanPerformance))
                                           ),]
myDataTestControl2$cond=paste(myDataTestControl2$group,myDataTestControl2$block,sep="*")
generateTableAndGraphsForCondition(myDataTestControl2,"blockXgroup",FALSE)

comparisonDataSummaryTestControl2=getStatsGroups(myDataTestControl2)

#select by performance and group (better group as control)
myDataTestControl3=myDataTestControl[which((myDataTestControl$group=="treatment" & myDataTestControl$meanPerformance>=median(myDataTestControl$meanPerformance)) |
                                             (myDataTestControl$group=="control" & myDataTestControl$meanPerformance<median(myDataTestControl$meanPerformance))
),]
myDataTestControl3$cond=paste(myDataTestControl3$group,myDataTestControl3$block,sep="*")
generateTableAndGraphsForCondition(myDataTestControl3,"blockXgroup2",FALSE)

comparisonDataSummaryTestControl3=getStatsGroups(myDataTestControl3)
comparisonDataSummaryTestControl3C=getStats(myDataTestControl3[which(myDataTestControl3$group=="control"),])
comparisonDataSummaryTestControl3T=getStats(myDataTestControl3[which(myDataTestControl3$group=="treatment"),])

#compare block 1 and 2/3 without actual treatment but separated by performance
myDataTestControl4=myData
#calculate average performance in first block
myDataTestControl4$meanPerformance=0
for(id in levels(as.factor(myDataTestControl4$ID))){
  myDataTestControl4$meanPerformance[which(myDataTestControl4$ID==id)]=mean(myDataTestControl4$reactionTime[which(myDataTestControl4$ID==id & myDataTestControl4$block=="main1")])
}
myDataTestControl4$group=ifelse(myDataTestControl4$meanPerformance<median(myDataTestControl4$meanPerformance),"treatment","control")
#merge block 2 and 3
myDataTestControl4$block[which(myDataTestControl4$block=="main3")]="main2"

comparisonDataSummaryTestControl4=getStatsGroups(myDataTestControl4)

#separate groups not by performance but by improvement
myDataTestControl5=myDataTestControl4
for(id in levels(as.factor(myDataTestControl4$ID))){
  myDataTestControl5$meanPerformance[which(myDataTestControl5$ID==id)]=
    mean(myDataTestControl5$reactionTime[which(myDataTestControl5$ID==id & myDataTestControl5$block=="main1")])-
    mean(myDataTestControl5$reactionTime[which(myDataTestControl5$ID==id & myDataTestControl5$block=="main2")])
}
myDataTestControl5$group=ifelse(myDataTestControl5$meanPerformance<median(myDataTestControl5$meanPerformance),"treatment","control")
myDataTestControl5$cond=paste(myDataTestControl5$group,myDataTestControl5$block,sep="*")
generateTableAndGraphsForCondition(myDataTestControl5,"blockXgroup3",FALSE)

comparisonDataSummaryTestControl5=getStatsGroups(myDataTestControl5)