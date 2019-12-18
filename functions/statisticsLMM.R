### Basics of statistical analysis, have to be adopted on a case by case basis
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


library(plyr)
library(lme4)
library(optimx)


#analysis of mental rotation
#prepare data
#do not modify original dataset
datasetForLMM=dataset

#scaling
datasetForLMM$deg=datasetForLMM$deg/100
datasetForLMM$absTime=datasetForLMM$absTime/1800000 #30 minutes= 1000(ms/s)*60(s/min)*30min=1800000

#prepare dataset for reaction Time analysis and accuracy analysis
dataset.noOutlier=datasetForLMM[which(!datasetForLMM$outlier),]
dataset.rt=dataset.noOutlier[which(dataset.noOutlier$typeOutlier=="hit"),]
dataset.acc=dataset.noOutlier
dataset.rt.axis=dataset.rt[which(dataset.rt$deg>0),]
dataset.acc.axis=dataset.acc[which(dataset.acc$deg>0),]

####adopt individually
##reaction time (model including axis)
#base model
mBase=lmer(reactionTime~fixedEffects+(randomSlopes|ID)+(randomSlopes|modelNumber),data=dataset.rt.axis,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
mBase.summary=modelSummary(mBase)
save(mBase,mBase.summary,file="RTModelmBase.RData")
#split interactions


#nonsignificant effects



##reaction time (model without axis)
#base model

#split interactions


#nonsignificant effects



##accuracy (model including axis)
#base model
aBase=glmer((type=="hit")~fixedEffects+(randomSlopes|ID)+(randomSlopes|modelNumber),family=binomial(),data=dataset.acc.axis,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aBase.summary=modelSummary(aBase)
save(aBase,aBase.summary,file="AccModelaBase.RData")

#split interactions


#nonsignificant effects


##accuracy (model without axis)
#base model


#split interactions

#nonsignificant effects


