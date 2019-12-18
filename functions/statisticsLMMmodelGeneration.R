### Basics of model generation for statistical analysis, have to be adopted on a case by case basis
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
##reaction time with axis
#model selection
m0=lmer(reactionTime~allFixedEffects+(rs|ID)+(rs|modelNumber),data=dataset.rt.axis,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
print(summary(m0),corr=FALSE)
summary(rePCA(m0))
#remove random slope correlation
m1=lmer(reactionTime~allFixedEffects+(rs||ID)+(rs||modelNumber),data=dataset.rt.axis,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
VarCorr(m1)
anova(m1, m0)
summary(rePCA(m1))
#remove parameter with correlation 1 and sd 0 or very close or NaN


#add random slope correlation again

#find final model

##remove nonsignificant fixed effects
modelWithRS=lmer(reactionTime~allFixedEffects+(rs|ID)+(rs|modelNumber),data=dataset.rt.axis,REML=FALSE,control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
modelWithRS.summary=modelSummary(modelWithRS,0)
#stepwise remove nonsignificant fixed effect with lowest lrt

#all values significant

#visual inspection of normality
plot(finalModelWithRS)



##accuracy with axis
a0=glmer((type=="hit")~allFixedEffects+(allFixedEffects|ID)+(allFixedEffects|modelNumber),family=binomial(),data=dataset.acc.axis,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
print(summary(a0),corr=FALSE)
summary(rePCA(a0))
#remove correlation
a1=glmer((type=="hit")~allFixedEffects+(allFixedEffects||ID)+(allFixedEffects||modelNumber),family=binomial(),data=dataset.acc.axis,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
VarCorr(a1)
anova(a1, a0)
summary(rePCA(a1))
#remove parameter with correlation 1 and sd 0 or very close or NaN


#add random slope correlation again

#find final model

##remove nonsignificant fixed effects

#stepwise remove nonsignificant fixed effect with lowest lrt

#all values significant

#visual inspection of normality
