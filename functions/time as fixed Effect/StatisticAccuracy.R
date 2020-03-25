### Statistical analysis of accuracy
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
#load dataset
myData=dataset
#split block by time of 10 minutes
myData$block=toChar(myData$block)
myData$block=ifelse(myData$endTime>10*60*1000,ifelse(myData$endTime>20*60*1000,"main3","main2"),"main1")

##testing for accuracy differences between blocks
accStats=getAccuracyStatistics(myData)

##functions
getAccuracyStatistics=function(datasetForLMM){
  #scaling
  datasetForLMM$deg=datasetForLMM$deg/100
  datasetForLMM$endTime=datasetForLMM$endTime/1800000 #30 minutes= 1000(ms/s)*60(s/min)*30min=1800000
  #prepare dataset
  dataset.acc=datasetForLMM[which(!datasetForLMM$outlier),]
  #stats
  a0=glmer((type=="hit")~deg*block+deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  a0.summary=modelSummary(a0,0)
  a1=glmer((type=="hit")~deg+block+deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  a1.summary=modelSummary(a1,0)
  return(list(a0.summary,a1.summary))
}
