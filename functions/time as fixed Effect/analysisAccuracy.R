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
dataset.acc=datasetA1a
#analysis without time
a0=glmer((type=="hit")~deg*block+deg*correctSide+(deg+block|ID)+(block|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
a0.summary=modelSummary(a0,0)
#stepwise remove nonsignificant effects
a1=glmer((type=="hit")~deg+block+deg*correctSide+(deg+block|ID)+(block|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
a1.summary=modelSummary(a1,0)

#analysis with time
aTime0=glmer((type=="hit")~deg*block*endTime+deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aTime0.summary=modelSummary(aTime0,0)
#stepwise remove nonsignificant effects
aTime1=glmer((type=="hit")~block*endTime+deg*endTime+deg*block+
               deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aTime1.summary=modelSummary(aTime1,0)
#split block*endTime
aTime2=glmer((type=="hit")~deg*endTime+deg*block+
               deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aTime2.summary=modelSummary(aTime2,0)
#split block*deg
aTime3=glmer((type=="hit")~deg*endTime+block+
               deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aTime3.summary=modelSummary(aTime3,0)
#split block*deg
aTime4=glmer((type=="hit")~deg+endTime+block+
               deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aTime4.summary=modelSummary(aTime4,0)
#remove time
aTime5=glmer((type=="hit")~deg+block+
               deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aTime5.summary=modelSummary(aTime5,0)

#effect of deg*block
aTime6=glmer((type=="hit")~deg*block+
               deg*correctSide+(deg+endTime|ID)+(endTime|modelNumber),family=binomial(),data=dataset.acc,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aTime6.summary=modelSummary(aTime6,0)
