### description
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

#mark outliers, whic deviate by more than sdFactor*sd from the mean by degree
sortOutliers=function(dataset,sdFactor) {
  degrees=levels(as.factor(dataset$deg))
  #allData$type=toChar(allData$type)
  dataset$outlier=FALSE
  for(degree in degrees) {
    degreeSubset=dataset[which(dataset$deg==degree),]
    mean=mean(degreeSubset$diff)
    sd=sd(degreeSubset$diff)
    #print(paste(degree,":",mean,"+-",sd,sep=""))
    #degreeSubset$type=toChar(degreeSubset$type)
    #degreeSubset$outlier[which(degreeSubset$diff>mean+sdFactor*sd)]=paste(degreeSubset$type[which(degreeSubset$diff>mean+sdFactor*sd)],"Slow",sep="")
    dataset$outlier[which(dataset$deg==degree & abs(dataset$diff-mean)>sdFactor*sd)]=TRUE
  }
  #allData$type=as.factor(allData$type)
  return(dataset)
}