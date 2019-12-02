### calculations with mental rotation data
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
sortOutliers=function(verbose,MRData,sdFactor) {
  degrees=levels(as.factor(MRData$deg))
  #allData$type=toChar(allData$type)
  MRData$outlier=FALSE
  for(degree in degrees) {
    degreeSubset=MRData[which(MRData$deg==degree),]
    meanRT=mean(degreeSubset$reactionTime)
    sdRT=sd(degreeSubset$reactionTime)
    if (verbose>2){
      print(paste("mean+-sd at angle ",degree," : ",meanRT,"+-",sdRT,sep=""))
    }
    MRData$outlier[which(MRData$deg==degree & abs(MRData$reactionTime-meanRT)>sdFactor*sdRT)]=TRUE
  }
  #allData$type=as.factor(allData$type)
  return(MRData)
}