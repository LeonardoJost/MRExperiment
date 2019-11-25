### Read mental rotation data
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
source("functions/readDataPresentation.R", encoding="utf-8")
source("functions/readDataOpenSesame.R", encoding="utf-8")
source("functions/calculateData.R", encoding="utf-8")

getQuestionaireData=function(experimentalSoftware,verbose,folder){
  if (experimentalSoftware=="OpenSesame") {
    questionaireData=getOpenSesameQuestionaireData(verbose,folder, preText="", part="questionaire",ending="csv")
  }
  if (experimentalSoftware=="Presentation") {
    questionaireData=getPresentationQuestionaireData(verbose,folder)
  }
  return(questionaireData)
}

getMRData=function(experimentalSoftware,verbose,folder){
  if (experimentalSoftware=="OpenSesame") {
    MRData=getOpenSesameMRData(verbose,folder, preText="", part="main",ending="csv")
  }
  if (experimentalSoftware=="Presentation") {
    MRData=getPresentationMRData(verbose,folder)
  }
  return(MRData)
}

modifyQuestionaireData=function(experimentalSoftware,questionaireData) {
  if (experimentalSoftware=="OpenSesame") {
    questionaireData=modifyOpenSesameQuestionaireData(questionaireData)
  }
  if (experimentalSoftware=="Presentation") {
    questionaireData=modifyPresentationQuestionaireData(questionaireData)
  }
  return(questionaireData)
}

modifyMRData=function(experimentalSoftware,MRData,outlierFactor) {
  if (experimentalSoftware=="OpenSesame") {
    MRData=modifyOpenSesameMRData(MRData,outlierFactor)
  }
  if (experimentalSoftware=="Presentation") {
    MRData=modifyPresentationMRData(MRData,outlierFactor)
  }
  return(MRData)
}

