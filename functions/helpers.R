### Supporting functions, which fit no category
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

#get filenames in folder, containing muster and ending with endung
getFileNames=function(folder,muster,endung) {
  fileNames=as.list(dir(path=folder,
                        pattern = paste(muster,"\\.",endung,"$",sep="")))
  return(fileNames)
}

#factor to numeric
toNumeric=function(vec) {
  if (is.factor(vec)) {
    return(as.numeric(levels(vec))[vec])
  } else {
    return(as.numeric(vec))
  }
}

#factor to string (only if levels of factor are strings)
toChar=function(vec) {
  if (is.factor(vec)) {
    return(as.character(levels(vec))[vec])
  } else {
    return(vec)
  }
}

#convert string to numeric
#apply modification for common input values 
#set decimal point to ., if , is entered
#if ranges are entered using "-" (e.g. 1-2), return mean
stringToNum=function(vec){
  vec=toChar(vec)
  vec=gsub("[[:alpha:]]+", "", vec)
  vec=gsub(",",".",vec)
  for (i in 1:length(vec)) {
    if (grepl("-",paste(vec[i]))) {
      temp=unlist(strsplit(paste(vec[i]),split='-'))
      vec[i]=(as.numeric(temp[1])+as.numeric(temp[2]))/2
    }
  }
  return(as.numeric(vec))
}

#remove digits from string, trim to length set by trim
stringRemoveNum=function(vec,trim){
  vec=gsub("[[:digit:]]+", "", vec)
  vec=substring(vec,trim)
  return(vec)
}

#calculate mean and sd (for numeric) or mode (for nonumeric) of vector
meanMode=function(vec,narm=TRUE,digitsFormat=4) {
  if(is.factor(vec)){
    vec=levels(vec)[vec]
  }
  if(is.numeric(vec))
    return(meanSd(vec,narm,digitsFormat))
  else
    return(modes(vec))
}

#return modes as string in decreasing order
modes = function(vec) {
  dat=as.data.frame(table(vec))
  dat=dat[order(dat$Freq,decreasing=TRUE),]
  paste(paste(dat$vec,dat$Freq,sep=":"),collapse=",")
}
#return mean(sd) as string
meanSd=function(vec,narm=TRUE,digitsFormat=4) {
  return(paste(format(mean(vec,na.rm=narm), digits=digitsFormat),"(",format(sd(vec,na.rm=narm), digits=digitsFormat),")",sep=""))
}

#calculate and print analysis of lmm 
#1: model summary
#2: chisquared test for all fixed effects
#3: confidence interval using parametric bootstrapping (not done if sims=0)
modelSummary=function(model, sims=1000) {
  print(summary(model))
  model.drop1=drop1(model,test="Chisq")
  print(model.drop1)
  if(sims>0) {
    model.confint=confint(model, method="boot",parallel="multicore", ncpus=8,nsim=sims)
    print(model.confint)
  } else {
    model.confint=0
  }
  return(list(summary(model),model.drop1,model.confint))
}