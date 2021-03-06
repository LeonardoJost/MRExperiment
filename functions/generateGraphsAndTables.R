### generate graph and table output
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

#generate table and graphs for cond from MRData dataset
generateTableAndGraphsForCondition=function(MRData,conditionString,degreeGraphs=TRUE,timeGraphs=TRUE,legendTitle="cond",lineTypes=FALSE){
  #calculate means by angle and interesting condition (important for plotting accuracy)
  #careful with outliers
  library(plyr)
  #averages for each participant and condition
  MRDataMeansByIDDegcond=ddply(MRData,
                               .(ID,deg,cond),
                               summarize,
                               reactionTime=weighted.mean(reactionTime,typeOutlier=="hit"),
                               hits=sum(typeOutlier=="hit"),
                               misses=sum(typeOutlier=="incorrect"),
                               acc=hits/(hits+misses),
                               outliers=sum(outlier))
  #average and sd over all participants
  MRDataMeansByDegcond=ddply(MRDataMeansByIDDegcond,
                             .(deg,cond),
                             summarize,
                             reactionTimeSd=sd(reactionTime,na.rm=T),
                             reactionTime=mean(reactionTime,na.rm=T),
                             accSd=sd(acc),
                             acc=mean(acc))
  #format digits
  MRDataMeansByDegcond=format(MRDataMeansByDegcond, digits=4)
  #write table
  write.table(MRDataMeansByDegcond,file=paste("output\\MRDataMeansByDeg",conditionString,".csv",sep=""),sep=";", col.names=NA)
  
  #generate plots
  if (degreeGraphs) {
    #all data
    generateGraphs(MRData,paste("MR/allData/",conditionString,sep=""),legendTitle)
    #average by participants
    generateGraphs(MRDataMeansByIDDegcond,paste("MR/meanData/",conditionString,sep=""),legendTitle)
    #accuracy is always only for averages
    generateAccGraphs(MRDataMeansByIDDegcond,paste("MR/accData/",conditionString,sep=""),legendTitle)
  }
  if (timeGraphs) {
    #plot line graphs for changes over time
    generateLineGraphsByTime(MRData[which(MRData$typeOutlier=="hit"),],paste("MR/Timed/",conditionString,sep=""),legendTitle,lineTypes)
  }
}

#generate reaction time graphs
generateGraphs=function(dataset,title,legendTitle="cond") {
  library(ggplot2)
  #plot data as line graph (mean Data by degree and condition)
  ggplot(dataset,aes(y=reactionTime,x=deg,group=deg,fill=cond, color=cond, linetype=cond, shape=cond)) + 
    stat_summary(na.rm=TRUE, fun.y=mean, geom="line",  aes(group=cond,color=cond)) +
    stat_summary(na.rm=TRUE, fun.y=mean, geom="point", size=2,aes(group=cond,color=cond)) +
    stat_summary(fun.data=mean_cl_normal,geom="errorbar", width=0.2,aes(group=cond,color=cond)) +
    scale_x_continuous(breaks=c(0:4)*45)+
    labs(x="degrees(°)",y="Reaction Time(ms)",color=legendTitle,fill=legendTitle,linetype=legendTitle,shape=legendTitle) + 
    theme_bw() + theme(legend.position = "bottom")
  ggsave(paste("figs/",title,"LinePlotByCondDegree.png",sep=""))
}

#generate lins graphs by time
generateLineGraphsByTime=function(dataset,title,legendTitle="cond",lineTypes=FALSE) {
  if(lineTypes){
    condForLineTypes=dataset$condForLineTypes
  } else {
    condForLineTypes=dataset$cond
  }
  library(ggplot2)
  #plot data as line graph (mean Data by degree and condition)
  ggplot(dataset,aes(y=reactionTime,x=endTime, color=condForLineTypes, linetype=condForLineTypes)) + 
    geom_smooth(aes(group=cond,fill=condForLineTypes)) +
    labs(x="time(min)",y="Reaction Time(ms)",color=legendTitle,linetype=legendTitle,fill=legendTitle) +
    theme_bw() +theme(legend.position = "bottom")
  ggsave(paste("figs/",title,"LinePlotByCondTime.png",sep=""))
}

#generate graphs for accuracy
generateAccGraphs=function(dataset,title,legendTitle="cond") {
  library(ggplot2)
  #plot data as line graph (mean Data by degree and condition)
  ggplot(dataset,aes(y=acc,x=deg,group=deg,fill=cond, color=cond, linetype=cond, shape=cond)) + 
    stat_summary(na.rm=TRUE, fun.y=mean, geom="line",  aes(group=cond,color=cond)) +
    stat_summary(na.rm=TRUE, fun.y=mean, geom="point", size=2,aes(group=cond,color=cond)) +
    stat_summary(fun.data=mean_cl_normal,geom="errorbar", width=0.2,aes(group=cond,color=cond)) +
    scale_x_continuous(breaks=c(0:4)*45)+
    labs(x="degrees(°)",y="Proportion of correct answers",color=legendTitle,fill=legendTitle,linetype=legendTitle, shape=legendTitle) + 
    theme_bw() + theme(legend.position = "bottom")
  ggsave(paste("figs/",title,"LinePlotByCondDegree.png",sep=""))
  
}

#calculate means and mode for questionnaire data and save to csv
calculateMeansQuestionnaire=function(verbose,questionnaireData,questionnaireOutFile,handednessGraphFile){
  #calculate means and modes by gender and save to csv
  questionnaireDataMeansByGender=data.frame(lapply(questionnaireData[which(questionnaireData$Gender==levels(as.factor(questionnaireData$Gender))[1]),],meanMode),stringsAsFactors = FALSE)
  for (genderNumber in 1:length(levels(as.factor(questionnaireData$Gender))))
    questionnaireDataMeansByGender[genderNumber,]=lapply(questionnaireData[which(questionnaireData$Gender==levels(as.factor(questionnaireData$Gender))[genderNumber]),],meanMode)
  questionnaireDataMeansByGender$ID=levels(as.factor(questionnaireData$Gender))
  #means overall
  questionnaireDataMeans=data.frame(lapply(questionnaireData,meanMode),stringsAsFactors = FALSE)
  
  #save to csv
  if (questionnaireOutFile!="") {
    if(verbose>1){
      print(paste("Writing mean and mode data for questionnaires (by gender) to file",paste(questionnaireOutFile,"MeansByGender.csv", sep="")))
      print(paste("Writing mean and mode data for questionnaires to file",paste(questionnaireOutFile,"Means.csv", sep="")))
    }
    write.table(questionnaireDataMeansByGender,file=paste(questionnaireOutFile,"MeansByGender.csv", sep=""),sep=";", col.names=NA)
    write.table(questionnaireDataMeans,file=paste(questionnaireOutFile,"Means.csv", sep=""),sep=";", col.names=NA)
    #write.table(questionnaireData,file=paste(questionnaireOutFile,".csv", sep=""),sep=";", col.names=NA)
  }
  if (handednessGraphFile!="") {
    if(verbose>1){
      print(paste("Writing handedness graph (by gender) to file",handednessGraphFile))
    }
    #plot handedness
    library(ggplot2)
    if(length(levels(as.factor(questionnaireData$Gender)))>1)
      ggplot(questionnaireData,aes(hand)) + geom_histogram(binwidth=0.5,aes(fill=Gender)) +xlab("Handedness") + ylab("Count") + theme_bw()
    else
      ggplot(questionnaireData,aes(hand)) + geom_histogram(binwidth=0.5) +xlab("Handedness") + ylab("Count") + theme_bw()
    
    ggsave(handednessGraphFile)
  }
}

#### other graphs (mostly boxplots)

#accuracy graphs
generateAccGraphsOld=function(dataset,title) {
  library(ggplot2)
  #plot data (all Data by degree and condition, grouped by condition)
  ggplot(dataset,aes(y=acc,x=deg,group=deg,fill=cond)) + 
    stat_boxplot(na.rm=TRUE, position=position_dodge(2),geom = "errorbar") +
    geom_boxplot(na.rm=TRUE, position=position_dodge(2)) +theme_bw() + facet_wrap(~cond) +
    stat_summary(na.rm=TRUE, fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
    labs(x="degrees(°)",y="accuracy",fill="condition")
  ggsave(paste("figs/",title,"PlotByDegreeConditionAxis.png",sep=""))
  
  #plot data (mean Data by degree and condition, grouped by degrees)
  ggplot(dataset,aes(y=acc,x=cond,group=cond,fill=cond)) + 
    stat_boxplot(na.rm=TRUE, position=position_dodge(2),geom = "errorbar") +
    geom_boxplot(na.rm=TRUE, position=position_dodge(2)) +theme_bw() + facet_wrap(~deg) +
    stat_summary(na.rm=TRUE, fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
    labs(x="condition",y="accuracy") + guides(fill=FALSE)
  ggsave(paste("figs/",title,"PlotByDegreeCondition.png",sep=""))
}

#generate reaction time graphs
generateRTGraphsOld=function(dataset,title,outliers=TRUE) {
  library(ggplot2)
  #plot data (all Data by degree)
  ggplot(dataset,aes(y=reactionTime,x=deg,group=deg)) + 
    stat_boxplot(na.rm=TRUE, position=position_dodge(2),geom = "errorbar") +
    geom_boxplot(na.rm=TRUE, position=position_dodge(2)) +theme_bw() +
    labs(x="degrees(°)",y="Reaction Time(ms)")
  ggsave(paste("figs/",title,"PlotByDegree.png",sep=""))
  
  if(outliers) {
    library(plyr)
    dataset=ddply(dataset, .(deg), mutate, Q1=quantile(reactionTime, 1/4,na.rm=T), Q3=quantile(reactionTime, 3/4,na.rm=T), IQR=Q3-Q1, upper.limit=Q3+1.5*IQR, lower.limit=Q1-1.5*IQR)
    
    #plot data (all Data by degree with means and 3*sd)
    ggplot(dataset,aes(y=reactionTime,x=deg,group=deg)) + 
      stat_boxplot(na.rm=TRUE, position=position_dodge(2),geom = "errorbar") +
      geom_boxplot(na.rm=TRUE, position=position_dodge(2)) +theme_bw() +
      stat_summary(na.rm=TRUE, fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
      stat_summary(fun.data = meanSd3, geom = "pointrange", position = position_dodge(1), fill="red", color="red") +
      labs(x="degrees(°)",y="Reaction Time(ms)")
    ggsave(paste("figs/",title,"PlotByDegreeWithMeanSd.png",sep=""))
    
    #plot data (all Data by degree with Outlier coloring)
    ggplot(dataset,aes(y=reactionTime,x=deg,group=deg)) + 
      stat_boxplot(na.rm=TRUE, position=position_dodge(2),geom = "errorbar") +
      geom_boxplot(na.rm=TRUE, position=position_dodge(2)) +theme_bw() +
      labs(x="degrees(°)",y="Reaction Time(ms)") +
      stat_summary(na.rm=TRUE, fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
      geom_point(data=dataset[dataset$reactionTime > dataset$upper.limit | dataset$reactionTime < dataset$lower.limit,], aes(col=typeOutlier))
    ggsave(paste("figs/",title,"PlotByDegreeOutlierColor.png",sep=""))
    
    #plot data (all Data by degree without outliers)
    ggplot(dataset[which(!dataset$outlier),],aes(y=reactionTime,x=deg,group=deg)) + 
      stat_boxplot(na.rm=TRUE, position=position_dodge(2),geom = "errorbar") +
      geom_boxplot(na.rm=TRUE, position=position_dodge(2)) +theme_bw() +
      labs(x="degrees(°)",y="Reaction Time(ms)") +
      stat_summary(na.rm=TRUE, fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") 
    ggsave(paste("figs/",title,"PlotByDegreeNoOutlier.png",sep=""))
  }
  #plot data (all Data by degree and condition, grouped by condition)
  ggplot(dataset,aes(y=reactionTime,x=deg,group=deg,fill=cond)) + 
    stat_boxplot(na.rm=TRUE, position=position_dodge(2),geom = "errorbar") +
    geom_boxplot(na.rm=TRUE, position=position_dodge(2)) +theme_bw() + facet_wrap(~cond) + 
    stat_summary(na.rm=TRUE, fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") + 
    labs(x="degrees(°)",y="Reaction Time(ms)",fill="condition")
  ggsave(paste("figs/",title,"PlotByConditionDegree.png",sep=""))
  
  #plot data (mean Data by degree and condition, grouped by degrees)
  ggplot(dataset,aes(y=reactionTime,x=cond,group=cond,fill=cond)) + 
    stat_boxplot(na.rm=TRUE, position=position_dodge(2),geom = "errorbar") +
    geom_boxplot(na.rm=TRUE, position=position_dodge(2)) +theme_bw() + facet_wrap(~deg) + 
    stat_summary(na.rm=TRUE, fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black") +
    labs(x="condition",y="Reaction Time(ms)") + guides(fill=FALSE)
  ggsave(paste("figs/",title,"PlotByDegreeCondition.png",sep=""))
}
