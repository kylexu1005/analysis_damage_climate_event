library(ggplot2)
setwd("~/Desktop/my_documents/coursera/fall_2015/reproducible_research/project_2")
data=read.csv('./repdata-data-StormData.csv',sep=',',header=TRUE)

data$prop.damage <- with(data, PROPDMG * ifelse(PROPDMGEXP == "", 1, ifelse(PROPDMGEXP == 
"K", 1000, ifelse(PROPDMGEXP == "M", 1e+06, ifelse(PROPDMGEXP == "B", 1e+09,-1)))))

data$crop.damage <- with(data, CROPDMG * ifelse(CROPDMGEXP == "", 1, ifelse(CROPDMGEXP == 
"K", 1000, ifelse(CROPDMGEXP == "M", 1e+06, ifelse(CROPDMGEXP == "B", 1e+09,-1)))))

################### fatal ##############################
s.fatal=split(data$FATALITIES,as.factor(data$EVTYPE))
fatal.sum=sapply(s.fatal,sum)
fatal.sum.sorted=fatal.sum[order(fatal.sum,decreasing=TRUE)]
fatal.mean=sapply(s.fatal,mean)
fatal.mean.sorted=fatal.mean[order(fatal.sum,decreasing=TRUE)]
event.fatal.sorted=names(fatal.sum.sorted)
summary.fatal=data.frame(event.type=event.fatal.sorted,total.fatal.number=fatal.sum.sorted,
                         fatal.per.event=fatal.mean.sorted,row.names=NULL)
print(summary.fatal[1:10,])

data.fatal.top10=subset(data,data$EVTYPE %in% summary.fatal[1:10,1])

ggplot(data.fatal.top10,aes(x=as.factor(EVTYPE),y=FATALITIES))+geom_boxplot()+
  ggtitle("Distribution of Fatalities for the most Harmful Events")+ylab("log10 of the number of fatals")+
  xlab("Event Type") + scale_y_log10() + theme_bw()

################### injure ##############################
s.injur=split(data$INJURIES,as.factor(data$EVTYPE))
injur.sum=sapply(s.injur,sum)
injur.sum.sorted=injur.sum[order(injur.sum,decreasing=TRUE)]
injur.mean=sapply(s.injur,mean)
injur.mean.sorted=injur.mean[order(injur.sum,decreasing=TRUE)]
event.injur.sorted=names(injur.sum.sorted)
summary.injur=data.frame(event.type=event.injur.sorted,total.injury.number=injur.sum.sorted,
                         injuries.per.event=injur.mean.sorted,row.names=NULL)
print(summary.injur[1:10,])

data.injur.top10=subset(data,data$EVTYPE %in% summary.injur[1:10,1])

ggplot(data.injur.top10,aes(x=as.factor(EVTYPE),y=INJURIES))+geom_boxplot()+
  ggtitle("Distribution of Injuries for the most Harmful Events")+ylab("log10 of the number of injuries")+
  xlab("Event Type") + scale_y_log10() + theme_bw()

################### propdamage ##############################
s.prop=split(data$prop.damage,as.factor(data$EVTYPE))
prop.sum=sapply(s.prop,sum)
prop.sum.sorted=prop.sum[order(prop.sum,decreasing=TRUE)]
prop.mean=sapply(s.prop,mean)
prop.mean.sorted=prop.mean[order(prop.sum,decreasing=TRUE)]
event.prop.sorted=names(prop.sum.sorted)
summary.prop=data.frame(event.type=event.prop.sorted,total.prop.damage.number=prop.sum.sorted,
                        prop.damage.per.event=prop.mean.sorted,row.names=NULL)
print(summary.prop[1:10,])

data.prop.top10=subset(data,data$EVTYPE %in% summary.prop[1:10,1])

ggplot(data.prop.top10,aes(x=as.factor(EVTYPE),y=prop.damage))+geom_boxplot()+
  ggtitle("Distribution of Properties Damage for the most Harmful Events")+ylab("log10 of the number of properties damage")+
  xlab("Event Type") + theme_bw()+scale_y_log10()

################### cropdamage ##############################
s.crop=split(data$crop.damage,as.factor(data$EVTYPE))
crop.sum=sapply(s.crop,sum)
crop.sum.sorted=crop.sum[order(crop.sum,decreasing=TRUE)]
crop.mean=sapply(s.crop,mean)
crop.mean.sorted=crop.mean[order(crop.sum,decreasing=TRUE)]
event.crop.sorted=names(crop.sum.sorted)
summary.crop=data.frame(event.type=event.crop.sorted,total.crop.damage.number=crop.sum.sorted,
                        crop.damage.per.event=crop.mean.sorted,row.names=NULL)
print(summary.crop[1:10,])

data.crop.top10=subset(data,data$EVTYPE %in% summary.crop[1:10,1])

ggplot(data.crop.top10,aes(x=as.factor(EVTYPE),y=crop.damage))+geom_boxplot()+
  ggtitle("Distribution of crops Damage for the most Harmful Events")+ylab("log10 of the number of crops damage")+
  xlab("Event Type") + theme_bw()+scale_y_log10()

