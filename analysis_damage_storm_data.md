#Analysis of Harmful and Damaging Evens in Storm Data

## Preparation of The Data
At the very beginning, we need to read the data from the data file and set the plotting environment by include the library of "ggplot". 

```r
library(ggplot2)
setwd("~/Desktop/my_documents/coursera/fall_2015/reproducible_research/project_2")
data=read.csv('./repdata-data-StormData.csv',sep=',',header=TRUE)
```

Since there is no explicity information on the damages of properties and crops, we need to add two new features into the data about these information. 

```r
data$prop.damage <- with(data, PROPDMG * ifelse(PROPDMGEXP == "", 1, ifelse(PROPDMGEXP == 
"K", 1000, ifelse(PROPDMGEXP == "M", 1e+06, ifelse(PROPDMGEXP == "B", 1e+09,-1)))))

data$crop.damage <- with(data, CROPDMG * ifelse(CROPDMGEXP == "", 1, ifelse(CROPDMGEXP == 
"K", 1000, ifelse(CROPDMGEXP == "M", 1e+06, ifelse(CROPDMGEXP == "B", 1e+09,-1)))))
```

## Analysis of the Harm to Population Health
In this section we analyze the most harmful events to population health. According to the information provided by the data, we divide the analysis into two parts: fatal analysis and injury analysis. 
### Fatal Analysis

```r
s.fatal=split(data$FATALITIES,as.factor(data$EVTYPE))
fatal.sum=sapply(s.fatal,sum)
fatal.sum.sorted=fatal.sum[order(fatal.sum,decreasing=TRUE)]
fatal.mean=sapply(s.fatal,mean)
fatal.mean.sorted=fatal.mean[order(fatal.sum,decreasing=TRUE)]
event.fatal.sorted=names(fatal.sum.sorted)
summary.fatal=data.frame(event.type=event.fatal.sorted,total.fatal.number=fatal.sum.sorted,
                         fatal.per.event=fatal.mean.sorted,row.names=NULL)
```
By the code above, we can see the ten most harmful events according to fatal in the table below: 

```r
print(summary.fatal[1:10,])
```

```
##        event.type total.fatal.number fatal.per.event
## 1         TORNADO               5633        0.092874
## 2  EXCESSIVE HEAT               1903        1.134088
## 3     FLASH FLOOD                978        0.018019
## 4            HEAT                937        1.221643
## 5       LIGHTNING                816        0.051796
## 6       TSTM WIND                504        0.002292
## 7           FLOOD                470        0.018558
## 8     RIP CURRENT                368        0.782979
## 9       HIGH WIND                248        0.012270
## 10      AVALANCHE                224        0.580311
```
We can also plot the distribution: 

```r
data.fatal.top10=subset(data,data$EVTYPE %in% summary.fatal[1:10,1])

ggplot(data.fatal.top10,aes(x=as.factor(EVTYPE),y=FATALITIES))+geom_boxplot()+
  ggtitle("Distribution of Fatalities for the most Harmful Events")+ylab("log10 of the number of fatals")+
  xlab("Event Type") + scale_y_log10() + theme_bw()
```

```
## Warning: Removed 394320 rows containing non-finite values (stat_boxplot).
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

### Injury Analysis

```r
s.injur=split(data$INJURIES,as.factor(data$EVTYPE))
injur.sum=sapply(s.injur,sum)
injur.sum.sorted=injur.sum[order(injur.sum,decreasing=TRUE)]
injur.mean=sapply(s.injur,mean)
injur.mean.sorted=injur.mean[order(injur.sum,decreasing=TRUE)]
event.injur.sorted=names(injur.sum.sorted)
summary.injur=data.frame(event.type=event.injur.sorted,total.injury.number=injur.sum.sorted,
                         injuries.per.event=injur.mean.sorted,row.names=NULL)
```
By the code above, we can see the ten most harmful events according to fatal in the table below: 

```r
print(summary.injur[1:10,])
```

```
##           event.type total.injury.number injuries.per.event
## 1            TORNADO               91346           1.506067
## 2          TSTM WIND                6957           0.031631
## 3              FLOOD                6789           0.268064
## 4     EXCESSIVE HEAT                6525           3.888558
## 5          LIGHTNING                5230           0.331979
## 6               HEAT                2100           2.737940
## 7          ICE STORM                1975           0.984546
## 8        FLASH FLOOD                1777           0.032739
## 9  THUNDERSTORM WIND                1488           0.018023
## 10              HAIL                1361           0.004715
```
We can also plot the distribution: 

```r
data.injur.top10=subset(data,data$EVTYPE %in% summary.injur[1:10,1])

ggplot(data.injur.top10,aes(x=as.factor(EVTYPE),y=INJURIES))+geom_boxplot()+
  ggtitle("Distribution of Injuries for the most Harmful Events")+ylab("log10 of the number of injuries")+
  xlab("Event Type") + scale_y_log10() + theme_bw()
```

```
## Warning: Removed 736740 rows containing non-finite values (stat_boxplot).
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


## Analysis of Harm to Economic Damage
In this section we analyze the most harmful events to economic damage. According to the information provided by the data, we divide the analysis into two parts: property damage analysis and crop damage analysis. 
### Property Damage Analysis

```r
s.prop=split(data$prop.damage,as.factor(data$EVTYPE))
prop.sum=sapply(s.prop,sum)
prop.sum.sorted=prop.sum[order(prop.sum,decreasing=TRUE)]
prop.mean=sapply(s.prop,mean)
prop.mean.sorted=prop.mean[order(prop.sum,decreasing=TRUE)]
event.prop.sorted=names(prop.sum.sorted)
summary.prop=data.frame(event.type=event.prop.sorted,total.prop.damage.number=prop.sum.sorted,
                        prop.damage.per.event=prop.mean.sorted,row.names=NULL)
```
By the code above, we can see the ten most harmful events according to property damage in the table below: 

```r
print(summary.prop[1:10,])
```

```
##           event.type total.prop.damage.number prop.damage.per.event
## 1              FLOOD                1.447e+11               5711826
## 2  HURRICANE/TYPHOON                6.931e+10             787566364
## 3            TORNADO                5.693e+10                938562
## 4        STORM SURGE                4.332e+10             165990559
## 5        FLASH FLOOD                1.614e+10                297378
## 6               HAIL                1.573e+10                 54484
## 7          HURRICANE                1.187e+10              68208730
## 8     TROPICAL STORM                7.704e+09              11165059
## 9       WINTER STORM                6.688e+09                585017
## 10         HIGH WIND                5.270e+09                260738
```
We can also plot the distribution: 

```r
data.prop.top10=subset(data,data$EVTYPE %in% summary.prop[1:10,1])

ggplot(data.prop.top10,aes(x=as.factor(EVTYPE),y=prop.damage))+geom_boxplot()+
  ggtitle("Distribution of Properties Damage for the most Harmful Events")+ylab("log10 of the number of properties damage")+
  xlab("Event Type") + theme_bw()+scale_y_log10()
```

```
## Warning: NaNs produced
## Warning: Removed 361979 rows containing non-finite values (stat_boxplot).
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

### Crop Damage Analysis

```r
s.crop=split(data$crop.damage,as.factor(data$EVTYPE))
crop.sum=sapply(s.crop,sum)
crop.sum.sorted=crop.sum[order(crop.sum,decreasing=TRUE)]
crop.mean=sapply(s.crop,mean)
crop.mean.sorted=crop.mean[order(crop.sum,decreasing=TRUE)]
event.crop.sorted=names(crop.sum.sorted)
summary.crop=data.frame(event.type=event.crop.sorted,total.crop.damage.number=crop.sum.sorted,
                        crop.damage.per.event=crop.mean.sorted,row.names=NULL)
```
By the code above, we can see the ten most harmful events according to property damage in the table below: 

```r
print(summary.crop[1:10,])
```

```
##           event.type total.crop.damage.number crop.damage.per.event
## 1            DROUGHT                1.397e+10               5615983
## 2              FLOOD                5.662e+09                223563
## 3        RIVER FLOOD                5.029e+09              29072017
## 4          ICE STORM                5.022e+09               2503546
## 5               HAIL                3.026e+09                 10481
## 6          HURRICANE                2.742e+09              15758103
## 7  HURRICANE/TYPHOON                2.608e+09              29634918
## 8        FLASH FLOOD                1.421e+09                 26186
## 9       EXTREME COLD                1.293e+09               1974005
## 10      FROST/FREEZE                1.094e+09                815265
```
We can also plot the distribution: 

```r
data.crop.top10=subset(data,data$EVTYPE %in% summary.crop[1:10,1])

ggplot(data.crop.top10,aes(x=as.factor(EVTYPE),y=crop.damage))+geom_boxplot()+
  ggtitle("Distribution of crops Damage for the most Harmful Events")+ylab("log10 of the number of crops damage")+
  xlab("Event Type") + theme_bw()+scale_y_log10()
```

```
## Warning: NaNs produced
## Warning: Removed 361433 rows containing non-finite values (stat_boxplot).
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


