setwd('~/Desktop/DSS/5-reproducible research/project2/')

library(ggplot2)

#
# ANALYZE CASUALTIES
#
raw_data<-read.csv('repdata-data-StormData.csv',header = TRUE,colClasses = "character",na.strings = c("?","-"))

events<-readLines("evtype.txt")

events<-paste0("^",(substr(events,1,nchar(events)-2)),"$",collapse = "|") # genetate regular expression

raw_data$BGN_DATE_new <- with(raw_data, paste(BGN_DATE, BGN_TIME))
raw_data$BGN_DATE_new <- with(raw_data, strptime(BGN_DATE_new,
                                               format = "%m/%d/%Y %T"))     
raw_data$END_DATE_new <- with(raw_data, paste(END_DATE, END_TIME))
raw_data$END_DATE_new <- with(raw_data, strptime(END_DATE_new,
                                               format = "%m/%d/%Y %T"))

raw_data <- raw_data[raw_data$BGN_DATE_new >= strptime("1990-01-01", "%F"),] # include only events starting from 1990

entries<-grepl(events,raw_data$EVTYPE,ignore.case = T)

tidy_data<-raw_data[entries,]
tidy_data$EVTYPE<-toupper(tidy_data$EVTYPE)
tidy_data$EVTYPE[tidy_data$EVTYPE=="MARINE TSTM WIND"] = "MARINE THUNDERSTORM WIND" # remove unnecessary events
tidy_data$EVTYPE[tidy_data$EVTYPE=="TSTM WIND"] = "THUNDERSTORM WIND"


tidy_data$FATALITIES<-as.numeric(tidy_data$FATALITIES)
tidy_data$INJURIES<-as.numeric(tidy_data$INJURIES)

public_health_injury<-aggregate(tidy_data$INJURIES~tidy_data$EVTYPE,FUN=sum,na.rm=TRUE)
colnames(public_health_injury)<-c("event","no.injuries")
public_health_fatality<-aggregate(tidy_data$FATALITIES~tidy_data$EVTYPE,FUN=sum,na.rm=TRUE)
colnames(public_health_fatality)<-c("event","no.fatalities")

public_health_fatality<-public_health_fatality[order(public_health_fatality$no.fatalities,decreasing = TRUE),]
public_health_injury<-public_health_injury[order(public_health_injury$no.injuries,decreasing = TRUE),]

#
# plot...  
# 1) no.injuries vs. year
# 2) no.fatalities vs. year
#
ggplot(public_health_injury,aes(reorder(event,-no.injuries),no.injuries))+
  geom_bar(stat="identity",fill="purple")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(x="weather events")+
  labs(y="total number of injuries caused")+
  labs(title="Total Number of Injuries Caused by Different Weather Events")+
  theme(panel.background=element_rect("yellow"))

ggplot(public_health_fatality,aes(reorder(event,-no.fatalities),no.fatalities))+
  geom_bar(stat="identity",fill="purple")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(x="weather events")+
  labs(y="total number of fatalities caused")+
  labs(title="Total Number of Fatalities Caused by Different Weather Events")+
  theme(panel.background=element_rect("yellow"))
#
# ANALYZING ECONOMIC DAMAGE
#
  tidy_data$PROPDMGEXP<-toupper(tidy_data$l)  
  tidy_data$CROPDMGEXP<-toupper(tidy_data$CROPDMGEXP)
  tidy_data$PROPDMG<-as.numeric(tidy_data$PROPDMG)
  tidy_data$CROPDMG<-as.numeric(tidy_data$CROPDMG)
  economic_damage<-tidy_data[tidy_data$PROPDMG > 0.0 | tidy_data$CROPDMG > 0.0, c(8,25:28)]
  colnames(economic_damage)<-c("events","prop.dmg","prop.dmg.exp","crop.dmg","crop.dmg.exp")
  
  unique(economic_damage$prop.dmg.exp)
  unique(economic_damage$crop.dmg.exp)
  
  economic_damage$prop.dmg.exp[!grepl("^(H|K|M|B)$",economic_damage$prop.dmg.exp)]=0.0
  economic_damage$crop.dmg.exp[!grepl("^(H|K|M|B)$",economic_damage$crop.dmg.exp)]=0.0
#
# convert prefixes into numbers 
#
  economic_damage$crop.dmg.exp<-gsub("K","1000",economic_damage$crop.dmg.exp)
  economic_damage$crop.dmg.exp<-gsub("M","1000000",economic_damage$crop.dmg.exp)
  economic_damage$crop.dmg.exp<-gsub("B","1000000000",economic_damage$crop.dmg.exp)
  
  economic_damage$prop.dmg.exp<-gsub("H","100",economic_damage$prop.dmg.exp)
  economic_damage$prop.dmg.exp<-gsub("K","1000",economic_damage$prop.dmg.exp)
  economic_damage$prop.dmg.exp<-gsub("M","1000000",economic_damage$prop.dmg.exp)
  economic_damage$prop.dmg.exp<-gsub("B","1000000000",economic_damage$prop.dmg.exp)
  
  economic_damage$crop.dmg.exp<-as.numeric(economic_damage$crop.dmg.exp)
  economic_damage$prop.dmg.exp<-as.numeric(economic_damage$prop.dmg.exp)
  
  economic_damage$total=economic_damage$prop.dmg*economic_damage$prop.dmg.exp+economic_damage$crop.dmg*economic_damage$crop.dmg.exp
                              
  total_economic_damage<-aggregate(economic_damage$total~economic_damage$events,FUN=sum,na.rm=TRUE)
  colnames(total_economic_damage)<-c("event","total.usd")

#
# plot...
# 1) ecomomic damage vs. year  
#
  ggplot(total_economic_damage,aes(reorder(event,-total.usd),total.usd))+
    geom_bar(stat="identity",fill="purple")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
    labs(x="weather events")+
    labs(y="total damage, USD")+
    labs(title="Damage (USD) Caused by Different Weather Events")+
    theme(panel.background=element_rect("yellow"))
  
 
#SCRATCH 
#  
#  !grepl("$H|K|M|B)",economic_damage$prop.dmg.exp)
#  
#  grep("$H|K|M|B)",economic_damage$prop.dmg.exp)
#health_damage <- tidy_set[,c(8, 23, 24, 25:28)]
#health_damage <- summarize(group_by(health_damage, EVTYPE),
#                           FATALITIES = sum(FATALITIES),
#                           INJURIES = sum(INJURIES))


#tidy_data<-raw_data[grep(events,raw_data$EVTYPE,ignore.case = TRUE),]
#junk<-raw_data[!(grep(events,raw_data$EVTYPE,ignore.case = TRUE)),]

#PROPDMG - property damage
#CROPDMG - crop damage
#PROPDMGEXP - alphabetical character sugnifying the magnitude of the number
#CROPDMGEXP - 
