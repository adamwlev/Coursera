setwd("~/Downloads")

data <- read.csv("repdata-data-StormData.csv")
library(dplyr)
data <- tbl_df(data)
data <- data %>% transform(Cropunits = ifelse(data$CROPDMGEXP=='K' | data$CROPDMGEXP=='k',1000,
       ifelse(data$CROPDMGEXP=='M' | data$CROPDMGEXP=='m',10^6,
              ifelse(data$CROPDMGEXP=='B' | data$CROPDMGEXP=='b',10^9,0))))
data <- data %>% transform(Propunits = ifelse(data$PROPDMGEXP=='K' | data$PROPDMGEXP=='k',1000,
                                              ifelse(data$PROPDMGEXP=='M' | data$PROPDMGEXP=='m',10^6,
                                                     ifelse(data$PROPDMGEXP=='B' | data$PROPDMGEXP=='b',10^9,0))))
data <- data %>%
    transform(Cropdamage = data$CROPDMG*data$Cropunits,
              Propdamage = data$PROPDMG*data$Propunits)

data <- data %>%
    transform(TotalEconDam = data$Cropdamage + data$Propdamage,
              TotalHumDam = data$FATALITIES + data$INJURIES)

data %>% 
    group_by(EVTYPE) %>% 
    summarize(TotalEconDam=sum(TotalEconDam),TotalHumDam=sum(TotalHumDam)) %>%
    arrange(desc(TotalHumDam))

top7econ_ave <- (data %>% 
    group_by(EVTYPE) %>% 
    summarize(AverageEconDam=mean(TotalEconDam),Numofoccurances=n()) %>%
    filter(Numofoccurances>=30) %>%
    arrange(desc(AverageEconDam)))[1:7,] %>% collect %>% .[['EVTYPE']]

df <- data %>% filter(EVTYPE %in% top7econ_ave) %>% select(EVTYPE,TotalEconDam)
require(reshape2)
df.m <- melt(df, id.var = "EVTYPE")
require(lattice)
bwplot(value~EVTYPE,data=df.m,scales=list(x=list(cex=.37),y=list(log=10)),
       panel=function(x,y,...){
            panel.bwplot(x,y,...)
            panel.stripplot(x,y,...)
        })
