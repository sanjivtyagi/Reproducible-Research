---
title: "Storms and other severe weather events analysis in USA"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Synopsis:

This project is performing an impact analysis of storms and other severe weather events on public health and economic problems for communities and municipalities. Based on  U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database collected between 1950 - 2011, evaluation of injuries, and property damage is done. This data also help to take any preventive steps which can reduce in any damage.



## Data Processing :

Set current directory and set all library package

```{r dataprocess, echo = TRUE}
setwd("/Users/styagi/Documents/coursera/course5")

library(ggplot2)
library(plyr)

if (!"stormdata.csv" %in% dir("./")) {
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "stormdata.csv.bz2")
    bunzip2("stormdata.csv.bz2", overwrite=T, remove=F)
}


stromdata <- read.csv("stormdata.csv")


stromdata$year <- as.numeric(format(as.Date(stromdata$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"),"%Y"))

```

## Impact on Public Health :

performing an analysis to identify fatalities and injuries in different  events

```{r publichhealth, echo = TRUE}

stromcol <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
stromsel <- stromdata[stromcol]

unique(stromsel$PROPDMGEXP)

unique(stromsel$CROPDMGEXP)


fataltot <- aggregate(stromsel$FATALITIES, by=list(EVTYPE= stromsel$EVTYPE) , FUN=sum)
fatalsort <- head(fataltot[order(-fataltot$x),],n=15)
fatalsort
barplot(fatalsort$x,main="No of Fatalities" ,  ylab="Fatalities",names.arg=fatalsort$EVTYPE,las=3)

injurtot <- aggregate(stromsel$INJURIES, by=list(EVTYPE= stromsel$EVTYPE) , FUN=sum)
injursort <- head(injurtot[order(-injurtot$x),],n=15)


injursort

barplot(injursort$x,main="No of Injuries" , ylab="Injuries",names.arg=injursort$EVTYPE,las=3)
```

## Impact on Economy :

performing an analysis to identify economic damage in different  events

Above graphs shows that Tornodo has caused most Fatalities and Insuries with in US.  

```{r propertycorp, echo = TRUE}

xformData <- function(dataset = stromsel) {
        
        #Property Damage
        dataset$PROPDMGEXP <- as.character(dataset$PROPDMGEXP)
        dataset$PROPDMGEXP = gsub("\\-|\\+|\\?","0",dataset$PROPDMGEXP)
        dataset$PROPDMGEXP = gsub("B|b", "9", dataset$PROPDMGEXP)
        dataset$PROPDMGEXP = gsub("M|m", "6", dataset$PROPDMGEXP)
        dataset$PROPDMGEXP = gsub("K|k", "3", dataset$PROPDMGEXP)
        dataset$PROPDMGEXP = gsub("H|h", "2", dataset$PROPDMGEXP)
        dataset$PROPDMGEXP <- as.numeric(dataset$PROPDMGEXP)
        dataset$PROPDMGEXP[is.na(dataset$PROPDMGEXP)] = 0
        dataset$ActPropDam<- dataset$PROPDMG * 10^dataset$PROPDMGEXP
        
        #crop damage
        dataset$CROPDMGEXP <- as.character(dataset$CROPDMGEXP)
        dataset$CROPDMGEXP = gsub("\\-|\\+|\\?","0",dataset$CROPDMGEXP)
        dataset$CROPDMGEXP = gsub("B|b", "9", dataset$CROPDMGEXP)
        dataset$CROPDMGEXP = gsub("M|m", "6", dataset$CROPDMGEXP)
        dataset$CROPDMGEXP = gsub("K|k", "3", dataset$CROPDMGEXP)
        dataset$CROPDMGEXP = gsub("H|h", "2", dataset$CROPDMGEXP)
        dataset$CROPDMGEXP <- as.numeric(dataset$CROPDMGEXP)
        dataset$CROPDMGEXP[is.na(dataset$CROPDMGEXP)] = 0
        dataset$ActCropDam<- dataset$CROPDMG * 10^dataset$CROPDMGEXP
        
        return(dataset)
} 

stromsel <- xformData(stromsel)



propertytot <- aggregate(stromsel$ActPropDam, by=list(EVTYPE= stromsel$EVTYPE) , FUN=sum)
propertysort <- head(propertytot[order(-propertytot$x),],n=15)

barplot(propertysort$x,main="Total cost of property damage" ,  ylab="Property Damage",names.arg=propertysort$EVTYPE,las=3)
 

cropdamtot <- aggregate(stromsel$ActCropDam, by=list(EVTYPE= stromsel$EVTYPE) , FUN=sum)
cropdamsort <- head(cropdamtot[order(-cropdamtot$x),],n=15)

barplot(cropdamsort$x,main="Total cost of damaged corp" ,  ylab="Damaged Corp",names.arg=cropdamsort$EVTYPE,las=3)
```

The weather event flood and drought has caused major property and crop damage.

## Result :

Different weather event like tornodo, flood, drought and typhoon have caused differnet damaged to different things While Tornodo has caused major fatalities and injuries, flood caused major property damaged and drought has caused major crop damaged.

