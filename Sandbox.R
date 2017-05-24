library(ggplot2)
library(scales)

setwd("~/R Working Directory")


# Download data

download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              destfile = "./Data/repdata%2Fdata%2FStormData.csv.bz2")

Storm_Data <- read.csv(file = "./Data/repdata%2Fdata%2FStormData.csv.bz2")


# Let's aggregate per event type, just to check the number of events
Storm_Data_Occurrences <- aggregate(Storm_Data$REFNUM, by = Storm_Data[c('EVTYPE')], length)
Storm_Data_Occurrences <- Storm_Data_Occurrences[order(-Storm_Data_Occurrences$x),]




# Let's rename Event type for the labels of Event type that look very similar
Storm_Data$EVTYPE[Storm_Data$EVTYPE == "TSTM WIND"] <- "THUNDERSTORM WIND"
Storm_Data$EVTYPE[Storm_Data$EVTYPE == "THUNDERSTORM WINDS"] <- "THUNDERSTORM WIND"
Storm_Data$EVTYPE[Storm_Data$EVTYPE == "URBAN FLOOD"] <- "FLOOD"
Storm_Data$EVTYPE[Storm_Data$EVTYPE == "FLASH FLOODING"] <- "FLASH FLOOD"
Storm_Data$EVTYPE[Storm_Data$EVTYPE == "RIVER FLOOD"] <- "FLOOD"
Storm_Data$EVTYPE[Storm_Data$EVTYPE == "MARINE TSTM WIND"] <- "MARINE THUNDERSTORM WIND"
Storm_Data$EVTYPE[Storm_Data$EVTYPE == "WILD/FOREST FIRE"] <- "WILDFIRE"


# Let's sum fatalities and injuries for each event type
Storm_Data_Fatalities_per_eventtype<- aggregate(Storm_Data$FATALITIES, by = Storm_Data[c('EVTYPE')], sum)

Fatalities <- head(Storm_Data_Fatalities_per_eventtype[order(-Storm_Data_Fatalities_per_eventtype$x),],20)

Storm_Data_Injuries_per_eventtype<- aggregate(Storm_Data$INJURIES, by = Storm_Data[c('EVTYPE')], sum)

Injuries <- head(Storm_Data_Injuries_per_eventtype[order(-Storm_Data_Injuries_per_eventtype$x),],20)

png(filename="./Coursera_May_2017_Reproducible_Research/Fatalities.png",width=960,height=480)
p <-  ggplot(Fatalities, aes(x = reorder(EVTYPE,-x),y = x))
p <- p+ geom_bar(stat  = "identity")
p <- p+ ggtitle("Top 20 Event types in terms of Fatalities")
p <- p+ geom_text(aes(label=comma(x), vjust=-0.5, size=2.5))
p <- p+scale_y_continuous(limits=c(0,6000), labels = comma)
p <- p+ ylab("Number of fatalities") + xlab("Event type")
p <- p+ theme(legend.position="none", plot.title = element_text(size = 20),axis.title = element_text(size = 20),axis.text.y = element_text(size=15), axis.text.x = element_text(angle=90,size=15))
p
dev.off()


png(filename="./Coursera_May_2017_Reproducible_Research/Injuries.png",width=960,height=480)
p <-  ggplot(Injuries, aes(x = reorder(EVTYPE,-x),y = x))
p <- p+ geom_bar(stat = "identity")
p <- p+ ggtitle("Top 20 Event types in terms of Injuries")
p <- p+ geom_text(aes(label=comma(x), vjust=-0.5, size=2.5))
p <- p+scale_y_continuous(limits=c(0,100000), labels = comma )
p <- p+ ylab("Number of injuries") + xlab("Event type")
p <- p+ theme(legend.position="none", plot.title = element_text(size = 20),axis.title = element_text(size = 20),axis.text.y = element_text(size=15), axis.text.x = element_text(angle=90,size=15))
p
dev.off()


# In order to compute the data about damages,we have to create a column that would multiply PROPDMG by the value of PROPDMGEXP
# if k or K, multiply by 1000, if m or M, by 1000000, if B, by 10 exp9, else do not multiply I guess


Storm_Data$PROPDMG[Storm_Data$PROPDMGEXP == "B"]

Storm_Data$PROPDMGEXP2[grepl("[kK]",Storm_Data$PROPDMGEXP)] <- 1000
Storm_Data$PROPDMGEXP2[grepl("[mM]",Storm_Data$PROPDMGEXP)] <- 1000000
Storm_Data$PROPDMGEXP2[grepl("[bB]",Storm_Data$PROPDMGEXP)] <- 1000000000
Storm_Data$PROPDMGEXP2[grepl("[mM]",Storm_Data$PROPDMGEXP) + grepl("[bB]",Storm_Data$PROPDMGEXP) + grepl("[kK]",Storm_Data$PROPDMGEXP) == 0 ] <- 0

Storm_Data$CROPDMGEXP2[grepl("[kK]",Storm_Data$CROPDMGEXP)] <- 1000
Storm_Data$CROPDMGEXP2[grepl("[mM]",Storm_Data$CROPDMGEXP)] <- 1000000
Storm_Data$CROPDMGEXP2[grepl("[bB]",Storm_Data$CROPDMGEXP)] <- 1000000000
Storm_Data$CROPDMGEXP2[grepl("[mM]",Storm_Data$CROPDMGEXP) + grepl("[bB]",Storm_Data$CROPDMGEXP) + grepl("[kK]",Storm_Data$CROPDMGEXP) == 0 ] <- 0

Storm_Data$PROPDMG2 <- Storm_Data$PROPDMG * Storm_Data$PROPDMGEXP2
Storm_Data$CROPDMG2 <- Storm_Data$CROPDMG * Storm_Data$CROPDMGEXP2
Storm_Data$Damage <- Storm_Data$PROPDMG2 + Storm_Data$CROPDMG2

Damage_per_eventtype <- aggregate(Storm_Data$Damage, by = Storm_Data[c('EVTYPE')], sum)
Damage_per_eventtype$x <- Damage_per_eventtype$x / 1000000000
Damages <- head(Damage_per_eventtype[order(-Damage_per_eventtype$x),],20)


png(filename="./Coursera_May_2017_Reproducible_Research/Damages.png",width=960,height=480)
p <-  ggplot(Damages, aes(x = reorder(EVTYPE,-x),y = x))
p <- p+ geom_bar(stat = "identity")
p <- p+ ggtitle("Top 20 Event types in terms of Damages")
p <- p+ geom_text(aes(label=sprintf("%1.0f", round(x,digits = 0)), vjust=-0.5, size=2.5))
p <- p+scale_y_continuous(limits=c(0,200), labels = comma )
p <- p+ ylab("Billion Dollars") + xlab("Event type")
p <- p+ theme(legend.position="none", plot.title = element_text(size = 20),axis.title = element_text(size = 20),axis.text.y = element_text(size=15), axis.text.x = element_text(angle=90,size=15))
p
dev.off()





# as.Date(Storm_Data$BGN_DATE,"%m/%d/%Y")
# min(as.Date(Storm_Data$BGN_DATE,"%m/%d/%Y"))
# max(as.Date(Storm_Data$BGN_DATE,"%m/%d/%Y"))


