Storms and other severe weather events can cause both public health and
economic problems for communities and municipalities. Many severe events
can result in fatalities, injuries, and property damage, and preventing
such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and
Atmospheric Administration’s (NOAA) storm database. This database tracks
characteristics of major storms and weather events in the United States,
including when and where they occur, as well as estimates of any
fatalities, injuries, and property damage.

The analysis answers the follwoing two questions.

1.  Across the United States, which types of events are most harmful
    with respect to population health?  
2.  Across the United States, which types of events have the greatest
    economic consequences?

Read in the file.
-----------------

     x <- read.csv("repdata_data_StormData.csv.bz2")

Clean data.
-----------

    x <- select(x, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
    x$EVTYPE <- str_to_upper(x$EVTYPE)
    x$EVTYPE<- str_trim(x$EVTYPE)
    x<- arrange(x,EVTYPE)

The origianl data needed the event types cleaned to become consistent
with the event categories listed in Table 1 from the [Sortm Data Code
Book](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)  
THe follwoing code cleans the event type names:

    x$EVTYPE[grepl("Astronomical", x$EVTYPE, ignore.case = TRUE)] <- "Astronomical Low Tide"
    x$EVTYPE[grepl("Avalanche|AVALANCE|Landslide|Mudslide", x$EVTYPE, ignore.case = TRUE)] <- "Avalanche"
    x$EVTYPE[grepl("Blizzard", x$EVTYPE, ignore.case = TRUE)] <- "Blizzard"
    x$EVTYPE[grepl("Coastal|Seas|Surf|Swells", x$EVTYPE, ignore.case = TRUE)] <- "Coastal Flood"
    x$EVTYPE[grepl("Cold/Wind Chill|COLD|Cool|Freez|snow", x$EVTYPE, ignore.case = TRUE)] <- "Cold/Wind Chill"
    x$EVTYPE[grepl("Excessive Heat|Heat", x$EVTYPE, ignore.case = TRUE)] <- "Excessive Heat"
    x$EVTYPE[grepl("Flood", x$EVTYPE, ignore.case = TRUE)] <- "Flood"
    x$EVTYPE[grepl("Hail", x$EVTYPE, ignore.case = TRUE)] <- "Hail"
    x$EVTYPE[grepl("RAIN|Percipatation", x$EVTYPE, ignore.case = TRUE)] <- "Heavy Rain"
    x$EVTYPE[grepl("Hurricane", x$EVTYPE, ignore.case = TRUE)] <- "Hurricane"
    x$EVTYPE[grepl("High Winds|Wind", x$EVTYPE, ignore.case = TRUE)] <- "High Winds"
    x$EVTYPE[grepl("Lightning", x$EVTYPE, ignore.case = TRUE)] <- "Lightning"
    x$EVTYPE[grepl("Thunderstorm", x$EVTYPE, ignore.case = TRUE)] <- "Thunderstorm"
    x$EVTYPE[grepl("Tornado", x$EVTYPE, ignore.case = TRUE)] <- "Tornado"
    x$EVTYPE[grepl("volcan", x$EVTYPE, ignore.case = TRUE)] <- "Volcanic Ash"
    x$EVTYPE[grepl("Tropical", x$EVTYPE, ignore.case = TRUE)] <- "Tropical Storm"

Now we will create a data table that adds the fatalities and injuries to
get the total harm.

    harm <- select(x, EVTYPE, FATALITIES, INJURIES)
    harm$total <- harm$FATALITIES + harm$INJURIES
    totalharm <- harm %>% group_by(EVTYPE) %>% summarize(total = sum(total))
    totalharm <- arrange(totalharm, desc(total))

Now we will create a data table for damage by combing the damage amount
with the exponential column to obtain a new column.

    econ <- select(x, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
    econ$PROPDMGEXP <- as.character(econ$PROPDMGEXP)
    econ$CROPDMGEXP <- as.character(econ$CROPDMGEXP)
    econ$propdamage <- 0
    econ$cropdamage <- 0

    econ$propdamage[grepl("[Hh]", econ$PROPDMGEXP,ignore.case = TRUE)]<-100
    econ$propdamage[grepl("[Kk]", econ$PROPDMGEXP,ignore.case = TRUE)]<-1000
    econ$propdamage[grepl("[Mm]", econ$PROPDMGEXP,ignore.case = TRUE)]<-1000000
    econ$propdamage[grepl("[Bb]", econ$PROPDMGEXP,ignore.case = TRUE)]<-1000000000
    econ$propdamage[grepl("-", econ$PROPDMGEXP,ignore.case = TRUE)]<-0
    econ$propdamage[grepl("+", econ$PROPDMGEXP,ignore.case = TRUE)]<-0

    econ$newprop <- econ$PROPDMG*econ$propdamage

    econ$cropdamage[grepl("0", econ$CROPDMGEXP,ignore.case = TRUE)]<-1
    econ$cropdamage[grepl("2", econ$CROPDMGEXP,ignore.case = TRUE)]<-100
    econ$cropdamage[grepl("[Kk]", econ$CROPDMGEXP,ignore.case = TRUE)]<-1000
    econ$cropdamage[grepl("[Mm]", econ$CROPDMGEXP,ignore.case = TRUE)]<-1000000
    econ$cropdamage[grepl("[Bb]", econ$CROPDMGEXP,ignore.case = TRUE)]<-1000000000

    econ$newcrop <- econ$CROPDMG*econ$cropdamage

    econ$total <- econ$newprop + econ$newcrop

    totalecon <- econ %>% group_by(EVTYPE) %>% summarize(total = sum(total)/1000000000)
    totalecon <- arrange(totalecon, desc(total))

Results
=======

We answeer the first question with the follwoing bar chart.

    graphharm <- head(totalharm,10)
    ggplot(graphharm, aes(x= reorder(EVTYPE, -total), y = total)) +geom_bar(stat = "identity") +labs(x = "Storm Event Type", y = "Total Fatalities and Injuries") +theme(axis.text.x = element_text(angle = 90, hjust = 1))

![](OceanProject_files/figure-markdown_strict/unnamed-chunk-5-1.png)

We answer the second question with the follwoing bar chart.

    graphecon <- head(totalecon,10)
    ggplot(graphecon, aes(x= reorder(EVTYPE, -total), y = total)) +geom_bar(stat = "identity") +labs(x = "Storm Event Type", y = "Total Monatary Damage ($Billions)") +theme(axis.text.x = element_text(angle = 90, hjust = 1))

![](OceanProject_files/figure-markdown_strict/unnamed-chunk-6-1.png)

We conclude that tornados are the most harmful storm damage that exsists
in the United States while droughts and floods cause the most monetary
damage.
