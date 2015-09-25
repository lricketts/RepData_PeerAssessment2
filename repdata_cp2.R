setwd("C:/Program Files/R/Coursera/repdata/RepData_PeerAssessment2")
library(dplyr)
library(reshape2)
# Download data and read in as data frame
if (!file.exists("./repdata%2Fdata%2FStormData.csv.bz2")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileUrl, "repdata%2Fdata%2FStormData.csv.bz2")
}

stormdata <- read.csv(bzfile("./repdata%2Fdata%2FStormData.csv.bz2"))
# Clean up data set
colnames(stormdata) <- tolower(colnames(stormdata))

#Remove time element in the bgn_date and end_date variables since it contains
#no information, convert to Date format. bgn_time and end_time have multiple
#coding formats and yield little value for analysis.
split <- colsplit(stormdata$bgn_date, " ", c("bgn_date", "trash")) 
stormdata$bgn_date <- as.Date(split$bgn_date, "%m/%d/%Y")

#243,411 observations of end_date are blank and yield missing values.
split <- colsplit(stormdata$end_date, " ", c("end_date", "trash")) 
stormdata$end_date <- as.Date(split$end_date, "%m/%d/%Y")

# According to the National Oceanic and Atmospheric Administration, only
# data on tornados were collected from 1950 to 1954, from 1955 to 1992 data on
# tornados, and thunderstorm wind and hail were recorded, from 1996 to present
# 48 event types were recorded as defined in the NWS Directive 10-1605. For
# the purposes of finding which types of storms caused the most fatalities and
# injuries, as well as property damage only data from 1996 to the present is
# used.
# https://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
stormdata <- filter(stormdata, bgn_date >= "1996-01-01")
cache <- stormdata
# Identify property damage codes
table(stormdata$propdmgexp)
table(stormdata$propdmg[stormdata$propdmgexp == "K"])

# 188,065 observations have 0 property damage yet have a damage multiplier
# Remove property damage code if damage value was zero.
stormdata$propdmgexp[stormdata$propdmg == 0] = ""

# Do the same clean up for crop damage.
table(stormdata$cropdmg[stormdata$cropdmgexp == "K"])
stormdata$cropdmgexp[stormdata$cropdmg == 0] = ""

# Put damage estimates in dollar amounts based on codes provided by NWSI 10-1605
# August 17, 2007. Then, select the variables for analysis. Then, filter only
# those rows where there is at least one fatality, one injury, some property 
# damage, or some crop damage. Then, change

test <- mutate(stormdata, 
               propdmg = ifelse(propdmgexp == "K", 1e3 * propdmg, 
                                ifelse(propdmgexp == "M", 1e6 * propdmg, 
                                       ifelse(propdmgexp == "B", 1e9 * propdmg, 
                                              propdmg))),
               cropdmg = ifelse(cropdmgexp == "K", 1e3 * cropdmg, 
                                ifelse(cropdmgexp == "M", 1e6 * cropdmg, 
                                       ifelse(cropdmgexp == "B", 1e9 * cropdmg, 
                                              cropdmg)))) %>% 
        # Select variables for analysis
        select(c(bgn_date, end_date, fatalities, evtype, state, injuries, 
                 propdmg, cropdmg, remarks)) %>%
        # Filter only those rows where there is at least one fatality, one 
        # injury, some property damage, or some crop damage.
        filter(fatalities > 0 | injuries > 0 | propdmg > 0 | cropdmg > 0) %>%
        # Reformate evtype from factor to character.
        mutate(evtype = as.character(evtype)) %>% 
        # Arrange in proposed order of importance.
        arrange(desc(fatalities, injuries, propdmg, cropdmg))
)
#%>%
        #mutate(evtype = as.factor(tolower(evtype)))

# Create event category variable based on 48 categeroical events listed in 
# Figure 2.1.1 in NWSI 10-1605 Aug. 17, 2007.
# Accessed at https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
# on 9/23/2015.
Events <- c("astronomical low tide", "avalanche", "blizzard", "coastal flood", 
            "cold/Wind chill", "debris flow", "dense fog", "dense smoke", 
            "drought", "dust devil", "dust storm", "excessive heat", 
            "extreme cold/wind chill", "flash flood", "flood", "freezing fog", 
            "frost/freeze", "funnel cloud", "hail", "heat", "heavy rain", 
            "heavy snow", "high surf", "high wind", "hurricane/typhoon", 
            "ice storm", "lakeshore flood", "lake-effect snow", "lightning", 
            "marine hail", "marine high wind", "marine strong wind", 
            "marine thunderstorm wind", "rip current", "seiche", "sleet", 
            "storm surge/tide", "strong wind", "thunderstorm wind", "tornado", 
            "tropical depression", "tropical storm", "tsunami", "volcanic ash", 
            "waterspout", "wildfire", "winter storm", "winter weather")

test <- mutate(test,
               evtype[gsub("devil", Events[10], evtype, ignore.case = TRUE)])

test <- test$evtype[grep("devil", test$evtype, ignore.case = TRUE)] = Events[10]

#GitHub please accept my new changes!