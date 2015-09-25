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
#table(stormdata$propdmgexp)
#table(stormdata$propdmg[stormdata$propdmgexp == "K"])

# 188,065 observations have 0 property damage yet have a damage multiplier
# Remove property damage code if damage value was zero.
#stormdata$propdmgexp[stormdata$propdmg == 0] = ""

# Do the same clean up for crop damage.
#table(stormdata$cropdmg[stormdata$cropdmgexp == "K"])
#stormdata$cropdmgexp[stormdata$cropdmg == 0] = ""



# According to the National Oceanic and Atmospheric Administration, only
# data on tornados were collected from 1950 to 1954, from 1955 to 1992 data on
# tornados, and thunderstorm wind and hail were recorded, from 1996 to present
# 48 event types were recorded as defined in the NWS Directive 10-1605. For
# the purposes of finding which types of storms caused the most fatalities and
# injuries, as well as property damage only data from 1996 to the present is
# used.
# https://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype

test <- filter(stormdata, bgn_date >= "1996-01-01") %>%
        
        # Filter only those rows where there is at least one fatality, one 
        # injury, some property damage, or some crop damage.
        filter(fatalities > 0 | injuries > 0 | propdmg > 0 | cropdmg > 0) %>%
        
        # Put damage estimates in dollar amounts based on codes provided by NWSI 10-1605
        # August 17, 2007. Then, select the variables for analysis. Then, filter only
        # those rows where there is at least one fatality, one injury, some property 
        # damage, or some crop damage.
        mutate(propdmg = ifelse(propdmgexp == "K", 1e3 * propdmg, 
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
        
        # Reformat evtype from factor to character.
        mutate(evtype = tolower(as.character(evtype))) %>% 
        
        # Arrange in proposed order of importance.
        arrange(desc(fatalities, injuries, propdmg, cropdmg))


# Create event category variable based on 48 categorical events listed in 
# Figure 2.1.1 in NWSI 10-1605 Aug. 17, 2007.
# Accessed at https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
# on 9/23/2015.
events <- c("astronomical low tide", "avalanche", "blizzard", "coastal flood", 
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


test$evtype[grep("small", test$evtype)] = events[19]
test$evtype[grep("wild", test$evtype)] = events[46]
test$evtype[grep("brush", test$evtype)] = events[46]
test$evtype[grep("hurricane", test$evtype)] = events[25]
test$evtype[grep("typhoon", test$evtype)] = events[25]
test$evtype[grep("squall", test$evtype)] = events[28]
test$evtype[grep("lake effect", test$evtype)] = events[28]
test$evtype[grep("torrential", test$evtype)] = events[21]
test$evtype[grep("unseasonal", test$evtype)] = events[21]
test$evtype[grep("urban", test$evtype)] = events[15]
test$evtype[grep("warm", test$evtype)] = events[20]
test$evtype[grep("freeze", test$evtype)] = events[17]
test$evtype[grep("surf", test$evtype)] = events[23]
test$evtype[grep("rip", test$evtype)] = events[34]
test$evtype[grep("marine tstm wind", test$evtype)] = events[33]
test$evtype[grep("mix", test$evtype)] = events[48]
test$evtype[grep("rain/snow", test$evtype)] = events[48]
test$evtype[grep("record heat", test$evtype)] = events[12]
test$evtype[grep("river", test$evtype)] = events[15]
test$evtype[grep("mud", test$evtype)] = events[6]
test$evtype[grep("gust", test$evtype)] = events[24]
test$evtype[grep("non", test$evtype)] = events[24]
test$evtype[grep("tstm wind", test$evtype)] = events[39]
test$evtype[grep("exposure", test$evtype)] = events[13]
test$evtype[grep("light ", test$evtype)] = events[48]
test$evtype[grep("unseason", test$evtype)] = events[13]
test$evtype[grep("surge", test$evtype)] = events[37]
test$evtype[grep("blowing dust", test$evtype)] = events[11]
test$evtype[grep("flash", test$evtype)] = events[14]
test$evtype[grep("extreme cold", test$evtype)] = events[13]
test$evtype[grep("erosion", test$evtype)] = events[4]
test$evtype[grep("slide", test$evtype)] = events[6]
test$evtype[grep("excessive snow", test$evtype)] = events[22]
test$evtype[grep("thunderstorm wind ", test$evtype)] = events[39]
test$evtype[grep("blowing", test$evtype)] = events[3]
test$evtype[grep("cold temperature", test$evtype)] = events[5]
test$evtype[grep("rain/snow", test$evtype)] = events[48]
test$evtype[grep("shower", test$evtype)] = events[22]
test$evtype[grep("extreme", test$evtype)] = events[13]
test$evtype[grep("road", test$evtype)] = events[48]
test$evtype[grep("coastal", test$evtype)] = events[4]
test$evtype[grep("cold", test$evtype)] = events[5]
test$evtype[grep("ice jam", test$evtype)] = events[15]
test$evtype[grep("glaze", test$evtype)] = events[48]


table(test$evtype)


table(test$evtype[grep("ice jam", test$evtype)])
test$evtype <- gsub("tstm", events[39], test$evtype)

test <- test$evtype[grep("devil", test$evtype, ignore.case = TRUE)] = events[10]

