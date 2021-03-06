setwd("C:/Program Files/R/Coursera/repdata/RepData_PeerAssessment2")
library(dplyr)
library(reshape2)
library(ggplot2)
# Download data and read in as data frame
if (!file.exists("./repdata%2Fdata%2FStormData.csv.bz2")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileUrl, "repdata%2Fdata%2FStormData.csv.bz2")
}

stormdata <- read.csv(bzfile("./repdata%2Fdata%2FStormData.csv.bz2"))

# Begin data cleaning
colnames(stormdata) <- tolower(colnames(stormdata))

# Remove time element in the bgn_date and end_date variables since it contains
# no information, convert to Date format. bgn_time and end_time have multiple
# coding formats and yield little value for analysis.
split <- colsplit(stormdata$bgn_date, " ", c("bgn_date", "trash")) 
stormdata$bgn_date <- as.Date(split$bgn_date, "%m/%d/%Y")

# 243,411 observations of end_date are blank and yield missing values.
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

stormdata <- filter(stormdata, bgn_date >= "1996-01-01") %>%
        
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
        
        # Reformat evtype and remarks from factor to character.
        mutate(evtype = tolower(as.character(evtype)), 
               remarks = as.character(remarks)) %>% 
        
        # Arrange in proposed order of importance.
        arrange(desc(fatalities, injuries, propdmg, cropdmg))


# Create event category variable based on 48 categorical events listed in 
# Figure 2.1.1 in NWSI 10-1605 Aug. 17, 2007.
# Accessed on 9/23/2015 at:
# https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf

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

# Recode evtype observations to standard codes after inspecting table(evtype)
# to identify which events were coded as something other than the 48 events
# listed by the NWSI. The latter changes required looking up the remarks for
# specific observations. In particular, the "other" observations consisted of
# mostly "dust devils" and crop flooding due to heavy rain.
stormdata$evtype[grep("small", stormdata$evtype)] = events[19]
stormdata$evtype[grep("wild", stormdata$evtype)] = events[46]
stormdata$evtype[grep("brush", stormdata$evtype)] = events[46]
stormdata$evtype[grep("hurricane", stormdata$evtype)] = events[25]
stormdata$evtype[grep("typhoon", stormdata$evtype)] = events[25]
stormdata$evtype[grep("squall", stormdata$evtype)] = events[28]
stormdata$evtype[grep("lake effect", stormdata$evtype)] = events[28]
stormdata$evtype[grep("torrential", stormdata$evtype)] = events[21]
stormdata$evtype[grep("unseasonal", stormdata$evtype)] = events[21]
stormdata$evtype[grep("urban", stormdata$evtype)] = events[15]
stormdata$evtype[grep("warm", stormdata$evtype)] = events[20]
stormdata$evtype[grep("freeze", stormdata$evtype)] = events[17]
stormdata$evtype[grep("surf", stormdata$evtype)] = events[23]
stormdata$evtype[grep("rip", stormdata$evtype)] = events[34]
stormdata$evtype[grep("marine tstm wind", stormdata$evtype)] = events[33]
stormdata$evtype[grep("mix", stormdata$evtype)] = events[48]
stormdata$evtype[grep("rain/snow", stormdata$evtype)] = events[48]
stormdata$evtype[grep("record heat", stormdata$evtype)] = events[12]
stormdata$evtype[grep("river", stormdata$evtype)] = events[15]
stormdata$evtype[grep("mud", stormdata$evtype)] = events[6]
stormdata$evtype[grep("gust", stormdata$evtype)] = events[24]
stormdata$evtype[grep("non", stormdata$evtype)] = events[24]
stormdata$evtype[grep("tstm wind", stormdata$evtype)] = events[39]
stormdata$evtype[grep("exposure", stormdata$evtype)] = events[13]
stormdata$evtype[grep("light ", stormdata$evtype)] = events[48]
stormdata$evtype[grep("unseason", stormdata$evtype)] = events[13]
stormdata$evtype[grep("surge", stormdata$evtype)] = events[37]
stormdata$evtype[grep("blowing dust", stormdata$evtype)] = events[11]
stormdata$evtype[grep("flash", stormdata$evtype)] = events[14]
stormdata$evtype[grep("extreme cold", stormdata$evtype)] = events[13]
stormdata$evtype[grep("erosion", stormdata$evtype)] = events[4]
stormdata$evtype[grep("slide", stormdata$evtype)] = events[6]
stormdata$evtype[grep("excessive snow", stormdata$evtype)] = events[22]
stormdata$evtype[grep("thunderstorm wind ", stormdata$evtype)] = events[39]
stormdata$evtype[grep("blowing", stormdata$evtype)] = events[3]
stormdata$evtype[grep("cold temperature", stormdata$evtype)] = events[5]
stormdata$evtype[grep("rain/snow", stormdata$evtype)] = events[48]
stormdata$evtype[grep("shower", stormdata$evtype)] = events[22]
stormdata$evtype[grep("extreme", stormdata$evtype)] = events[13]
stormdata$evtype[grep("road", stormdata$evtype)] = events[48]
stormdata$evtype[grep("coastal", stormdata$evtype)] = events[4]
stormdata$evtype[grep("cold", stormdata$evtype)] = events[5]
stormdata$evtype[grep("ice jam", stormdata$evtype)] = events[15]
stormdata$evtype[grep("glaze", stormdata$evtype)] = events[48]
stormdata$evtype[grep("black", stormdata$evtype)] = events[48]
stormdata$evtype[grep("freezing drizzle", stormdata$evtype)] = events[48]
stormdata$evtype[grep("falling", stormdata$evtype)] = events[48]
stormdata$evtype[grep("microburst", stormdata$evtype)] = events[38]
stormdata$evtype[grep("downburst", stormdata$evtype)] = events[38]
stormdata$evtype[grep("landslump", stormdata$evtype)] = events[6]
stormdata$evtype[grep("landspout", stormdata$evtype)] = events[40]
stormdata$evtype[grep("high water", stormdata$evtype)] = events[21]
stormdata$evtype[which(stormdata$evtype == "late season snow")] = events[48]
stormdata$evtype[which(stormdata$evtype == "fog")] = events[7]
stormdata$evtype[which(stormdata$evtype == "freezing rain")] = events[48]
stormdata$evtype[which(stormdata$evtype == "snow and ice")] = events[48]
stormdata$evtype[which(stormdata$evtype == "frost")] = events[17]
stormdata$evtype[which(stormdata$evtype == "heat wave")] = events[12]
stormdata$evtype[which(stormdata$evtype == "gradient wind")] = events[24]
stormdata$evtype[which(stormdata$evtype == "high wind (g40)")] = events[24]
stormdata$evtype[which(stormdata$evtype == "high winds")] = events[24]
stormdata$evtype[which(stormdata$evtype == "strong winds")] = events[38]
stormdata$evtype[which(stormdata$evtype == "freezing spray")] = events[38]
stormdata$evtype[which(stormdata$evtype == "wind damage")] = events[38]
stormdata$evtype[which(stormdata$evtype == "early frost")] = events[17]
stormdata$evtype[which(stormdata$evtype == "drowning")] = events[21]
stormdata$evtype[which(stormdata$evtype == "rain")] = events[21]
stormdata$evtype[which(stormdata$evtype == "winds")] = events[32]
stormdata$evtype[which(stormdata$evtype == "heavy seas")] = events[23]
stormdata$evtype[which(stormdata$evtype == "rough seas")] = events[23]
stormdata$evtype[which(stormdata$evtype == "rogue wave")] = events[23]
stormdata$evtype[which(stormdata$evtype == "high seas")] = events[23]
stormdata$evtype[which(stormdata$evtype == "marine accident")] = events[23]
stormdata$evtype[which(stormdata$evtype == "astronomical high tide")] = events[23]
stormdata$evtype[which(stormdata$evtype == "wind and wave")] = events[23]
stormdata$evtype[which(stormdata$evtype == "high swells")] = events[23]
stormdata$evtype[which(stormdata$evtype == "dam break")] = events[14]
stormdata$evtype[which(stormdata$evtype == "tidal flooding")] = events[15]
stormdata$evtype[which(stormdata$evtype == "wind")] = events[24]
stormdata$evtype[which(stormdata$evtype == "snow")] = events[22]
stormdata$evtype[2500] = events[48]
stormdata$evtype[19694] = events[39]
stormdata$evtype[1335] = events[10]
stormdata$evtype[9541] = events[10]
stormdata$evtype[19677] = events[10]
stormdata$evtype[70686] = events[32]
stormdata$evtype[20454:20483] = events[21]
stormdata$evtype[54842] = events[10]
stormdata$evtype[65845] = events[10]
stormdata$evtype[5481] = events[2]

# Create binary bins for fatalities, injuries, property damage, and crop damage
stormdata <- mutate(stormdata, lethal = ifelse(fatalities > 0, 1, 0), 
                    hurtful = ifelse(injuries > 0, 1, 0), 
                    damaging = ifelse(propdmg > 0, 1, 0),
                    withering = ifelse(cropdmg > 0, 1, 0))

# Begin analysis
storm_summary <- group_by(stormdata, evtype) %>%
        # Identify the sum of metrics over time as well as share of incidents as
        # provided by the binary indicators
        summarise(total_fatalities = sum(fatalities),
                  total_injuries = sum(injuries),
                  total_propdmg = sum(propdmg),
                  total_cropdmg = sum(cropdmg),
                  share_lethal = 100 * round(mean(lethal),2),
                  share_hurtful = 100 * round(mean(hurtful),2),
                  mean_propdmg = 100 * round(mean(propdmg),2),
                  mean_cropdmg = 100 * round(mean(cropdmg),2)) %>%
        
        arrange(desc(total_fatalities, total_injuries, 
                     total_propdmg, total_cropdmg))

# Create bar chart of total fatalities by event type, using ggplot2
graphset <- head(storm_summary, 10)
plot1 <- ggplot(graphset, aes(x = reorder(evtype, total_fatalities), y = total_fatalities)) + 
        geom_bar(fill = "#CC0000", stat = "identity") + 
        geom_text(aes(label = total_fatalities), size = 6, hjust = -.5, fontface = "bold") +
        labs(title = "Total Number of Fatalities Since January 1996, by Disaster") +
        coord_flip() +
        labs(x = expression("Disaster")) + 
        labs(y = expression("Death Toll")) +
        theme(title = element_text(size = 16, face = "bold"),
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 16, face = "bold")) +
        scale_y_continuous(limits = c(0,2000))
print(plot1)

# Chart the total number of injuries for each type of disaster over time
plot2 <- ggplot(graphset, aes(x = reorder(evtype, total_injuries), y = total_injuries)) + 
        geom_bar(fill = "#FF6600", stat = "identity") + 
        geom_text(aes(label = total_injuries), size = 6, fontface = "bold", hjust = -0.5) +
        labs(title = "Total Number of Injuries Since January 1996, by Disaster") +
        coord_flip() +
        labs(x = expression("Disaster")) + 
        labs(y = expression("Injury Toll")) +
        theme(title = element_text(size = 16, face = "bold"),
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 14, face = "bold")) +
        scale_y_continuous(limits = c(0,22000))
print(plot2)

graphset <- head(arrange(storm_summary, desc(share_lethal, share_hurtful)), 10)

# Create bar chart which shows how certain disastors are more likely to cause
# at least some loss of life
plot3 <- ggplot(graphset, aes(x = reorder(evtype, share_lethal), y = share_lethal)) + 
        geom_bar(fill = "#990000", stat = "identity") + 
        geom_text(aes(label = share_lethal), size = 6, fontface = "bold", hjust = -.5) +
        labs(title = "Share of Lethal Instances Since January 1996, by Disaster") +
        coord_flip() +
        labs(x = expression("Disaster")) + 
        labs(y = expression("Percent")) +
        theme(title = element_text(size = 16, face = "bold"),
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 14, face = "bold")) +
        scale_y_continuous(limits = c(0,90))
print(plot3)

graphset <- head(arrange(storm_summary, desc(total_propdmg + total_cropdmg)), 10)

plot4 <- ggplot(graphset, aes(x = reorder(evtype, total_propdmg + total_cropdmg), 
                              y = (total_propdmg + total_cropdmg)/1000000000)) +
        geom_bar(fill = "#006653", stat = "identity") + 
        geom_text(aes(label = round(((total_propdmg + total_cropdmg)/1000000000),1)), 
                  size = 6, hjust = -.5, fontface = "bold") +
        labs(title = "Total Amount of Economic Damage Since January 1996, by Disaster") +
        coord_flip() +
        labs(x = expression("Disaster")) + 
        labs(y = expression("Nominal Monetary Damage ($ Billions)")) +
        theme(title = element_text(size = 16, face = "bold"),
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 16, face = "bold")) +
        scale_y_continuous(limits = c(0,170))
print(plot4)

graphset <- head(arrange(storm_summary, desc(total_propdmg + total_cropdmg)), 10)

dat <- select(graphset, c(evtype, total_propdmg, total_cropdmg))

graphset$total = rowSums(graphset[,4:5])

# Calculate the share of overall drought damage consisting of damage to crops 
droughtcropdmg <- 100 * graphset$total_cropdmg[7] / graphset$total[7]

graphset <-  select(graphset, c(total)) %>%
        arrange(desc(total))

dat <- melt(dat, id = "evtype")
dat <- cbind(dat, rep(graphset,1))
dat[11:20,4] = NA

# Create a bar chart which shows the total amount of monetary damage caused by
# the top 10 worst disasters (in terms of total monetary damage). Stack charts
# to show contribution of crop damage to the total.
plot5 <- ggplot(dat, aes(x = reorder(evtype, value), y = value/1000000000, fill = variable)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round((total/1000000000),1)),
                  size = 6, hjust = -.5, fontface = "bold") +
        labs(title = "Total Amount of Economic Damage Since January 1996, by Disaster") +
        coord_flip() +
        labs(x = expression("Disaster")) + 
        labs(y = expression("Nominal Monetary Damage ($ Billions)")) +
        scale_fill_manual(values= c("#CC0000", "#FF6600"),
                          name="Type of Damage",
                            breaks=c("total_propdmg", "total_cropdmg"),
                            labels=c("Property", "Crops")) +
        theme(title = element_text(size = 16, face = "bold"),
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 16, face = "bold"),
              legend.title = element_text(size = 12, face = "bold"),
              legend.text = element_text(size = 12)) +
        scale_color_discrete(name = "Type of Damage") +
        scale_y_continuous(limits = c(0,170))
suppressWarnings(print(plot5))

droughtcropdmg