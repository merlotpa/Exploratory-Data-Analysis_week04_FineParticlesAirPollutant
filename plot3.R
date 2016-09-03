# Peer Graded Assignment: Exploratory Data Analysis Course Project 2 (week 4)
# https://github.com/merlotpa/Exploratory-Data-Analysis_week04_FineParticlesAirPollutant

## READ THE FILES
### This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
        NEI <- readRDS(paste(rawDir, "summarySCC_PM25.rds", sep = "/"))
}
if(!exists("SCC")){
        SCC <- readRDS(paste(rawDir, "Source_Classification_Code.rds", sep = "/"))
}

### PLOT 3
### Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
### which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
### Which have seen increases in emissions from 1999–2008?
### Use the ggplot2 plotting system to make a plot answer this question.

####  Find PostCode of Baltimore-City to match the fips
install.packages("maps")
library("maps")
county.fips[grep("baltimore", county.fips$polyname),]
# fips                polyname
# 1164 24005      maryland,baltimore
# 1185 24510 maryland,baltimore city

####  subset the dataframe to Baltimore-City only
postCode <- county.fips[grep("baltimore city", county.fips$polyname),]$fips
baltimoreCityNEI <- NEI[NEI$fips==postCode,]

head(baltimoreCityNEI)
# fips      SCC Pollutant Emissions  type year
# 114288 24510 10100601  PM25-PRI     6.532 POINT 1999
# 114296 24510 10200601  PM25-PRI    78.880 POINT 1999
# 114300 24510 10200602  PM25-PRI     0.920 POINT 1999
# 114308 24510 30100699  PM25-PRI    10.376 POINT 1999
# 114325 24510 30183001  PM25-PRI    10.859 POINT 1999
# 114329 24510 30201599  PM25-PRI    83.025 POINT 1999

unique(baltimoreCityNEI$type)
# [1] "POINT"    "NONPOINT" "ON-ROAD"  "NON-ROAD"

####  as for plot1, aggregate PM2.5 emissions by year
aggByYearByType <- aggregate(Emissions ~ year + type, baltimoreCityNEI, sum)


####  generates the plot
library(ggplot2)
g <- ggplot(aggByYearByType, aes(year, Emissions, color = type))
g <- g + geom_line() +
        xlab("year") +
        ylab("PM2.5 Emissions (tons)") +
        ggtitle("Total PM2.5 Emissions per type in Baltimore City (MD) from 1999 to 2008")
print(g)

dev.copy(png, file="plot3.png", height=480)
dev.off()
