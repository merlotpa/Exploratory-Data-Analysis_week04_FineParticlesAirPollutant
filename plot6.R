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

### PLOT 5
### Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources
### in Los Angeles County, California (fips == "06037").
### Which city has seen greater changes over time in motor vehicle emissions?


####  Find PostCode of Baltimore-City to match the fips
# install.packages("maps")
library("maps")
county.fips[grep("baltimore", county.fips$polyname),]
# fips                polyname
# 1164 24005      maryland,baltimore
# 1185 24510 maryland,baltimore city

####  subset the dataframe to Baltimore-City only
postCode_BaltimoreCity <- county.fips[grep("baltimore city", county.fips$polyname),]$fips
postCode_LosAngeles <- "06037"
baltimoreCityNEI <- NEI[NEI$fips==postCode_BaltimoreCity,]
losAngelesNEI <- NEI[NEI$fips==postCode_LosAngeles,]

head(baltimoreCityNEI)
# fips      SCC Pollutant Emissions  type year
# 114288 24510 10100601  PM25-PRI     6.532 POINT 1999
# 114296 24510 10200601  PM25-PRI    78.880 POINT 1999
# 114300 24510 10200602  PM25-PRI     0.920 POINT 1999
# 114308 24510 30100699  PM25-PRI    10.376 POINT 1999
# 114325 24510 30183001  PM25-PRI    10.859 POINT 1999
# 114329 24510 30201599  PM25-PRI    83.025 POINT 1999

# Find 'vehicle' anywhere
vehicle_bool <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case = TRUE)
for (name in names(SCC)){
        vehicle_bool <- vehicle_bool | grepl("vehicle", SCC[name], ignore.case=TRUE)
}
vehicle_SCC <- SCC[vehicle_bool,]$SCC
vehicle_baltimoreCity_NEI <- baltimoreCityNEI[baltimoreCityNEI$SCC %in% vehicle_SCC,]
vehicle_baltimoreCity_NEI$city <- "Baltimore City"
vehicle_losAngeles_NEI <- losAngelesNEI[losAngelesNEI$SCC %in% vehicle_SCC,]
vehicle_losAngeles_NEI$city <- "Los Angeles"
cities_NEI <- rbind(vehicle_baltimoreCity_NEI,vehicle_losAngeles_NEI)
unique(cities_NEI$fips)
# [1] "24510" "06037"

####  as for plot1, aggregate PM2.5 emissions by year
aggByYearByCity <- aggregate(Emissions ~ year+city, cities_NEI, sum)

####  generates the plot
library(ggplot2)
g <- ggplot(aggByYearByCity, aes(x=year, y=Emissions, , color=city))
g <- g + geom_line() +
        xlab("year") +
        ylab("PM2.5 Emissions (tons)") +
        ggtitle("Total PM2.5 Emissions from motor vehicle sources in Baltimore City (MD) & LA from 1999 to 2008")
print(g)
dev.copy(png, file="plot6.png", height=480)
dev.off()



