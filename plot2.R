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

### PLOT 2
### Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
### Use the base plotting system to make a plot answering this question.

####  Find PostCode of Baltimore-City to match the fips
# install.packages("maps")
library("maps")
county.fips[grep("baltimore", county.fips$polyname),]
# fips                polyname
# 1164 24005      maryland,baltimore
# 1185 24510 maryland,baltimore city

####  subset the dataframe to Baltimore-City only
postCode <- county.fips[grep("baltimore city", county.fips$polyname),]$fips
baltimoreCityNEI <- NEI[NEI$fips==postCode,]

####  as for plot1, aggregate PM2.5 emissions by year
aggByYear <- aggregate(Emissions ~ year, baltimoreCityNEI, sum)

####  generates the plot
par(mfrow = c(1,1))
barplot(height=aggByYear$Emissions,
        names.arg=aggByYear$year,
        xlab = "Year",
        ylab = "PM2.5 Emissions (tons)",
        main = "Total PM2.5 Emissions in Baltimore City (MD) from 1999 to 2008")
dev.copy(png, file="plot2.png", height=480)
dev.off()
