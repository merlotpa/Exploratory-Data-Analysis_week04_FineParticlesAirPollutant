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

### PLOT 4
### Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# coal combustion-related sources
names(SCC)
# [1] "SCC"                 "Data.Category"       "Short.Name"          "EI.Sector"           "Option.Group"       
# [6] "Option.Set"          "SCC.Level.One"       "SCC.Level.Two"       "SCC.Level.Three"     "SCC.Level.Four"     
# [11] "Map.To"              "Last.Inventory.Year" "Created_Date"        "Revised_Date"        "Usage.Notes"    

# Find coal anywhere 
coal_Short.Name <- grepl("coal", SCC$Short.Name, ignore.case=TRUE)
coal_EI.Sector  <- grepl("coal", SCC$EI.Sector, ignore.case=TRUE)
coal_SCC.Level.One <- grepl("coal", SCC$SCC.Level.One, ignore.case=TRUE)
coal_SCC.Level.Two <- grepl("coal", SCC$SCC.Level.Two, ignore.case=TRUE)
coal_SCC.Level.Three <- grepl("coal", SCC$SCC.Level.Three, ignore.case=TRUE)
coal_SCC.Level.Four <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE)

coal_bool <- coal_Short.Name | coal_EI.Sector | coal_SCC.Level.One | coal_SCC.Level.Two | coal_SCC.Level.Three | coal_SCC.Level.Four
coal_SCC <- SCC[coal_bool,]$SCC
coal_NEI <- NEI[NEI$SCC %in% coal_SCC,]

####  as for plot1, aggregate PM2.5 emissions by year
aggByYear <- aggregate(Emissions ~ year, coal_NEI, sum)


####  generates the plot
library(ggplot2)

g <- ggplot(aggByYear, aes(factor(year), Emissions/10^6))
g <- g + geom_bar(stat="identity") + xlab("year") + ylab("PM2.5 Emissions (million tons)") + 
        ggtitle('Total Emissions from coal sources in the USA from 1999 to 2008')
print(g)

dev.copy(png, file="plot4.png", height=480)
dev.off()

