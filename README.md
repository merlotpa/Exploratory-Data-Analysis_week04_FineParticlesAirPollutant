# Peer Graded Assignment: Exploratory Data Analysis of **"Air Pollution Case Study"** (Course Project 2, week 4)

This assignment is the final project of the course on COURSERA named ["Exploratory Data Analysis"](https://www.coursera.org/learn/exploratory-data-analysis)

The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.

## COURSERA instructions

> Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health.
> In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of this pollutant into the atmosphere.
> Approximatly every 3 years, the EPA releases its database on emissions of PM2.5.
> This database is known as the National Emissions Inventory (NEI). You can read more information about the NEI at the [EPA National Emissions Inventory web site](http://www.epa.gov/ttn/chief/eiinformation.html).
> 
> For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year.
> The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.
> 
> 
> ### Review criteria
> For each question
> 
> 1. Does the plot appear to address the question being asked?
> 2. Does the plot appear to address the question being asked?
> 
> ### Data
> The data for this assignment are available from the course web site as a single zip file:
> 
> [Data for Peer Assessment [29Mb]](https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip)
> 
> The zip file contains two files:
> 
> PM2.5 Emissions Data (**summarySCC_PM25.rds**): This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of *tons* of PM2.5 emitted from a specific type of source for the entire year. Here are the first few rows.
> 
> 
> ```R
> ##     fips      SCC Pollutant Emissions  type year
> ## 4  09001 10100401  PM25-PRI    15.714 POINT 1999
> ## 8  09001 10100404  PM25-PRI   234.178 POINT 1999
> ## 12 09001 10100501  PM25-PRI     0.128 POINT 1999
> ## 16 09001 10200401  PM25-PRI     2.036 POINT 1999
> ## 20 09001 10200504  PM25-PRI     0.388 POINT 1999
> ## 24 09001 10200602  PM25-PRI     1.490 POINT 1999
> ```
> 
> * *fips*: A five-digit number (represented as a string) indicating the U.S. county
> * *SCC*: The name of the source as indicated by a digit string (see source code classification table)
> * *Pollutant*: A string indicating the pollutant
> * *Emissions*: Amount of PM2.5 emitted, in tons
> * *type*: The type of source (point, non-point, on-road, or non-road)
> * *year*: The year of emissions recorded
> 
> Source Classification Code Table (*Source_Classification_Code.rds*): This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.
> 
> 
> You can read each of the two files using the *readRDS()* function in R. For example, reading in each file can be done with the following code:
> 
> 
> ```R
> ## This first line will likely take a few seconds. Be patient!
> NEI <- readRDS("summarySCC_PM25.rds")
> SCC <- readRDS("Source_Classification_Code.rds")
> ```
> 
> as long as each of those files is in your current working directory (check by calling *dir()* and see if those files are in the listing).
> 
> 
> ### Assignment
> The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.
> 
> ### Questions
> 
> You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.
> 
> Q1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

![plot1.png](plot1.png "Plot1")

**We see from the plot above that the total emission of PM2.5 accross the USA has gradually decreased from 7 millions tons in 1999 to 52% less in 2008.**

> Q2. Have total emissions from PM2.5 decreased in the *Baltimore City*, Maryland (*fips == "24510"*) from 1999 to 2008? Use the base plotting system to make a plot answering this question.

**Looking at the total emission of PM2.5 in Baltimore City during the same period, we see that it had also dramatically decreased between 1999 and 2008 by more than 40%, despite an increase from 2002 to 2005.**

![plot2.png](./plot2.png?raw=true "Plot2")

> Q3. Of the four types of sources indicated by the *type* (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for *Baltimore City*? Which have seen increases in emissions from 1999–2008? Use the *ggplot2* plotting system to make a plot answer this question.

![plot3.png](./plot3.png?raw=true "Plot3")

**All source types except the *point* source have seen their emissions of PM2.5 decrease from 1999 to 2008 in Baltimore City.
Only the point source increased slightly from 1999 to 2008, but reached its highest peak in 2005.**


> Q4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

![plot4.png](./plot4.png?raw=true "Plot4")

**The emissions of PM2.5 from coal combustion diminished in parallel of the overal emission of PM2.5 during that period. It represented 8% of the total emission of PM2.5 in 1999, and 10% in 2008.**

> Q5. How have emissions from motor vehicle sources changed from 1999–2008 in *Baltimore City*?

![plot5.png](./plot5.png?raw=true "Plot5")

**The emissions of PM2.5 in Baltimore City due to motor vehicle sources dimished drastically from 400 tons in 1999 to 140 tons in 2008. By comparing to the total emission of PM2.5 in Baltimore City (Question 2) where it showed a net decrease of 1400 tons of PM2.5, we see that the motor vehicles represented around 20% of this fall between 1999 and 2008.**

> Q6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in *Los Angeles County*, California (*fips == "06037"*). Which city has seen greater changes over time in motor vehicle emissions?

![plot6.png](./plot6.png?raw=true "Plot6")

**The difference of emission of PM2.5 between LA and Baltimore City is striking. Los Angeles emitted 15 times more PM2.5 than Baltimore in 1999, and increased slightly from 1999 to 2008, while Baltimore City managed to decrease their emission, so that LA emitted more than 45 times particles than Baltimore City in 2008.**

> 
> ### Making and Submitting Plots
> For each plot you should
> 
> * Construct the plot and save it to a PNG file.
> * Create a separate R code file (*plot1.R*, *plot2.R*, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot.
>     Your code file should include code for reading the data so that the plot can be fully reproduced. You must also include the code that creates the PNG file.
>     Only include the code for a single plot (i.e. *plot1.R* should only include code for producing *plot1.png*)
> * Upload the PNG file on the Assignment submission page
> * Copy and paste the R code from the corresponding R file into the text box at the appropriate point in the peer assessment.



## Content of this repository

### The raw data

The [raw data](https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip) is already saved locally in this repository for reproducability purpose ([https://github.com/merlotpa/Exploratory-Data-Analysis_week04_FineParticlesAirPollutant/blob/master/rawData/exdata_data_NEI_data.zip?raw=true]()).