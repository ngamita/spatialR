# Visualization of how students performed across
# different districts in Uganda Primary Leaving 
# Examinations 2013/2014. 
# Plotting the Primary leaving Exams data on a color-coded map,
# in less than 100 lines of R code. 

# Author: Richard Ngamita 'ngamita@gmail.com'


# Disclaimer: These methods here may not be the best solutions,
# but seemed the easiest for getting started with spatial data 
# in R. For any feedback: ngamita@gmail.com


# Load the rgdal library.
# If you don't have it 
# run this command: install.packages(''rgdal)
library(rgdal)

# Set working directory
# Check if there, else create one.
if(!file.exists('data')){
  dir.create('data')
}

# Set wd to data
setwd('./data')

# Download Uganda district shape files into districts.zip from the web
# Simple google search of data.ug district shapefiles will pull this up. 
download.file("http://maps.data.ug/geoserver/wfs?format_options=charset%3AUTF-8&typename=geonode%3Adistricts_2013_112_web_wgs84&outputFormat=SHAPE-ZIP&version=1.0.0&service=WFS&request=GetFeature", "districts.zip")

# Unzip file the shape file. 
unzip("districts.zip")

# Read in shape files. 
# ?readOGR() to find out more. 
districts <- readOGR(".", "districts_2013_112_web_wgs84")

# Check for loaded data quality.
# Plot districts  to ch
#plot(districts)
#slotNames(districts)
#class(districts)
#head(districts@data) 
# Note the @ sign includes the "SpatialPolygonsDataFrame" details.


## Pull in Primary Leaving Exams results
# CSV data file. A little of data.ug PLE 
# data files will pop up the link. 

# download the uneb PLE data from data.ug website. 
# Use the wget method of **nix or Mac machines should
# use the curl. Windows, don't think need any methods. 

# download the file to local directory
download.file('http://catalog.data.ug/dataset/a4a1ef8b-afa4-4b8f-b9f3-d4ef9b783eee/resource/1ba956e0-e8b5-42f8-9655-c464715ec065/download/ple.csv', destfile='ple.csv', method='wget')

# Read CSV uneb data, sep as csv and include the header/column names. 
# as.is to keep the data file types.
ple <- read.csv('ple.csv', sep=',', header=TRUE, as.is = TRUE)

# Check if loaded well
#head(ple)
#str(ple) # Make sure data types are right.

# Clean the last column, useless to us. 
ple <- ple[,1:4]

# Convert Division1 to numeric .
# SuppressWarrnings as NA or missings values are present. 
ple$Division1 <- suppressWarnings(as.numeric(ple$Division1))

# RUN: install.packages('plyr')
# incase you don't have it installed. 
library(plyr)

# We want, to dice and aggregate counts. 
# Get all sum or totals of division1s per district
ple_division1 <- aggregate(Division1 ~ District, data = ple, sum)

# Creates a dataframe with division1 totals per district, check if loaded well. 
# head(ple_division1)

# Use the match() function to append these two different dataframes into the one SpatialPolygonsDataFrame.
districts@data <- data.frame(districts@data, 
                                  ple_division1[match(districts@data[, "DNAME_2011"],
																													ple_division1[, "District"]), ])

# Remove the repeated columns, specifically "District". 
districts@data$District <- NULL

# Re-name the colname to make sense. 
colnames(districts@data)[1] <- 'Districts_2013'



# Now the shape ﬁle or SpatialPolygonsDataFrame contains our added ﬁeld called ‘Division1’ 
# which contains the count of the number of first grades 
#  We can use this to create a choropleth map with:

# First remove incomplete rows/NA values, disctricts without results.
districts@data <- na.omit(districts)

# Using Basic plot() function
# Load the mapping and color packages.
# RUN: install.packages('package name')
# If you dont have it installed. 

library(maptools) 
library(RColorBrewer) 
library(classInt) 

# select a colour palette and 
# the number of colours you wish to display
# Could be 4, 5 or many more.
colours <- brewer.pal(4, "Blues")

# we need to set breaks
# can use the classIntervals function 
# in the classInt package e just loaded.
brks<-classIntervals(districts$Division1, n=4, style="quantile")

# With plot function, lets plot the distribution 
# of the data and view the colours assigned respectively. 
plot(brks, pal=colours)

# extract brks values from the brks object above.
brks<- brks$brks

# Finally, i got a map to show you. 
plot(districts, col=colours[findInterval(districts$Division1, brks,
all.inside=TRUE)], axes=F)


# Go ahead and add title, legent, scale etc. 

# Save file locally. 
png(filename="your/file/location/name.png")
plot(out_put)
dev.off()


#Part 2:

library(GISTools)
library(RColorBrewer)

# Clear the missing values issue. 
districts@data <- districts@data[complete.cases(districts@data), ] # getting a bug. 

# Use choropleth function show performing districts. 
choropleth(districts, districts$Division1)

#  map looks fine, but lets make it better with a few extra commands.

# Set colour and number of classes
shades <- auto.shading(districts$Division1, n = 9, cols = brewer.pal(9, "Blues"))

# Draw the map
choropleth(districts, districts$Division1, shades)

# Add a legend
choro.legend(26.64793, 1.674763, shades, fmt = "%g", title = "Count of Division 1", cex = 1.0)

# Add a title to the map
title("Count of Division 1's in PLE, 2013")

# add Notth arrow
north.arrow(27.92452, 3.30194, 10)


# Working with GoogleMaps and OpenStreetMap.
libary (ggmap) 

# Further reading: check out these solutions by Antonio
# https://sites.google.com/site/rodriguezsanchezf/news/usingrasagis

