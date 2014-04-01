# Visualization of how students performed per District
# in Uganda Primary Leaving Examinations 2013. 
# plotting the Primary leaving Exams data on a color-coded map,
# in less than 100 lines of R code. 

# Author: Richard Ngamita 'ngamita@gmail.com'


library(rgdal)

# Set working directory
setwd("~/refunite/code2014/uganda/")

# Download Uganda district shape files into districts.zip from the web
download.file("http://maps.data.ug/geoserver/wfs?format_options=charset%3AUTF-8&typename=geonode%3Adistricts_2013_112_web_wgs84&outputFormat=SHAPE-ZIP&version=1.0.0&service=WFS&request=GetFeature", "districts.zip")
# Unzip file
unzip("districts.zip")

# Read in shape file
districts_data <- readOGR(".", "districts_2013_112_web_wgs84")

# Plot districts_data to check it has been imported correctly
#plot(districts_data)
#slotNames(districts_data)
#class(districts_data)
#head(districts_data@data) # Note the @ sign includes the "SpatialPolygonsDataFrame" details.


##  Pull in UNEB results per district. ##

# download the uneb PLE data from data.ug website. Use the wget method of **nix or Mac machines. 
download.file('http://catalog.data.ug/dataset/a4a1ef8b-afa4-4b8f-b9f3-d4ef9b783eee/resource/1ba956e0-e8b5-42f8-9655-c464715ec065/download/ple.csv', destfile='ple.csv', method='wget')
# Read CSV uneb data, sep as csv and include the header/column names. 
uneb_data <- read.csv('ple.csv', sep=',', header=TRUE, as.is = TRUE)

# Check if loaded well
head(uneb_data)
str(uneb_data) # Make sure data types are right. 

# Convert Division1 to numeric .
# SuppressWarrnings as NA or missings values are present. 
uneb_data$Division1 <- suppressWarnings(as.numeric(uneb_data$Division1))

library(plyr)

# Get all sum or totals of division1s per district
uneb_data_division1 <- aggregate(Division1 ~ District, data = uneb_data, sum)

# Creates a dataframe with division1 totals per district, checl if loaded well. 
#head(uneb_data_division1)

# Use the match() function to append these two different dataframes into the one SpatialPolygonsDataFrame.
districts_data@data <- data.frame(districts_data@data, uneb_data_division1[match(districts_data@data[, "DNAME_2011"],
																													uneb_data_division1[, "District"]), ])

# Remove the repeated columns, specifically "District". 
districts_data@data$District <- NULL

# Re-name the colname to make sense. 
colnames(districts_data@data)[1] <- 'Districts_2011'

library(GISTools)
library(RColorBrewer)

# Now the shape ﬁle or SpatialPolygonsDataFrame contains our added ﬁeld called ‘Division1’ 
# which contains the count of the number of first grades 
#  We can use this to create a choropleth map with:

# First remove incomplete rows/NA values, disctricts without results.
districts_data@data <- districts_data@data[complete.cases(districts_data@data), ] # getting a bug. 
districts_data@data <- na.omit(districts_data)


# Use choropleth function show performing districts. 
choropleth(districts_data, districts_data$Division1)

#  map looks fine, but lets make it better with a few extra commands.

# Set colour and number of classes
shades <- auto.shading(districts_data$Division1, n = 9, cols = brewer.pal(9, "Blues"))

# Draw the map
choropleth(districts_data, districts_data$Division1, shades)

# Add a legend
choro.legend(26.64793, 1.674763, shades, fmt = "%g", title = "Count of Division 1", cex = 1.0)

# Add a title to the map
title("Count of Division 1's in PLE, 2013")

# add Notth arrow
north.arrow(27.92452, 3.30194, 10)



