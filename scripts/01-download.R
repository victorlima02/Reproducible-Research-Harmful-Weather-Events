library(R.utils)
library(dplyr)
library(sqldf)

dataDirectory <-'data'
dataFile <- file.path(dataDirectory,'repdata_data_StormData.csv')
compressedDataFile <- file.path(dataDirectory,'repdata_data_StormData.csv.bz2')

#Verify the data directory existence
if(!dir.exists(dataDirectory)){
	dir.create(dataDirectory)
}

download.file(url='https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',
	      destfile = compressedDataFile)

bunzip2(compressedDataFile, destname=dataFile, overwrite=TRUE )