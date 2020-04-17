#Converting from pixels to um:
#(1 in/2400 pix)(25.4 mm/1 in)(10^3um/1 mm) = 10.5833 um/pix
#see line 78 for resolution = 2400 for future definition of a variable 
#(if Resol=2400, there are 10.5833 microns/pixel)
#Values of “Area” column have to be transformed by multiplying it??
#(e.g. for 2400 resolution: Area*10.58332 (because the area is in 2 D), 
#and Major=Major*10.5833


#set working directory
setwd('/Users/juliacox/Desktop/Zooscan/Zooscan')

#Removing the header: 
#load all the lines of the text file into a string vector called allLines.
zooscandataVector <- "SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt"
conn <- file(zooscandataVector,open="r")
#make vector
allLines <-readLines(conn)
close(conn)
#Find the line number of the end of the header: 
headerEnd <- which(allLines %in% '[Data]')
print(headerEnd)

#read in the data
metadata <- read.table(file='SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt', sep = ';', header = T)
zooscandata <- read.table(file='SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt', sep = ';', header = T, row.names=1, skip = headerEnd)

#Search for and extract the pixel resolution from the header 
#Looks like the units are mm/pixel, (header contains rounded version of 10.5833 um/px)
PixelSizeLine = grep("PixSize=", allLines)
cat('line number of PixelSize = ', PixelSizeLine)
cat('raw string data at that line is: ', allLines[PixelSizeLine])
#This convertion from string to numeric type is what we want to do math on
#sub() function strips off all the character stuff before the useful number
decimalNumeric = as.numeric(sub("PixSize=|","", allLines[PixelSizeLine]))
#get numeric mm/pix value
cat(decimalNumeric)
#convert from mm/pix to um/pix 
umpix <- decimalNumeric * 1000
print(umpix)

#multiply Min and Max by 10.6 um/pix (umpix) to get size in um
#not sure how to convert Area 
zooscandata$Min.um <- zooscandata$Min * umpix
zooscandata$Max.um <- zooscandata$Max * umpix

head(zooscandata)
# view all the data:
View(zooscandata)

# plots: Area, Min (Minimum diameter/axis), Max (maximum diameter/axis)

# calculate mean area:
mean(zooscandata$Area)

par(mfrow = c(2,1)) # made 2 subplots
hist(zooscandata$Max.um, xlab = "Size (um)", main = "Maximum Axis Distribution")
hist(zooscandata$Min.um, xlab = "Size (um)", main = "Minimum Axis Distribution")

# let's try making a histogram of Max (maximum diameter)
par(mfrow = c(2,1)) # made 2 subplots
hist(zooscandata$Max[zooscandata$Max<247], breaks = seq(from=243, to=246, by=1), freq=T, main='Small things', xlab = 'Size (pixels)')
hist(zooscandata$Max[zooscandata$Max>=247], freq=T, main='Less small things', xlab = 'Size (pixels)')

# pull out the object numbers for things that are exactly 243 pixels Max axis:
tinythings<- zooscandata$Item[zooscandata$Max==243]
tinythingsrows<- zooscandata[zooscandata$Max==243,]
View(tinythingsrows)

# pull out info on things that are between 244 and 246:
othertinythings<- zooscandata$Item[zooscandata$Max>=244 & zooscandata$Max<=246]
othertinythingsrows<- zooscandata[zooscandata$Max>=244 & zooscandata$Max<=246,]

print("test")


## These are all the functions we need for processing Zooscan data
install.packages("reader")


#function for removing header
ZooscanHeaderRm <- function(filename){
  ## assuming that all files have the same number of header rows, use skip=208 to read in the data from the full file
  #Removing the header: 
  #load all the lines of the text file into a string vector called allLines.
  zooscandataVector <- "SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt"
  conn <- file(zooscandataVector,open="r")
  allLines <-readLines(conn)
  close(conn)
  #Find the line number of the end of the header: 
  headerEnd = which(allLines %in% '[Data]')
  print(headerEnd)
}
#function for reading in files
readZooscan<-function(filename){
  library(reader)
  ## assuming that all files have the same number of header rows, use skip=208 to read in the data from the full file
  #Removing the header: 
  #load all the lines of the text file into a string vector called allLines.
  zooscandataVector <- "SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt"
  conn <- file(zooscandataVector,open="r")
  allLines <-readLines(conn)
  close(conn)
  #Find the line number of the end of the header: 
  headerEnd = which(allLines %in% '[Data]')
  print(headerEnd)
  
  #read in the data
  metadata <- read.table(file='SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt', sep = ';', header = T)
  zooscandata <- read.table(file='SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt', sep = ';', header = T, row.names=1, skip = headerEnd)
  View(zooscandata)
  
  zooscandata<- read.table(filename, sep=';', header=T, skip=208) 
  
  # read in things that we need from the header:
  headerlines<- n.readLines('SampleZooscan_HB1907Moc8Net5_333_1.txt', 208)
  PixelSizeLine = grep("PixSize=", headerlines)
  
  #sub() function strips off all the character stuff before the useful number
  decimalNumeric = as.numeric(sub("PixSize=|","", allLines[PixelSizeLine]))
  umpix <- decimalNumeric * 1000
  
  #convert sizes of all columns of interest
  zooscandata$Min.um <- zooscandata$Min * umpix
  zooscandata$Max.um <- zooscandata$Max * umpix
  
  return(zooscandata)
}

#function for calculating histograms
ZooscanHeaderRm("SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt")
readZooscan("SampleZooscan_HB1907Moc8Net5_333_1withmetadata.txt")


