install.packages("readxl")


library(tidyr)
maxtemp <- "maxt"
averagetemp<-"avgt"
mintemp<-"mint"
maxsalinity<-"maxs"
averagesalinity<-"avgs"


#merge moc_all and salinity data
moc_all_data<-merge(moc_all,
      Salinity_Data_07_22_20,by="Label")

View(moc_all_data)

# Simple Scatterplot
attach(Salinity_Data_07_22_20)
plot(avgt, avgs, main="Salinity v Temperature", 
     xlab="Average Temperature (degrees C)", ylab="Average Salinity (PSU)", pch=19)


readZooscan2<-function(filename){
  library(reader)
  conn <- file(moc_all, open = r)
  #load all the lines of the text file into a string vector called allLines
  allLines <- readLines(conn)
  #close connection 
  close(conn)
  #Find the line number of the end of the header, and mark it as headerEnd 
  headerEnd <- which(allLines %in% '[Data]')
  print(headerEnd)
  
  #Search for and extract the pixel resolution from the header 
  #Looks like the units are mm/pixel, (header contains rounded version of 10.5833 um/px)
  #grep searches for the line number "PixSize=" in allLines vector string 
  PixelSizeLine <- grep("PixSize=", allLines)
  #print the line numner where pixel size is given in mm 
  cat('line number of PixelSize = ', PixelSizeLine)
  #print the contents of the PixelSizeLine in allLines
  cat('raw string data at that line is: ', allLines[PixelSizeLine])
  #This convertion from string to numeric type is what we want to do math on
  #sub() function strips off all the character stuff before the useful number
  #and replaces it with nothing (shown by ""), decimalNumericPix is the value we 
  #use for unit convertions
  decimalNumericPix <- as.numeric(sub("PixSize=|","", allLines[PixelSizeLine]))
  #get numeric mm/pix value
  cat(decimalNumericPix)
  
  #read in data table removing everything before headerEnd
  zooscandata <- read.table(file=filename, sep = ';', header = T, row.names=1, skip = headerEnd)
  
  #convert from mm/pix to um/pix
  umpix <- decimalNumericPix * 1000
  print(umpix)
  
  #multiply Min and Max by 10.6 um/pix (umpix) to get size in um
  #not sure how to convert Area 
  zooscandata$Minor.um <- zooscandata$Minor * umpix
  zooscandata$Major.um <- zooscandata$Major * umpix
  zooscandata$Area.um2 <- zooscandata$Area * (umpix^2)
  
  #Search for and extract the volume of water filtered (m^3) 
  VolLine <- grep("Vol=", allLines)
  #print the line number where volume filtered is given in m^3 
  cat('line number of Vol Filtered = ', VolLine)
  #print the contents of the VolLine in allLines
  cat('raw string data at that line is: ', allLines[VolLine])
  #This convertion from string to numeric type is what we want to do math on
  decimalNumericVol <- as.numeric(sub("Vol=|","", allLines[VolLine]))
  #get numeric Vol value
  cat(decimalNumericVol)
  
  #Search for and extract the split done directly before scanning 
  SplitLine <- grep("SubPart=", allLines)
  #print the line number where split fraction (1=1/1, 2=1/2, 4=1/4, etc.)
  cat('line number of Split = ', SplitLine)
  #print the contents of the SplitLine in allLines
  cat('raw string data at that line is: ', allLines[SplitLine])
  #This convertion from string to numeric type is what we want to do math on
  #For some reason, this is printing the number 2X i.e. 1 would print as
  #11 and 2 would print as 22. The math works correctly (11*5 = 5, 22*5 = 10) 
  #but this may be an issue later. This is probably just an error with printing, 
  #and not with the actual numeric value 
  decimalNumericSplit <- as.numeric(sub("SubPart=|","", allLines[SplitLine]))
  #get numeric value
  cat(decimalNumericSplit)
  
  #This part is commented out because it isn't needed unless the initial split is 
  #something other than 1/4.
  #The initial split was accidentally entered as "tot" rather than 1/4 for these samples, 
  #so I hardcoded it to multiply by 4 on line "calculate zooplankton area accounting for
  #1/4 split". Uncomment if this isn't the case:
  #InitSplitLine <- grep("FracId=", allLines)
  #print the line number where initial split fraction is (1=1/1, 2=1/2, 4=1/4, etc.):
  #cat('line number of Init. Split = ', InitSplitLine)
  #print the contents of the InitSplitLine in allLines:
  #cat('raw string data at that line is: ', allLines[InitSplitLine])
  #convert from string to numeric: 
  #decimalNumericInitSplit <- as.numeric(sub("FracId=|","", allLines[InitSplitLine]))
  #get numeric value
  #cat(decimalNumericInitSplit)
  #if you're using the above code, you should replace "zooscandata$Area.um2x4 <- zooscandata$Area.um2 * 4" with: 
  #zooscandata$Area.um2xInitSplit <- zooscandata$Area.um2 * decimalNumericInitSplit
  #also replace "zooscandata$Area.um2x4 * (decimalNumericSplit/2)*100)/decimalNumericVol" with:
  #zooscandata$Area.um2xInitSplit * (decimalNumericSplit/2)*100)/decimalNumericVol
  
  #calculate zooplankton area accounting for 1/4 split  
  zooscandata$Area.um2x4 <- zooscandata$Area.um2 * 4
  #calculate zooplankton area per 100m^3, accounting for 1/4 split and additional splits 
  zooscandata$Area.um2per100m3 <- 
    (zooscandata$Area.um2x4 * decimalNumericSplit * 100)/decimalNumericVol
  
  #Add two columns showing max and min depth of net tow 
  #Search for and extract the two depths (m)
  MaxDepthLine <- grep("Zmax=", allLines)
  MinDepthLine <- grep("Zmin=", allLines)
  #print the line numbers where depths are
  cat('line number of Max Depth = ', MaxDepthLine)
  cat('line number of Min Depth = ', MinDepthLine)
  #print the contents of the objects in allLines
  cat('raw string data at that line is: ', allLines[MaxDepthLine])
  cat('raw string data at that line is: ', allLines[MinDepthLine])
  #Convert to decimal numeric 
  decimalNumericMaxDepth <- as.numeric(sub("Zmax=|","", allLines[MaxDepthLine]))
  decimalNumericMinDepth <- as.numeric(sub("Zmin=|","", allLines[MinDepthLine]))
  #get numeric value
  cat(decimalNumericMaxDepth)
  cat(decimalNumericMinDepth)
  
  zooscandata$Max_Depth <- decimalNumericMaxDepth
  zooscandata$Min_Depth <- decimalNumericMinDepth
  
  return(zooscandata)
}

readZooscan2(moc_all)
#######


#make sure you have ggplot for any of these following plots
library(ggplot2)


# 3D Scatterplot of temp, area, vs salinity #not super useful, better two seperate 2D graphs
install.packages("scatterplot3d")
library(scatterplot3d)
attach(moc_all_data)
scatterplot3d(x=avgt,z=Area_um2,y=avgs, main="3D Scatterplot")

#####bar plot of density by salinity
#first need to merge salinity and mocness data
moc_8_total_data <- merge(data.frame(Salinity_Data_07_22_20, row.names=NULL), data.frame(moc_8, row.names=NULL), 
      by = 0, all = TRUE)[-1]
#moc_8 from stacked bar plot vs depth file

#cut() to create another column based on range of Density values
moc_8$density = cut(moc_8$Area.um2per100m3, c(100, 1000, 1500, 2000))

moc_8$density_char <- as.character(moc_8$density)


#graph salinity bar plot with scale color gradient
salinityplot<-ggplot(moc_8_total_data, aes(fill=density_char, y=Area.um2per100m3, x = avgs)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Salinity vs Density of Species for Moc 8")+ 
  labs(x="Average Salinities", y= "Density (Area.um2per100m3)")+coord_flip()+
  scale_color_gradient(low='skyblue', high='royalblue')
salinityplot

#graph temperature bar plot with scale color gradient
tempplot<-ggplot(moc_8_total_data, aes(fill=density_char, y=Area.um2per100m3, x = avgt)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Salinity vs Density of Species for Moc 8")+ 
  labs(x="Average Temperature", y= "Density (Area.um2per100m3)")+coord_flip()+
  scale_color_gradient(low='skyblue', high='royalblue')
salinityplot

#to see color options
colors()
