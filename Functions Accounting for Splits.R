#function for reading in >5000um and 1000-5000um files and converting pixels to micrometers (um)
readZooscan<-function(filename){
  library(reader)
  #Removing the header: 
  #open file for reading in text mode 
  conn <- file(filename,open="r")
  #load all the lines of the text file into a string vector called allLines
  allLines <-readLines(conn)
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
  #sub() function strips off all the character stuff before the useful number
  #and replaces it with nothing (shown by ""), decimalNumericVol is the value we 
  #use for unit convertions
  decimalNumericVol <- as.numeric(sub("Vol=|","", allLines[VolLine]))
  #get numeric Vol value
  cat(decimalNumericVol)
  
  #Search for and extract the split done directly before scanning 
  SplitLine <- grep("SubPart=", allLines)
  #print the line number where split fraction (1=1/1, 2=1/2, 4=1/4, etc.)
  cat('line number of Split = ', SplitLine)
  #print the contents of the VolLine in allLines
  cat('raw string data at that line is: ', allLines[SplitLine])
  #This convertion from string to numeric type is what we want to do math on
  #sub() function strips off all the character stuff before the useful number
  #and replaces it with nothing (shown by ""), decimalNumericSplit is the value we 
  #use for unit convertions. 
  #For some reason, this is printing the number 2X i.e. 1 would print as
  #11 and 2 would print as 22. The math works correctly (11*5 = 5, 22*5 = 10) 
  #but this may be an issue later. This is probably just an error with printing, 
  #and not with the actual numeric value 
  decimalNumericSplit <- as.numeric(sub("SubPart=|","", allLines[SplitLine]))
  #get numeric mm/pix value
  cat(decimalNumericSplit)
  
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
  #get numeric mm/pix value
  cat(decimalNumericMaxDepth)
  cat(decimalNumericMinDepth)
  
  zooscandata$Max_Depth <- decimalNumericMaxDepth
  zooscandata$Min_Depth <- decimalNumericMinDepth
  
  return(zooscandata)
}

###########################################################################
#function for reading in 333-1000um files, combining files from the same
#net and tow, and converting pixels to micrometers (um)
readZooscan333<-function(filename.a,filename.b){
  library(reader)
  #Removing the header: 
  #open file for reading in text mode 
  conn.a <- file(filename.a,open="r")
  conn.b <- file(filename.b,open="r")
  #load all the lines of the text file into a string vector called allLines
  allLines.a <-readLines(conn.a)
  allLines.b <-readLines(conn.b)
  #close connection 
  close(conn.a)
  close(conn.b)
  #Find the line number of the end of the header, and mark it as headerEnd 
  headerEnd.a <- which(allLines.a %in% '[Data]')
  headerEnd.b <- which(allLines.b %in% '[Data]')
  print(headerEnd.a)
  print(headerEnd.b)
  
  #Search for and extract the pixel resolution from the header 
  #Looks like the units are mm/pixel, (header contains rounded version of 10.5833 um/px)
  #grep searches for the line number "PixSize=" in allLines vector string 
  PixelSizeLine <- grep("PixSize=", allLines.a)
  #print the line numner where pixel size is given in mm 
  cat('line number of PixelSize = ', PixelSizeLine)
  #print the contents of the PixelSizeLine in allLines
  cat('raw string data at that line is: ', allLines.a[PixelSizeLine])
  #This convertion from string to numeric type is what we want to do math on
  #sub() function strips off all the character stuff before the useful number
  #and replaces it with nothing (shown by ""), decimalNumericPix is the value we 
  #use for unit convertions
  decimalNumericPix <- as.numeric(sub("PixSize=|","", allLines.a[PixelSizeLine]))
  #get numeric mm/pix value
  cat(decimalNumericPix)
  
  #read in data table removing everything before headerEnd
  zooscandata.a <- read.table(file=filename.a, sep = ';', header = T, row.names=1, skip = headerEnd.a)
  zooscandata.b <- read.table(file=filename.b, sep = ';', header = T, row.names=1, skip = headerEnd.b)
  
  #combine the two data files
  zoototal <- rbind(zooscandata.a,zooscandata.b)
  
  #convert from mm/pix to um/pix
  umpix <- decimalNumericPix * 1000
  print(umpix)
  
  #multiply Min and Max by 10.6 um/pix (umpix) to get size in um
  #not sure how to convert Area 
  zoototal$Minor.um <- zoototal$Minor * umpix
  zoototal$Major.um <- zoototal$Major * umpix
  zoototal$Area.um2 <- zoototal$Area * (umpix^2)
  
  #Search for and extract the volume of water filtered (m^3) 
  VolLine <- grep("Vol=", allLines.a)
  #print the line number where volume filtered is given in m^3 
  cat('line number of Vol Filtered = ', VolLine)
  #print the contents of the VolLine in allLines
  cat('raw string data at that line is: ', allLines.a[VolLine])
  #This convertion from string to numeric type is what we want to do math on
  #sub() function strips off all the character stuff before the useful number
  #and replaces it with nothing (shown by ""), decimalNumericVol is the value we 
  #use for unit convertions
  decimalNumericVol <- as.numeric(sub("Vol=|","", allLines.a[VolLine]))
  #get numeric Vol value
  cat(decimalNumericVol)
  
  #Search for and extract the split done directly before scanning 
  SplitLine <- grep("SubPart=", allLines.a)
  #print the line number where split fraction (1=1/1, 2=1/2, 4=1/4, etc.)
  cat('line number of Split = ', SplitLine)
  #print the contents of the VolLine in allLines
  cat('raw string data at that line is: ', allLines.a[SplitLine])
  #This convertion from string to numeric type is what we want to do math on
  #sub() function strips off all the character stuff before the useful number
  #and replaces it with nothing (shown by ""), decimalNumericSplit is the value we 
  #use for unit convertions. 
  #For some reason, this is printing the number 2X i.e. 1 would print as
  #11 and 2 would print as 22. The math works correctly (11*5 = 5, 22*5 = 10) 
  #but this may be an issue later. This is probably just an error with printing, 
  #and not with the actual numeric value 
  decimalNumericSplit <- as.numeric(sub("SubPart=|","", allLines.a[SplitLine]))
  #get numeric mm/pix value
  cat(decimalNumericSplit)
  
  #calculate zooplankton area accounting for 1/4 split  
  zoototal$Area.um2x4 <- zoototal$Area.um2 * 4
  #calculate zooplankton area per 100m^3, accounting for 1/4 split and additional splits 
  #since combining two files for the same net and sizeclass, divide decimalNumericSplit by
  #2 (i.e. two 1/4 splits would equate to one 1/2 split, 4/2 = 2)
  zoototal$Area.um2per100m3 <- 
    (zoototal$Area.um2x4 * (decimalNumericSplit/2) * 100)/decimalNumericVol
  
  #Add two columns showing max and min depth of net tow 
  #Search for and extract the two depths (m)
  MaxDepthLine <- grep("Zmax=", allLines.a)
  MinDepthLine <- grep("Zmin=", allLines.a)
  #print the line numbers where depths are
  cat('line number of Max Depth = ', MaxDepthLine)
  cat('line number of Min Depth = ', MinDepthLine)
  #print the contents of the objects in allLines
  cat('raw string data at that line is: ', allLines.a[MaxDepthLine])
  cat('raw string data at that line is: ', allLines.a[MinDepthLine])
  #Convert to decimal numeric 
  decimalNumericMaxDepth <- as.numeric(sub("Zmax=|","", allLines.a[MaxDepthLine]))
  decimalNumericMinDepth <- as.numeric(sub("Zmin=|","", allLines.a[MinDepthLine]))
  #get numeric mm/pix value
  cat(decimalNumericMaxDepth)
  cat(decimalNumericMinDepth)
  
  zoototal$Max_Depth <- decimalNumericMaxDepth
  zoototal$Min_Depth <- decimalNumericMinDepth
  
  return(zoototal)
}



