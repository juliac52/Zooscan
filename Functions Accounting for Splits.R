#function for reading in >5000um files and converting pixels to micrometers (um)
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
  #and replaces it with nothing (shown by ""), decimalNumeric is the value we 
  #use for unit convertions
  decimalNumeric <- as.numeric(sub("PixSize=|","", allLines[PixelSizeLine]))
  #get numeric mm/pix value
  cat(decimalNumeric)
  
  #read in data table removing everything before headerEnd
  zooscandata <- read.table(file=filename, sep = ';', header = T, row.names=1, skip = headerEnd)
  
  #convert from mm/pix to um/pix
  umpix <- decimalNumeric * 1000
  print(umpix)
  
  #multiply Min and Max by 10.6 um/pix (umpix) to get size in um
  #not sure how to convert Area 
  zooscandata$Minor.um <- zooscandata$Minor * umpix
  zooscandata$Major.um <- zooscandata$Major * umpix
  zooscandata$Area.um2 <- zooscandata$Area * (umpix^2)
  
  #Search for and extract the volume of water filtered (m^3) 
  VolLine <- grep("Vol=", allLines)
  #print the line numner where volume filtered is given in m^3 
  cat('line number of Vol Filtered = ', VolLine)
  #print the contents of the VolLine in allLines
  cat('raw string data at that line is: ', allLines[VolLine])
  #This convertion from string to numeric type is what we want to do math on
  #sub() function strips off all the character stuff before the useful number
  #and replaces it with nothing (shown by ""), decimalNumeric2 is the value we 
  #use for unit convertions
  decimalNumericVol <- as.numeric(sub("Vol=|","", allLines[VolLine]))
  #get numeric mm/pix value
  cat(decimalNumericVol)
  #calculate zooplankton area per 100m^3, accounting for 1/4 split  
  zooscandata$Area.um2x4 <- zooscandata$Area.um2 * 4
  zooscandata$Area.um2per100m3 <- (zooscandata$Area.um2x4 * 100)/decimalNumericVol
  
  return(zooscandata)
}
