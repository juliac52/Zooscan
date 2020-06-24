#made function to do this automatically 
getwd()
ZooscanData <- read_csv("~/Downloads/SampleZooscan_HB1907Moc8Net5_333_1.txt")
N <- 197
ZooscanData.1 <- tail(ZooscanData, -N)
#remove undeeded rows 
#reimport using ; as seperater 
#reminder: we need to convert pixels to um at some point, working with pixels for now 
#plots: Area, Min (minimum diameter/axis), Max (maximum diameter/axis) 
#calcualte mean area: 
mean(ZooscanData.1$Area)

#histogram
bins<-c(243,seq(from=249, to=255, by=1))
hist(ZooscanData$Max)
print("done")

