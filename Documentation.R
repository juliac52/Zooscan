#Zooscan Data Documentation
##########################################################################################
#Converting from pixels to um:
#(1 in/2400 pix)(25.4 mm/1 in)(10^3um/1 mm) = 10.5833 um/pix
#see line 78 for resolution = 2400 for future definition of a variable 
#(if Resol=2400, there are 10.5833 microns/pixel)
#Values of “Area” column have to be transformed by squaring convertion factor 
#becasue area is in 2D

#Functions 
#readZooscan: 
#reads in raw data file, removing header metadata until line including [Data]
#converts Min and Max from pixels to micrometers (um) and Area to micrometers squared (um2),
#see columns zooscandata$Min.um, zooscandata$Max.um, and zooscandata$Area.um2
#use View(zooscandata) to open resulting table




