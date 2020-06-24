#Zooscan Data Documentation
##########################################################################################
#####################Calculations####################
#Converting from pixels to um:
#(1 in/2400 pix)(25.4 mm/1 in)(10^3um/1 mm) = 10.5833 um/pix
#see line 78 for resolution = 2400 for future definition of a variable 
#(if Resol=2400, there are 10.5833 microns/pixel)
#Values of “Area” column have to be transformed by squaring convertion factor 
#becasue area is in 2D

#Calculating Area um^2 per 100m^3 for 5000 and 1000 samples:
#(Area.um2 * 4 * split * 100m^3)/(volume of water filtered m^3) = Area.um2/100m^3
#multiply by 4 to compensate for initial 1/4 split 
#multiply by split to compensate for splitting prior to scanning 

#Calculating Area um^2 per 100m^3 for 333 samples:
#(Area.um2 * 4 * (split/2) * 100m^3)/(volume of water filtered m^3) = Area.um2/100m^3
#multiply by 4 to compensate for initial 1/4 split 
#divide split by 2 since two samples are combined, and split will be 2x greater in combined
#dataframe than individual dataframes

####################Functions#################### 
#readZooscan:
#updated in file "Functions Accounting for Splits.R"
#used for 5000 and 1000 files, 333 needs a separate function 
#reads in raw data file, removing header metadata until line including [Data]
#converts Minor and Major from pixels to micrometers (um) and Area to micrometers squared (um2)
#calculates Area um^2 per 100m^3 of water filtered by MOCNESS, see above calculations 
#adds columns for max and min depths sampled 

#readZooscan333:
#added to file "Functions Accounting for Splits.R"
#used for 333 files
#reads in two raw data files from same tow, net, and size fraction, removing header metadata until line including [Data]
#combines the two files
#converts Minor and Major from pixels to micrometers (um) and Area to micrometers squared (um2)
#calculates Area um^2 per 100m^3 of water filtered by MOCNESS, see above calculations 
#adds columns for max and min depths sampled 






