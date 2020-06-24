#DAY
#create first matrix moc8 daytime, size creatures >5000um
first <- data.frame(
                    x1=Data3$Area.um2per100m3,
                    x2=c("1000-799.7"))
#view first matrix
first
#create second matrix, moc8 daytime, size creatures=1000um
second <- data.frame(
                    x1=Data2$Area.um2per100m3,
                   x2=c("1000-799.7"))
#view
second

#moc8 day, size creatures = 333um
third <- data.frame(
  x1=Data5$Area.um2per100m3,
  x2=c("1000-799.7"))

#hb1907_moc8_net2_5000
seventh <- data.frame(
  x1=Data8$Area.um2per100m3,
  x2=c("799.7-600.9"))

#hb1907_moc8_net2_1000
eighth <- data.frame(
  x1=Data9$Area.um2per100m3,
  x2=c("799.7-600.9"))

#hb1907_moc8_net2_333_1:2
ninth <- data.frame(
  x1=Data10$Area.um2per100m3,
  x2=c("799.7-600.9"))


#hb1907_moc12_net2_5000 
seventeen <-data.frame(
  x1=Data17$Area.um2per100m3,
  x2=c("799.9-700.4"))

#hb1907_moc12_net2_1000 
eighteen <-data.frame(
  x1=Data18$Area.um2per100m3,
  x2=c("799.9-700.4"))

#hb1907_moc12_net2_5000 (there are two samples for sizes 333um)
nineteen <-data.frame(
  x1=Data19$Area.um2per100m3,
  x2=c("799.9-700.4"))

#hb1907_moc12_net5_5000 
twenty <-data.frame(
  x1=Data20$Area.um2per100m3,
  x2=c("599.9-499.4"))

#hb1907_moc12_net5_1000 
twentyone <-data.frame(
  x1=Data21$Area.um2per100m3,
  x2=c("599.9-499.4"))

#hb1907_moc12_net5_333_1:2
twentytwo <-data.frame(
  x1=Data22$Area.um2per100m3,
  x2=c("599.9-499.4"))

#hb1907_moc12_net6_5000
twentythree <-data.frame(
  x1=Data23$Area.um2per100m3,
  x2=c("499.4-351.1-498.6"))

#hb1907_moc12_net6_1000
twentyfour <-data.frame(
  x1=Data24$Area.um2per100m3,
  x2=c("499.4-351.1-498.6"))

#hb1907_moc12_net6_333_1:2
twentyfive <-data.frame(
  x1=Data25$Area.um2per100m3,
  x2=c("499.4-351.1-498.6"))

#merge all matrix from tow 8, net 1
zoodata.day <- rbind(first,second, third, seventh, eighth, ninth,
                      seventeen, eighteen, nineteen, twenty, twentyone,
                     twentytwo,twentythree, twentyfive, twentyfour,
                     deparse.level = 15)

##############################################################################
#NIGHT

#hb1907_moc10_net3_1000
fourth <- data.frame(
  x1=Data4$Area.um2per100m3,
  x2=c("599.9-400.2"))
#view
fourth

#hb1907_moc10_net3_5000
fifth<- data.frame(
  x1=Data6$Area.um2per100m3,
  x2=c("599.9-400.2"))

#hb1907_moc10_net3_333_1:2
sixth <- data.frame(
  x1=Data7$Area.um2per100m3,
  x2=c("599.9-400.2"))

##hb1907_moc11_net3_5000
ten <-data.frame(
  x1=Data11$Area.um2per100m3,
  x2=c("599.9-399.5"))

#hb1907_moc11_net3_1000
eleven <-data.frame(
  x1=Data12$Area.um2per100m3,
  x2=c("599.9-399.5"))

#hb1907_moc11_net3_333_1:2
twelve <-data.frame(
  x1=Data13$Area.um2per100m3,
  x2=c("599.9-399.5"))

#hb1907_moc11_net5_5000
fourteen <-data.frame(
  x1=Data14$Area.um2per100m3,
  x2=c("199.5-99.3"))

#hb1907_moc11_net5_1000
fifteen <-data.frame(
  x1=Data15$Area.um2per100m3,
  x2=c("199.5-99.3"))

#hb1907_moc11_net5_333_1:2 (there are two samples for sizes 333um)
sixteen <-data.frame(
  x1=Data16$Area.um2per100m3,
  x2=c("199.5-99.3"))

#merge all sizes of night
zoodata.night <- rbind(fourth, fifth, sixth, ten, eleven, twelve, fourteen, fifteen,
                       sixteen, 
                       deparse.level=9)


########################################################################
#set up the data
## set the levels in order we want
zoodata.day <- within(zoodata.day, 
                      x2 <- factor(x2, 
                                   levels=names(sort(table(x2), 
                                  increasing=TRUE))))

                                                                                                increasing=TRUE))))
#plotDAY
ggplot(data = zoodata.day,
        aes(x = x1, y = x2))+
  geom_bar(stat = "identity") +
labs(x = "Biomass (Area.um2per100m3)", y = "Depth (m)",
      title = "Biomass at Various Depths",
      subtitles = "Day Time")+
  theme_minimal()+
  guides(fill = FALSE) 

#sd??+
  geom_errorbar(data = zoodata.day, aes(ymin=(x1-5455750), ymax=(x1+5455750)), 
               width=.2, position=position_dodge(.9))
 

#set up data
## set the levels in order we want
zoodata.night <- within(zoodata.night, 
                      x2 <- factor(x2, 
                                   levels=names(sort(table(x2), 
                                                     increasing=TRUE))))
#plotNIGHT
ggplot(data = zoodata.night,
       aes(x = x1, y = x2))+
  geom_bar(stat = "identity") +
  labs(x = "Biomass (Area.um2per100m3)", y = "Depth (m)",
       title = "Biomass at Various Depths",
       subtitles = "Night Time")+
  theme_minimal()+
  guides(fill = FALSE) + facet_wrap(~zoodata.night)

library(viridis)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()


install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

#install r color palettes 
install.packages('RColorBrewer')
library(RColorBrewer)
head(brewer.pal.info)
#########################################################################
#I think the increasing=TRUE argument didn't work because the depth data is 
#character data rather than numeric data. Can check this with class(zoodata.day$x2)
#Since character data cannot be ordered, need to associate this column with numeric 
#data.
#Reorder the depth column (x2) in relation to another numeric column (for example, Max_Depth). 
#For this matix, will need to add an additional column including the numeric depth data 
#in order for this method to work. 
zoodata.day$x2 = with(zoodata.day, reorder(x2,Max_Depth))
#plot, with coordinate flip and reversal of x axis (limits = rev)
p <- ggplot(zoodata.day, aes(x= x2, y=x1, fill=x2)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(zoodata.day$x2)))


 
