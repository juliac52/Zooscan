# library
library(ggplot2)
library(grid)
library(gridExtra)

#run depth range and area plot file first
# Stacked bar graph
#plot of size 500um data
ggplot(moc_5000, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2)")+coord_flip()

#plot of size 1000um data
ggplot(moc_1000, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2)")+coord_flip()


#plot of size 333um data
ggplot(moc_333, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2)")+coord_flip()

#plot of all data
ggplot(moc_all, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2)")+coord_flip()


#####

#plot based on tow
#tow8
moc8_net1_5000<-readZooscan("hb1907_moc8_net1_5000_1_tot_1_dat1.pid")
moc8_net2_5000<-readZooscan("hb1907_moc8_net2_5000_1_tot_1_dat1.pid")
moc8_net3_5000<-readZooscan("hb1907_moc8_net3_5000_1_tot_1_dat1.pid")
moc8_net4_5000<-readZooscan("hb1907_moc8_net4_5000_1_tot_1_dat1.pid")
moc8_net5_5000<-readZooscan("hb1907_moc8_net5_5000_1_tot_1_dat1.pid")
moc8_net6_5000<-readZooscan("hb1907_moc8_net6_5000_1_tot_1_dat1.pid")
moc8_net8_5000<-readZooscan("hb1907_moc8_net8_5000_1_tot_1_dat1.pid")
moc8_net1_1000<-readZooscan("hb1907_moc8_net1_1000_1_tot_1_dat1.pid")
moc8_net2_1000<-readZooscan("hb1907_moc8_net2_1000_1_tot_1_dat1.pid")
moc8_net3_1000<-readZooscan("hb1907_moc8_net3_1000_1_tot_1_dat1.pid")
moc8_net4_1000<-readZooscan("hb1907_moc8_net4_1000_1_tot_1_dat1.pid")
moc8_net5_1000<-readZooscan("hb1907_moc8_net5_1000_1_tot_1_dat1.pid")
moc8_net6_1000<-readZooscan("hb1907_moc8_net6_1000_1_tot_1_dat1.pid")
moc8_net8_1000<-readZooscan("hb1907_moc8_net8_1000_1_tot_1_dat1.pid")
moc8_net1_333<-readZooscan333("hb1907_moc8_net1_333_1_tot_1_dat1 (1).pid","hb1907_moc8_net1_333_2_tot_1_dat1.pid")
moc8_net2_333<-readZooscan333("hb1907_moc8_net2_333_1_tot_1_dat1.pid","hb1907_moc8_net2_333_2_tot_1_dat1.pid")
moc8_net3_333<-readZooscan333("hb1907_moc8_net3_333_1_tot_1_dat1.pid","hb1907_moc8_net3_333_2_tot_1_dat1.pid")
moc8_net4_333<-readZooscan333("hb1907_moc8_net4_333_1_tot_1_dat1.pid","hb1907_moc8_net4_333_2_tot_1_dat1.pid")
moc8_net5_333<-readZooscan333("hb1907_moc8_net5_333_1_tot_1_dat1.pid","hb1907_moc8_net5_333_2_tot_1_dat1.pid")
moc8_net6_333<-readZooscan333("hb1907_moc8_net6_333_1_tot_1_dat1.pid","hb1907_moc8_net6_333_2_tot_1_dat1.pid")
moc8_net8_333<-readZooscan333("hb1907_moc8_net8_333_1_tot_1_dat1.pid","hb1907_moc8_net8_333_2_tot_1_dat1.pid")
#combine tow 8 files
moc_8 <- rbind(moc8_net1_5000, moc8_net2_5000,moc8_net3_5000, moc8_net4_5000,
               moc8_net5_5000,moc8_net6_5000, moc8_net8_5000, moc8_net1_1000,
               moc8_net2_1000,moc8_net3_1000, moc8_net4_1000, moc8_net5_1000, 
               moc8_net6_1000, moc8_net8_1000, moc8_net1_333, moc8_net2_333, 
               moc8_net3_333, moc8_net4_333, moc8_net5_333, moc8_net6_333,
               moc8_net8_333)

#make new columns in moc_1000 for max and min depths as character data 
moc_8$Max_Depth_Char <- as.character(moc_8$Max_Depth)
moc_8$Min_Depth_Char <- as.character(moc_8$Min_Depth)
#combine the new character depth columns into one depth range column, separated by a "-" 
moc_8$Depth_Range = paste(moc_8$Min_Depth_Char, moc_8$Max_Depth_Char, sep="-")
#reorder the depth column (Depth_Range) in relation to another numeric
#column (Max_Depth)
moc_8$Depth_Range = with(moc_8, reorder(Depth_Range,Max_Depth))

#cut() to create another column based on range of Area values
moc_8$size = cut(moc_8$Area.um2, c(1.2e+04,1.2e+05, 1.2e+06,1.2e+07,
                                         1.2e+08, 1.2e+09,1.2e+10, 1.2e+11))

moc_8$Density_Ranges <- as.character(moc_8$size)


#plot of all data
p8<-ggplot(moc_8, aes(fill=size_char, y=Area.um2per100m3, x = Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths for Moc8")+ 
  labs(x="Depth Ranges (m)", y= "Density (Area.um2per100m3)")+
  coord_flip()
p8

############tow10 note:only one net
moc10_net3_333<-readZooscan333("hb1907_moc10_net3_333_1_tot_1_dat1.pid","hb1907_moc10_net3_333_2_tot_1_dat1.pid")
moc10_net3_1000<-readZooscan("hb1907_moc10_net3_1000_1_tot_1_dat1.pid")
moc10_net3_5000<-readZooscan("hb1907_moc10_net3_5000_1_tot_1_dat1.pid")

#combine tow 10 files
moc_10 <- rbind(moc10_net3_333, moc10_net3_1000, moc10_net3_5000)

#make new columns in moc_1000 for max and min depths as character data 
moc_10$Max_Depth_Char <- as.character(moc_10$Max_Depth)
moc_10$Min_Depth_Char <- as.character(moc_10$Min_Depth)
#combine the new character depth columns into one depth range column, separated by a "-" 
moc_10$Depth_Range = paste(moc_10$Min_Depth_Char, moc_10$Max_Depth_Char, sep="-")
#reorder the depth column (Depth_Range) in relation to another numeric
#column (Max_Depth)
moc_10$Depth_Range = with(moc_10, reorder(Depth_Range,Max_Depth))

#cut() to create another column based on range of Area values
moc_10$size = cut(moc_10$Area.um2, c(1.2e+04,1.2e+05, 1.2e+06,1.2e+07,
                                   1.2e+08, 1.2e+09,1.2e+10, 1.2e+11))

moc_10$size_char <- as.character(moc_10$size)


#plot of all data
p10<-ggplot(moc_10, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2per100m3)")+coord_flip()


 


#########tow11
moc11_net0_333<-readZooscan333("hb1907_moc11_net0_333_1_tot_1_dat1.pid","hb1907_moc11_net0_333_2_tot_1_dat1.pid")
moc11_net3_333<-readZooscan333("hb1907_moc11_net3_333_1_tot_1_dat1.pid","hb1907_moc11_net3_333_2_tot_1_dat1.pid")
moc11_net5_333<-readZooscan333("hb1907_moc11_net5_333_1_tot_1_dat1.pid","hb1907_moc11_net5_333_2_tot_1_dat1.pid")
moc11_net0_1000<-readZooscan("hb1907_moc11_net0_1000_1_tot_1_dat1.pid")
moc11_net3_1000<-readZooscan("hb1907_moc11_net3_1000_1_tot_1_dat1.pid")
moc11_net5_1000<-readZooscan("hb1907_moc11_net5_1000_1_tot_1_dat1.pid")
moc11_net0_5000<-readZooscan("hb1907_moc11_net0_5000_1_tot_1_dat1.pid")
moc11_net3_5000<-readZooscan("hb1907_moc11_net3_5000_1_tot_1_dat1.pid")
moc11_net5_5000<-readZooscan("hb1907_moc11_net5_5000_1_tot_1_dat1.pid")

#combine tow 11 files
moc_11 <- rbind(moc11_net0_333, moc11_net3_333, moc11_net5_333,moc11_net0_1000,
                moc11_net3_1000, moc11_net5_1000, moc11_net0_5000,
                moc11_net3_5000, moc11_net5_5000)

#make new columns in moc_1000 for max and min depths as character data 
moc_11$Max_Depth_Char <- as.character(moc_11$Max_Depth)
moc_11$Min_Depth_Char <- as.character(moc_11$Min_Depth)
#combine the new character depth columns into one depth range column, separated by a "-" 
moc_11$Depth_Range = paste(moc_11$Min_Depth_Char, moc_11$Max_Depth_Char, sep="-")
#reorder the depth column (Depth_Range) in relation to another numeric
#column (Max_Depth)
moc_11$Depth_Range = with(moc_11, reorder(Depth_Range,Max_Depth))

#cut() to create another column based on range of Area values
moc_11$size = cut(moc_11$Area.um2, c(1.2e+04,1.2e+05, 1.2e+06,1.2e+07,
                                   1.2e+08, 1.2e+09,1.2e+10, 1.2e+11))

moc_11$size_char <- as.character(moc_11$size)


#plot of all data
p11<-ggplot(moc_11, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2per100m3)")+coord_flip()




###############tow12
moc12_net0_333<-readZooscan333("hb1907_moc12_net0_333_1_tot_1_dat1.pid","hb1907_moc12_net0_333_2_tot_1_dat1.pid")
moc12_net2_333<-readZooscan333("hb1907_moc12_net2_333_1_tot_1_dat1.pid","hb1907_moc12_net2_333_2_tot_1_dat1.pid")
moc12_net5_333<-readZooscan333("hb1907_moc12_net5_333_1_tot_1_dat1.pid","hb1907_moc12_net5_333_2_tot_1_dat1.pid")
moc12_net6_333<-readZooscan333("hb1907_moc12_net6_333_1_tot_1_dat1.pid","hb1907_moc12_net6_333_2_tot_1_dat1.pid")
moc12_net0_1000<-readZooscan("hb1907_moc12_net0_1000_1_tot_1_dat1.pid")
moc12_net2_1000<-readZooscan("hb1907_moc12_net2_1000_1_tot_1_dat1.pid")
moc12_net5_1000<-readZooscan("hb1907_moc12_net5_1000_1_tot_1_dat1.pid")
moc12_net6_1000<-readZooscan("hb1907_moc12_net6_1000_1_tot_1_dat1.pid")
moc12_net0_5000<-readZooscan("hb1907_moc12_net0_5000_1_tot_1_dat1.pid")
moc12_net2_5000<-readZooscan("hb1907_moc12_net2_5000_1_tot_1_dat1.pid")
moc12_net5_5000<-readZooscan("hb1907_moc12_net5_5000_1_tot_1_dat1.pid")
moc12_net6_5000<-readZooscan("hb1907_moc12_net6_5000_1_tot_1_dat1.pid")


#combine tow 12 files
moc_12 <- rbind(moc12_net0_333, moc12_net2_333, moc12_net5_333,
                moc12_net6_333, moc12_net0_1000, moc12_net2_1000,
                moc12_net5_1000, moc12_net6_1000, moc12_net0_5000,
                moc12_net2_5000, moc12_net5_5000, moc12_net6_5000)

#make new columns in moc_1000 for max and min depths as character data 
moc_12$Max_Depth_Char <- as.character(moc_12$Max_Depth)
moc_12$Min_Depth_Char <- as.character(moc_12$Min_Depth)
#combine the new character depth columns into one depth range column, separated by a "-" 
moc_12$Depth_Range = paste(moc_12$Min_Depth_Char, moc_12$Max_Depth_Char, sep="-")
#reorder the depth column (Depth_Range) in relation to another numeric
#column (Max_Depth)
moc_12$Depth_Range = with(moc_12, reorder(Depth_Range,Max_Depth))

#cut() to create another column based on range of Area values
moc_12$size = cut(moc_12$Area.um2, c(1.2e+04,1.2e+05, 1.2e+06,1.2e+07,
                                   1.2e+08, 1.2e+09,1.2e+10, 1.2e+11))

moc_12$size_char <- as.character(moc_12$size)


#plot of all data
p12<-ggplot(moc_12, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2per100m3)")+coord_flip()


#####combine graphs into one frame #way too small, doesn't work
grid.arrange(p8, p10, p11, p12, nrow = 2)


ggplot(moc_12, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_violin(width=1.4)+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2per100m3)")

