getwd()

setwd("C:/Users/KatieRedmond/Desktop/Zooscan Git KR")

#code to combine matrices, provide depth range information, and make a plot to 
#reflect the relationship between area and depth based on “merging data.R”
#read in all 11 333um samples using readZooscan333 function 
moc8_net1_333<-readZooscan333("hb1907_moc8_net1_333_1_tot_1_dat1.pid","hb1907_moc8_net1_333_2_tot_1_dat1.pid")
moc8_net2_333<-readZooscan333("hb1907_moc8_net2_333_1_tot_1_dat1.pid","hb1907_moc8_net2_333_2_tot_1_dat1.pid")
moc8_net5_333<-readZooscan333("hb1907_moc8_net5_333_1_tot_1_dat1.pid","hb1907_moc8_net5_333_2_tot_1_dat1.pid")
moc12_net0_333<-readZooscan333("hb1907_moc12_net0_333_1_tot_1_dat1.pid","hb1907_moc12_net0_333_2_tot_1_dat1.pid")
moc12_net2_333<-readZooscan333("hb1907_moc12_net2_333_1_tot_1_dat1.pid","hb1907_moc12_net2_333_2_tot_1_dat1.pid")
moc12_net5_333<-readZooscan333("hb1907_moc12_net5_333_1_tot_1_dat1.pid","hb1907_moc12_net5_333_2_tot_1_dat1.pid")
moc12_net6_333<-readZooscan333("hb1907_moc12_net6_333_1_tot_1_dat1.pid","hb1907_moc12_net6_333_2_tot_1_dat1.pid")
moc11_net0_333<-readZooscan333("hb1907_moc11_net0_333_1_tot_1_dat1.pid","hb1907_moc11_net0_333_2_tot_1_dat1.pid")
moc11_net3_333<-readZooscan333("hb1907_moc11_net3_333_1_tot_1_dat1.pid","hb1907_moc11_net3_333_2_tot_1_dat1.pid")
moc11_net5_333<-readZooscan333("hb1907_moc11_net5_333_1_tot_1_dat1.pid","hb1907_moc11_net5_333_2_tot_1_dat1.pid")
moc10_net3_333<-readZooscan333("hb1907_moc10_net3_333_1_tot_1_dat1.pid","hb1907_moc10_net3_333_2_tot_1_dat1.pid")

#code to combine matrices, provide depth range information, and make a plot to 
#reflect the relationship between area and depth based on “merging data.R”
#read in all 11 1000 um samples using readZooscan function 

moc8_net1_1000<-readZooscan("hb1907_moc8_net1_1000_1_tot_1_dat1.pid")
moc8_net5_1000<-readZooscan(
moc12_net0_1000<-readZooscan(
moc12_net2_1000<-readZooscan(
moc12_net5_1000<-readZooscan(
moc12_net6_1000<-readZooscan(
moc11_net0_1000<-readZooscan(
moc11_net3_1000<-readZooscan(
moc11_net5_1000<-readZooscan()
moc10_net3_1000<-readZooscan("hb1907_moc10_net3_1000_1_tot_1_dat1.pid")


#combine all 333um samples into one dataframe called moc_333 
moc_333 <- rbind(moc8_net1_333,moc8_net2_333,moc8_net5_333,moc12_net0_333,moc12_net2_333,
                 moc12_net5_333,moc12_net6_333,moc11_net0_333,moc11_net3_333,moc11_net5_333,
                 moc10_net3_333)
#make new columns in moc_333 for max and min depths as character data 
moc_333$Max_Depth_Char <- as.character(moc_333$Max_Depth)
moc_333$Min_Depth_Char <- as.character(moc_333$Min_Depth)
#combine the new character depth columns into one depth range column, separated by a "-" 
moc_333$Depth_Range = paste(moc_333$Min_Depth_Char, moc_333$Max_Depth_Char, sep="-")
#reorder the depth column (Depth_Range) in relation to another numeric
#column (Max_Depth)
moc_333$Depth_Range = with(moc_333, reorder(Depth_Range,Max_Depth))
#plot, with coordinate flip and reversal of x axis (limits = rev)
p<-ggplot(moc_333, aes(x= Depth_Range, y=Area.um2per100m3, fill = Depth_Range)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(moc_333$Depth_Range)))
p
#####
