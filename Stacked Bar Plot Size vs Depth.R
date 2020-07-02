# library
library(ggplot2)

#run depth range and area plot file first
# Stacked bar graph
ggplot(moc_all, aes(fill=size_char, y=Area.um2per100m3, x=Depth_Range)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Size Distribution Across Various Depths")+ 
  labs(x="Depth Ranges", y= "Size (Area.um2)")+coord_flip()




