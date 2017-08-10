## Katherine Silliman 
## Summer 2017
## Puget Sound Larval and Juvenile Growth 2015 - Manuscript Plots

library(dplyr) #data frame grouping
library(plotrix)#for SE calculation
library(ggplot2) #Plotting

setwd("~/Projects/PS_CommonG/Analysis/Phenotype/Larvae/")
# Reading in data
all.larvae <- read.csv("PS_Larvae.csv",header = TRUE, na.strings = "#DIV/0!")
G1.all <- select(all.larvae, Population, Replicate, Date, Average_Length,Area) %>% group_by(Population,Date)

#Summarizing replicates at each time point
G1.all.mean <- summarise(G1.all,mean.Area = mean(Area, na.rm = T),
                         se.Area = std.error(Area,na.rm =T),
                         mean.Length = mean(Average_Length, na.rm = T),
                         se.Length = std.error(Average_Length,na.rm =T))
G1.all.mean$Date <- c(0,14,7,0,14,7,0,14,7)

# Plotting shell length through time
Fig.L <- ggplot(G1.all.mean, aes(x=Date, y=mean.Length, group=Population)) +
  geom_errorbar(aes(ymin=G1.all.mean$mean.Length-G1.all.mean$se.Length,ymax=G1.all.mean$mean.Length+G1.all.mean$se.Length,color=Population), width=.07) +
  geom_line(aes(color=Population) ,size = 0.7) +   
  geom_point(aes(shape=Population,color=Population), size = 3.5)+
  xlab("Days") +scale_x_continuous(breaks=c(1,7,14))+
  ylab(expression(paste("Shell Area (mm)"))) +
  theme_bw(base_size = 14) + #Set the background color
  theme(axis.text.x = element_text(vjust = 1, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.key = element_blank(),  #remove legend background
        legend.position=c(.2, .7)) + #set legend location
  ggtitle("Larvae Shell Length") + theme(plot.title = element_text(size = 18))
  #theme(plot.title = element_text(face = 'bold', 
  #                                size = 14, 
  #                                hjust = 0))
Fig.Lcolor <- Fig.L + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
png(filename = "Larvae_Length_Line_PS215_color.png", width = 700, height = 400)
Fig.Lcolor
dev.off()

Fig.L <- Fig.L + scale_color_grey()
png(filename = "Larvae_Length_Line_PS215_bw.png",width = 700, height = 400)
Fig.L
dev.off()

# JUVENILE GROWTH
setwd("~/Projects/PS_CommonG/Analysis/Phenotype/Juveniles/")
#Rread in data
all.tiles <- read.csv("Oyster Tile Size Data - Sheet9.csv",header = TRUE, na.strings = c("#DIV/0!","#VALUE!","e","gone","g","#NUM!","#REF!"))
#Filter
stack.1.A <- filter(all.tiles,A.or.B == "A",!is.na(Area.827)) %>% 
  select(Tile,Population,include, Status,Mean.length.827, Area.827, Mean.length.1014, Area.1014,I.Growth.1014,Growth.1014,Mean.length.113,Area.113,I.Growth.113,Growth.113,Tray) %>%
  group_by(Population)

#Make new dataframe with stacked dates
new.827 <- select(stack.1.A,Tile,Population,Area.827,Mean.length.827,Tray) %>%  mutate(Growth=0) %>%mutate(I.Growth=0) %>% mutate(Day = "0")
colnames(new.827) <- c("Tile","Population","Area","Length","Tray","Growth","I.Growth","Day")
new.1014 <- filter(stack.1.A,include !="n", !is.na(Area.1014),Growth.1014>0) %>%
  select(Tile,Population,Area.1014,Mean.length.1014,Tray,Growth.1014,I.Growth.1014) %>%
  mutate(Day = "48")
colnames(new.1014) <-c("Tile","Population","Area","Length","Tray","Growth","I.Growth","Day")
new.113 <- filter(stack.1.A,include!="n", !is.na(Area.113),Growth.113>0) %>%
  select(Tile,Population,Area.113,Mean.length.113,Tray,Growth.113,I.Growth.113) %>%  mutate(Day = "68")
colnames(new.113) <-c("Tile","Population","Area","Length","Tray","Growth","I.Growth","Day")
stack.1.A.ex.byDay <- rbind(new.827,new.1014,new.113)
stack.1.A.ex.byDay <- group_by(stack.1.A.ex.byDay,Day,Population)
Shell.Size = summarise(stack.1.A.ex.byDay,mean.Area = mean(Area, na.rm = T),se.Area = std.error(Area,na.rm =T),mean.Length = mean(Length, na.rm = T),se.Length = std.error(Length,na.rm =T),mean.Igrowth = mean(I.Growth,na.rm=T), se.Igrowth = std.error(I.Growth,na.rm=T),mean.Growth=mean(Growth,na.rm=T),se.Growth=std.error(Growth,na.rm=T))

#Plotting Shell Area through time
Fig.Line.area <- ggplot(Shell.Size, aes(x=Day, y=mean.Area, group=Population)) + 
  geom_errorbar(aes(ymin=Shell.Size$mean.Area-Shell.Size$se.Area,ymax=Shell.Size$mean.Area+Shell.Size$se.Area,color=Population), width=.07,position = position_dodge(width = 0.05)) +
  geom_line(aes(color=Population) ,size = 1,position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Population,color=Population), size = 4,position = position_dodge(width = 0.05))+
  xlab("Days") +
  ylab(expression(paste('Shell Area (',mm^2,') /day'))) +
  theme_bw(base_size = 16) + #Set the background color
  theme(axis.text.x = element_text(vjust = 1, hjust=1, size = 14), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.key = element_blank(),  #remove legend background
        legend.position=c(.2, .7)) + #set legend location
  ggtitle("Juvenile Shell Area") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 20, 
                                  hjust = 0))
Fig.Line.color <- Fig.Line.area + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
png(filename = "Juvenile_Area_Line_PS215_color.png")
Fig.Line.color
dev.off()

Fig.Line.bw <- Fig.Line.area + scale_color_grey()
png(filename = "Juvenile_Area_Line_PS215_bw.png")
Fig.Line.bw
dev.off()

#Growth rate
Fig.Growth.size <- ggplot(Shell.Size, aes(x=Day, y=mean.Growth, group=Population)) + 
  geom_errorbar(aes(ymin=Shell.Size$mean.Growth-Shell.Size$se.Growth,ymax=Shell.Size$mean.Growth+Shell.Size$se.Growth,color=Population), width=.07,position = position_dodge(width = 0.1)) +
  geom_line(aes(color=Population) ,size = 0.5,position = position_dodge(width = 0.1)) +   
  geom_point(aes(shape=Population,color=Population), size = 3,position = position_dodge(width = 0.1))+
  xlab("Days") +
  ylab(expression(paste('Shell Area (',mm^2,') /day'))) +
theme_bw() + #Set the background color
  theme(axis.text.x = element_text(vjust = 1, hjust=1, size = 12), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.key = element_blank(),  #remove legend background
        legend.position=c(.2, .7)) + #set legend location
  ggtitle("Juvenile Shell Area Growth Rate") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 14, 
                                  hjust = 0))

Fig.Growth.color <- Fig.Growth.size + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
png(filename = "Juvenile_Growth_Line_PS215_color.png")
Fig.Growth.color
dev.off()

Fig.Growth.bw <- Fig.Growth.size + scale_color_grey()
png(filename = "Juvenile_Growth_Line_PS215_bw.png")
Fig.Growth.bw
dev.off()

SS <- filter(Shell.Size, Day != "0")
SS$Population <- factor(SS$Population,levels =c("Dabob Bay","Oyster Bay","Fidalgo Bay"))
Bar.size <- ggplot(SS, aes(x=Day, y=mean.Growth, fill=Population)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw(base_size = 14) +
  ylab(expression(paste('Shell Area (',mm^2,') /day'))) +
  ggtitle("Juvenile Shell Area Growth Rate") +
  #theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0)) 
  theme(plot.title = element_text(size = 18)) +
  geom_errorbar(aes(ymin=SS$mean.Growth-SS$se.Growth,ymax=SS$mean.Growth+SS$se.Growth), width=.07,position = position_dodge(0.9)) +
  scale_fill_manual(values=c("#999999","#56B4E9","#E69F00" )) + 
  scale_x_discrete(labels = c("48" ="0 - 48","68" = "48 - 68"))

png(filename = "Juvenile_Growth_Bar_PS215_color.png")
Bar.size
dev.off()