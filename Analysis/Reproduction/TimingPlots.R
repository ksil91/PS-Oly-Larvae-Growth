## Katherine Silliman 
## Summer 2017
## Puget Sound Reproduction 2015 - Manuscript Plots

library(ggplot2) #for plotting
library(dplyr) #Data summary
library(plotrix)#for SE calculation
library("grid")#plotting
library(gridExtra) #plotting

setwd("~/Projects/PS_CommonG/Analysis/Phenotype")
larvae = read.csv("Larval counts - Day 1-By Family.csv", header = TRUE)
larvae$Population <- gsub("\\s{1}\\(HC\\)","",larvae$Population)
larvae$Population <- gsub("\\s{1}\\(SS\\)","",larvae$Population)
larvae$Population <- gsub("\\s{1}\\(NF\\)","",larvae$Population)

pop_larvae <- subset(larvae, select = c(Date,Population,Family,Total.Larvae,Total.per.capita)) %>% filter(Family != "") %>% group_by(Date,Population)
pop_larvae$Date <- as.Date(pop_larvae$Date, "%m/%d/%Y")
pop_larvae <- arrange(pop_larvae, Date) %>% mutate(CalDay = format(Date,"%j"))

#Calculate total larvae released across families
pop_total <- summarise(pop_larvae, total.by.date = sum(Total.Larvae))

#Get the number of larvae produced per oyster "percap"
percap.norms <- function(x) {  #write function
  if(x == "Oyster Bay") y <- 99 
  if(x == "Dabob Bay") y <- 94 
  if(x == "Fidalgo Bay") y <- 99 
  return(y) #return result
}
pop_total$percap.norm <- as.numeric(sapply(pop_total$Population,percap.norms))
pop_total$total.per.cap <- pop_total$total.by.date/pop_total$percap.norm 

#Calculate cumulative larvae released through time
pop_total <- group_by(pop_total, Population) %>% mutate(cum.total=cumsum(total.by.date),cum.percap = cumsum(total.per.cap),CalDay = format(Date,"%j")) %>% arrange(Date) %>% select(Date,CalDay,Population,total.by.date,total.per.cap,cum.total,cum.percap)

# Make stacked graphs of cumulative larvae through time and number of larvae released by date
b3<- ggplot(pop_total, aes(x=Date, y=total.by.date)) + facet_grid(Population~.,)+
  geom_bar(stat="identity",position=position_dodge(),colour="black",fill="grey") + ylab("Number of Larve")+
  ggtitle("Timing of Larvae Release by Population")  + scale_y_continuous(labels = c("0","","","","4e5"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+theme_bw()  +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

l_bw <- ggplot(data=pop_total, aes(x=Date, y=cum.total, group=Population, colour=Population)) +
  geom_line() + ylab("Cumulative Number of Larve") +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+ scale_y_continuous(breaks = seq(0,2500000,by=500000),labels = c("0","","1e6","","2e6",""))+
  scale_color_grey()+theme_bw() +  theme(plot.margin = unit(c(0,0.9,0,0.2),"cm")) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.position = c(.2,.8))

jpeg(filename = "stacked_larvae_timing_PS2015_bw.jpeg")
grid.arrange(b3,l_bw,nrow=2)
dev.off()

#Stacked graph with color
b <- ggplot(data=pop_total, aes(x=Date, y=total.by.date, group=Population, fill=Population)) + 
  geom_bar(stat="identity",position=position_dodge()) + ylab("Number of Larve Released")+
  ggtitle("Timing of Larvae Release by Population") + 
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+theme_bw() 

l <- ggplot(data=pop_total, aes(x=Date, y=cum.total, group=Population, colour=Population)) +
  geom_line() + geom_point() + ylab("Number of Larve Released") +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+theme_bw() 

jpeg(filename = "stacked_larvae_timing_PS2015_color.jpeg")
grid.newpage()
grid.draw(rbind(ggplotGrob(b),ggplotGrob(l),size="last"))
dev.off()

# Make stacked graphs per cap cumulative larvae through time and number of larvae released by date
b3<- ggplot(pop_total, aes(x=Date, y=total.per.cap)) + facet_grid(Population~.,)+
  geom_bar(stat="identity",position=position_dodge(),colour="black",fill="grey") + ylab("Number of Larve")+
  ggtitle("Timing of Larvae Release by Population")  + 
  scale_y_continuous(labels = c("0","","","","4e3"))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+theme_bw()  +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

l_bw <- ggplot(data=pop_total, aes(x=Date, y=cum.percap, group=Population, colour=Population)) +
  geom_line() + ylab("Cumulative Number of Larve") +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+ 
  scale_y_continuous(breaks = seq(0,30000,by=5000),labels = c("0","","1e4","","2e4","","3e4"))+
  scale_color_grey()+theme_bw() +  theme(plot.margin = unit(c(0,0.9,0,0.2),"cm")) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.position = c(.2,.8))

jpeg(filename = "stacked_larvae_timing_PS2015_bw_percap.jpeg")
grid.arrange(b3,l_bw,nrow=2)
dev.off()

#Stacked graph with color
b <- ggplot(data=pop_total, aes(x=Date, y=total.per.cap, group=Population, fill=Population)) + 
  geom_bar(stat="identity",position=position_dodge()) + ylab("Number of Larve Released")+
  ggtitle("Timing of Larvae Release by Population") + 
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+theme_bw() 

l <- ggplot(data=pop_total, aes(x=Date, y=cum.percap, group=Population, colour=Population)) +
  geom_line() + geom_point() + ylab("Number of Larve Released") +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+theme_bw() 

jpeg(filename = "stacked_larvae_timing_PS2015_color_percap.jpeg")
grid.newpage()
grid.draw(rbind(ggplotGrob(b),ggplotGrob(l),size="last"))
dev.off()


#Overlay graph with color
bb <- ggplot(data=pop_total, aes(x=Date, y=total.per.cap, group=Population, fill=Population)) + 
  geom_bar(stat="identity",position=position_dodge()) + ylab("Number of Larve Released")+
  ggtitle("Timing of Larvae Release by Population") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

bl <- bb+ geom_line(data=pop_total, aes(x=Date, y=cum.percap/5, group=Population, colour=Population),size=1.2) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(sec.axis = sec_axis(~.*5,name="Cumulative Larvae Released"))
  
jpeg(filename = "overlay_larvae_timing_PS2015_color_percap.jpeg")
grid.newpage()
bl
dev.off()

# Using buckets as replicates

family_larvae <- group_by(pop_larvae,Family) %>% filter(Family !="")
fam.sum <- summarize(family_larvae, overall_Total = sum(Total.Larvae, na.rm = T), 
                     mean.larvae = mean(Total.Larvae,na.rm=T), 
                     se.larvae = std.error(Total.Larvae,na.rm=T), 
                     mean.percap = mean(Total.per.capita,na.rm=T), 
                     total.percap = sum(Total.per.capita,na.rm=T), 
                     maxday = as.numeric(CalDay[which.max(Total.Larvae)]), 
                     max = max(Total.Larvae), max.percap = max(Total.per.capita), 
                     Population = first(Population), 
                     first.big = as.numeric(CalDay[which(Total.Larvae > 0)[1]]))

#Cumulative larvae violin plots, with bucket as replicate
tc.plot <- ggplot(fam.sum, aes(x=Population, y=total.percap,fill=Population)) + 
  geom_violin(trim=FALSE) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  geom_boxplot(width=0.05,fill="white") + 
  labs(title="Cumulative Larvae",y=expression("Cumulative Larvae")) + 
  theme_bw(base_size = 14)+
  theme(title = element_text(size = 18))
jpeg(filename = "Cum_byFam_PS2015_color.jpeg")
tc.plot
dev.off()

tc.plot <- ggplot(fam.sum, aes(x=Population, y=total.percap,fill=Population)) + 
  geom_violin(trim=FALSE) + 
  geom_boxplot(width=0.05,fill="white") + 
  labs(title="Cumulative Larvae",y=expression("Cumulative Larvae")) + 
  theme_bw(base_size = 14)+
  scale_fill_grey()+
  theme(title = element_text(size = 18))
jpeg(filename = "Cum_byFam_PS2015_bw.jpeg")
tc.plot
dev.off()

#First day of larval release violin plots, with bucket as replicate
fd.plot <- ggplot(fam.sum, aes(x=Population, y=first.big,fill=Population)) + 
  geom_violin(trim=FALSE) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  geom_boxplot(width=0.05,fill="white") + 
  labs(title="Date of First Larval Release",y="Calendar Day") + 
  theme_bw(base_size = 14)+
  theme(title = element_text(size = 18))
jpeg(filename = "1stDay_byFam_PS2015_color.jpeg")
fd.plot
dev.off()

fd.plot <- ggplot(fam.sum, aes(x=Population, y=first.big,fill=Population)) + 
  geom_violin(trim=FALSE) + 
  geom_boxplot(width=0.05,fill="white") + 
  labs(title="Date of First Larval Release",y="Calendar Day") + 
  theme_bw(base_size = 14)+
  scale_fill_grey()+
  theme(title = element_text(size = 18))
jpeg(filename = "1stDay_byFam_PS2015_bw.jpeg")
fd.plot
dev.off()

tc.plot <- tc.plot + theme(legend.position="none") + labs(title="a. Cumulative Larvae") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
fd.plot <- fd.plot + labs(title="b. Date of First Larval Release") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
jpeg(filename = "Cum_1stDay_Comb_PS2015_color.jpeg", width = 1156, height = 400)
grid.arrange(tc.plot,fd.plot, ncol = 2, widths = c(1.68,2))
dev.off()