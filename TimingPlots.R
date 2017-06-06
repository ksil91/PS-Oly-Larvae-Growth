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
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.position = c(.2,.8)) +

png(filename = "stacked_larvae_timing_PS2015_bw.png")
grid.arrange(b3,l,nrow=2)
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

png(filename = "stacked_larvae_timing_PS2015_color.png")
grid.newpage()
grid.draw(rbind(ggplotGrob(b),ggplotGrob(l),size="last"))
dev.off()

#Overlay graph with color
bb <- ggplot(data=pop_total, aes(x=Date, y=total.per.cap, group=Population, fill=Population)) + 
  geom_bar(stat="identity",position=position_dodge()) + ylab("Number of Larve Released")+
  ggtitle("Timing of Larvae Per Capita Release by Population") + 
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+theme_bw() 

ll <- ggplot(data=pop_total, aes(x=Date, y=cum.percap, group=Population, colour=Population)) +
  geom_line() + geom_point() + ylab("Number of Larve Released") +
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+theme_bw() 
png(filename = "overlay_larvae_timing_PS2015_color.png")
grid.newpage()
grid.draw(rbind(ggplotGrob(bb),ggplotGrob(ll),size="last"))
