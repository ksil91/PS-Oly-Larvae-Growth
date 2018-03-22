## Katherine Silliman 
## Summer 2017
## Puget Sound Reproduction 2015 - Manuscript Plots

library(ggplot2) #for plotting
library(dplyr) #Data summary
library(plotrix)#for SE calculation
library("grid")#plotting
library(gridExtra) #plotting
library(scales) #plotting

larvae = read.csv("../Data/Larval counts - Day 1-By Family.csv", header = TRUE)
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

#Overlay graph with color
bb <- ggplot(data=pop_total, aes(x=Date, y=total.per.cap, group=Population, fill=Population)) + 
  geom_bar(stat="identity",position=position_dodge()) + ylab("Number of Larvae Released") +
  ggtitle("Timing of Larvae Release by Population") + theme_bw(base_size = 16) +   
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))+
  scale_x_date(date_breaks = "1 week",date_labels ="%b-%d")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

bl <- bb+ geom_line(data=pop_total, aes(x=Date, y=cum.percap/5, group=Population, colour=Population),size=1.2) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(sec.axis = sec_axis(label=comma,~.*5,name="Cumulative Larvae Released"), label=comma)

jpeg(filename = "Figure1_Reproduction.jpeg")
grid.newpage()
bl
dev.off()

setEPS()
postscript("Figure1_Reproduction.eps")
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
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_y_continuous(label=comma)

#First day of larval release violin plots, with bucket as replicate
fd.plot <- ggplot(fam.sum, aes(x=Population, y=first.big,fill=Population)) + 
  geom_violin(trim=FALSE) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  geom_boxplot(width=0.05,fill="white") + 
  labs(title="Date of First Larval Release",y="Calendar Day") + 
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0))

tc.plot <- tc.plot + theme(legend.position="none",legend.text = element_text(size = 18)) + labs(title="a. Cumulative Larvae") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
fd.plot <- fd.plot + labs(title="b. Date of First Larval Release") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#jpeg(filename = "Cum_1stDay_Comb_PS2015_color.jpeg", width = 1156, height = 400)
grid.arrange(tc.plot,fd.plot, ncol = 2, widths = c(1.68,2))
dev.off()