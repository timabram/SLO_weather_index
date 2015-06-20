### Wunderground data

setwd("C:\\Users\\Tim\\Dropbox\\Bootstrap Web Design\\Data Playground\\wunderground data\\")
library(ggplot2)
library(grid)
library(stringr)
library(stringi)

# Multiple plot function - from R-Cookbook (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Load Data and add column titles:
slo_2014 <- read.csv('Tuscany_weather_2014.txt', sep=';',header=F, as.is=T, encoding="latin1")

colnames(slo_2014)<-c("Time_EST","Temp", "Windchill", "Dew_Point", 
                      "Humidity", "Pressure", "Visibility", "Wind_Dir",
                      "Wind_Speed", "Gust_Speed", "Precip", "Events", "Conditions",
                      "Date", "Location")

# ----------------------------------Data Formatting-----------------------------------

    slo_2014$Time<-as.POSIXlt(slo_2014$Time_EST, format = '%I:%M %p')
    slo_2014$Date <-as.Date(slo_2014$Date, '%Y-%m-%d')
    slo_2014$Conditions <-factor(slo_2014$Conditions)
    slo_2014[,"Full_Date"] <- paste(slo_2014$Date, ' ', slo_2014$Time_EST, sep="")
    slo_2014$Full_Date <- as.POSIXlt(slo_2014$Full_Date, '%Y-%m-%d %I:%M %p', tz="CDT")
    slo_2014[which(slo_2014$Precip == 'N/A'),"Precip"]<-"0.00 in"
    slo_2014[,"Precip"] <- as.data.frame(as.numeric(substr(slo_2014$Precip, 1, nchar(slo_2014$Precip)-3)))
    #Ex: Total precipitation for 2014:
    #sum(slo_2014$Precip, na.rm=T)
    slo_2014[,"Temp"] <- as.data.frame(as.numeric(substr(slo_2014$Temp, 1, 
                                                         nchar(slo_2014$Temp)-3)))
    slo_2014[,"Visibility"] <- as.data.frame(as.numeric(substr(slo_2014$Visibility, 1, 
                                                               nchar(slo_2014$Visibility)-3)))
    slo_2014[,"Windchill"] <- as.data.frame(as.numeric(substr(slo_2014$Windchill, 1, 
                                                              nchar(slo_2014$Windchill)-3)))
    slo_2014[,"Dew_Point"] <- as.data.frame(as.numeric(substr(slo_2014$Dew_Point, 1, 
                                                              nchar(slo_2014$Dew_Point)-3)))      
    slo_2014[,"Pressure"] <- as.data.frame(as.numeric(substr(slo_2014$Pressure, 1, 
                                                             nchar(slo_2014$Pressure)-3)))
    slo_2014[,"Humidity"] <- as.data.frame(as.numeric(substr(slo_2014$Humidity, 1, 
                                                             nchar(slo_2014$Humidity)-1)))
    slo_2014[which(slo_2014$Wind_Speed == 'Calm'),"Wind_Speed"]<-"0.0 mph"
    slo_2014[,"Wind_Speed"] <- as.data.frame(as.numeric(substr(slo_2014$Wind_Speed, 1, 
                                                               nchar(slo_2014$Wind_Speed)-3))) 
# Add NEW factors based on time of day (early morning, mid-morning, early afternoon, late afternoon, evening)
    today = "2015-06-03"
    t1 <-paste(today, ' ', "09:00:00 CDT", sep="")
    t2 <-paste(today, ' ', "12:00:00 CDT", sep="")
    t3 <-paste(today, ' ', "15:00:00 CDT", sep="")
    t4 <-paste(today, ' ', "18:00:00 CDT", sep="")
    t5 <-paste(today, ' ', "21:00:00 CDT", sep="")
    slo_2014[which(slo_2014$Time > t1 & slo_2014$Time < t2),"Day_Period"] <-"Early_Morning"
    slo_2014[which(slo_2014$Time > t2 & slo_2014$Time < t3),"Day_Period"] <-"Early_Afternoon"
    slo_2014[which(slo_2014$Time > t3 & slo_2014$Time < t4),"Day_Period"] <-"Late_Afternoon"
    slo_2014[which(slo_2014$Time > t4 & slo_2014$Time < t5),"Day_Period"] <-"Early_Evening"
    slo_2014$Day_Period <-factor(slo_2014$Day_Period)

# Split by Seasons:
    sp <-"2014-03-20 00:00:00 CDT"
    su <-"2014-06-21 00:00:00 CDT"
    au <-"2014-09-23 00:00:00 CDT"
    wi <-"2014-12-21 00:00:00 CDT"
        slo_2014[which(slo_2014$Full_Date < sp | slo_2014$Full_Date > wi), "Season"] <- "Winter"
        slo_2014[which(slo_2014$Full_Date > sp & slo_2014$Full_Date < su), "Season"] <- "Spring"
        slo_2014[which(slo_2014$Full_Date > su & slo_2014$Full_Date < au), "Season"] <- "Summer"
        slo_2014[which(slo_2014$Full_Date > au & slo_2014$Full_Date < wi), "Season"] <- "Autumn"
    slo_2014$Season <-factor(slo_2014$Season)

    #Ex: Avg visibility:
    mean(slo_2014$Visibility, na.rm=T)
        ggplot(slo_2014, aes(x = Date, y=Visibility)) + 
        geom_smooth(size=0.5, se=F) + ggtitle("Annual Visibility")

        ggplot(slo_2014, aes(x = Time_EST, y=Visibility)) + 
        geom_smooth(size=0.5, se=F) + ggtitle("Daily Visibility")

        noon <- subset(slo_2014, Time_EST == '2015-06-01 11:56:00 CDT')
        ggplot(noon, aes(x = Date, y=Visibility)) + 
        geom_smooth(size=0.5, se=F) + ggtitle("Annual Noon Visibility")


    daytime <-subset(slo_2014, is.na(Day_Period) != 1)
ggplot(daytime, aes(x = Date, y=Visibility, color = Day_Period, group=Day_Period)) + 
geom_smooth(size=1.5, se=T, alpha=0.4) + ggtitle("Annual Visibility by Day Period")

ggplot(daytime, aes(x = Date, y=Temp, color = Day_Period, group=Day_Period)) + 
  geom_smooth(size=1.5, se=T, alpha=0.4) + ggtitle("Annual Temp by Day Period")

ggplot(daytime, aes(x = Time, y=Temp)) + 
  geom_smooth(size=1.5, se=T, alpha=0.4) + ggtitle("Daily Temp - Daytime NA's removed")
ggplot(slo_2014, aes(x = Time, y=Temp)) + 
  geom_smooth(size=1.5, se=T, alpha=0.4) + ggtitle("Daily Temp - Full Dataset") 
ggplot(slo_2014, aes(x = Time, y=Temp, color = Season, group=Season)) + 
  geom_smooth(size=1.5, se=T, alpha=0.4) + ggtitle("Daily Temp - Full Dataset, By Season")

clear_skies <-subset(slo_2014, Conditions=="Clear")
ggplot(clear_skies, aes(x = Time, group=Season, color=Season)) + 
  geom_bar(size=1.5, alpha=0.4, aes(fill=Season)) + ggtitle("Clear Skies by Time") 

ggplot(slo_2014, aes(x = Time, group=Conditions, color=Conditions)) + 
  geom_bar(size=1.5, alpha=0.4, aes(fill=Conditions)) + ggtitle("Condition by Time") 

#Combine factor levels
sort(table(slo_2014$Conditions))
# 1) Clear - 5054, 2) Overcast - 2472, 3) Mostly Cloudy - 1097, 4) Scattered Clouds - 710, 5) Partly Cloudy - 323
# Combine ANY cloud cover into 1 group; combine ANY precip into 1 group

# Weather patterns as a function of time of day and season:

# ---Should the normalization be applied to the whole dataset or for individual seasons???
    norm_slo <- slo_2014
    norm_slo$Temp <-scale(norm_slo$Temp)
    norm_slo$Humidity <-scale(norm_slo$Humidity)
    norm_slo$Pressure <-scale(norm_slo$Pressure)
    norm_slo$Wind_Speed <-scale(norm_slo$Wind_Speed)

    spring <-subset(norm_slo, Season =='Spring')
    summer <-subset(norm_slo, Season =='Summer')
    autumn <-subset(norm_slo, Season =='Autumn')
    winter <-subset(norm_slo, Season =='Winter')

p1 <-ggplot(spring)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = 'Temp')) +
  geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = 'Humidity')) +
  geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = 'Pressure')) +
  geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = 'Wind_Speed'))+
  ggtitle('Spring') + ylim(-2,2)
p2 <-ggplot(summer)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = 'Temp')) +
  geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = 'Humidity')) +
  geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = 'Pressure')) +
  geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = 'Wind_Speed'))+
  ggtitle('Summer') + ylim(-2,2)
p3 <-ggplot(autumn)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = 'Temp')) +
  geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = 'Humidity')) +
  geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = 'Pressure')) +
  geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = 'Wind_Speed'))+
  ggtitle('Autumn') + ylim(-2,2)
 p4 <-ggplot(winter)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = 'Temp')) +
  geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = 'Humidity')) +
  geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = 'Pressure')) +
  geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = 'Wind_Speed'))+
  ggtitle('Winter') + ylim(-2,2)

pdf("Tuscany_weather_by_season_1.pdf", width = 11, height = 8.5)
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

norm2 <- slo_2014
spring <-subset(norm2, Season =='Spring')
        spring$Temp <-scale(spring$Temp)
        spring$Humidity <-scale(spring$Humidity)
        spring$Pressure <-scale(spring$Pressure)
        spring$Wind_Speed <-scale(spring$Wind_Speed)
summer <-subset(norm2, Season =='Summer')
        summer$Temp <-scale(summer$Temp)
        summer$Humidity <-scale(summer$Humidity)
        summer$Pressure <-scale(summer$Pressure)
        summer$Wind_Speed <-scale(summer$Wind_Speed)
autumn <-subset(norm2, Season =='Autumn')
        autumn$Temp <-scale(autumn$Temp)
        autumn$Humidity <-scale(autumn$Humidity)
        autumn$Pressure <-scale(autumn$Pressure)
        autumn$Wind_Speed <-scale(autumn$Wind_Speed)
winter <-subset(norm2, Season =='Winter')
        winter$Temp <-scale(winter$Temp)
        winter$Humidity <-scale(winter$Humidity)
        winter$Pressure <-scale(winter$Pressure)
        winter$Wind_Speed <-scale(winter$Wind_Speed)

p9 <-ggplot(spring)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = 'Temp')) +
  geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = 'Humidity')) +
  geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = 'Pressure')) +
  geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = 'Wind_Speed'))+
  ggtitle('Spring') + ylim(-2,2)
p10 <-ggplot(summer)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = 'Temp')) +
  geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = 'Humidity')) +
  geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = 'Pressure')) +
  geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = 'Wind_Speed'))+
  ggtitle('Summer') + ylim(-2,2)
p11 <-ggplot(autumn)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = 'Temp')) +
  geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = 'Humidity')) +
  geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = 'Pressure')) +
  geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = 'Wind_Speed'))+
  ggtitle('Autumn') + ylim(-2,2)
p12 <-ggplot(winter)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = 'Temp')) +
  geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = 'Humidity')) +
  geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = 'Pressure')) +
  geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = 'Wind_Speed'))+
  ggtitle('Winter') + ylim(-2,2)

pdf("Tuscany_weather_by_season_3.pdf", width = 11, height = 8.5)
multiplot(p9, p10, p11, p12, cols=2)
dev.off()


p5 <-ggplot(slo_2014)+ geom_smooth(aes(x = Time, y = Temp, se=T, alpha=0.3, size=1, color = Season, Group=Season)) +
  ggtitle('Temp') 
p6 <-ggplot(slo_2014)+ geom_smooth(aes(x = Time, y = Humidity, se=T, alpha=0.3, size=1, color = Season, Group=Season)) +
  ggtitle('Humidity')
p7 <-ggplot(slo_2014)+ geom_smooth(aes(x = Time, y = Pressure, se=T, alpha=0.3, size=1, color = Season, Group=Season)) +
  ggtitle('Pressure') 
p8 <-ggplot(slo_2014)+ geom_smooth(aes(x = Time, y = Wind_Speed, se=T, alpha=0.3, size=1, color = Season, Group=Season)) +
  ggtitle('Wind Speed') 

pdf("Tuscany_weather_by_season_2.pdf", width = 11, height = 8.5)
multiplot(p5, p6, p7, p8, cols=2)
dev.off()

ggplot(slo_2014)+ geom_smooth(aes(x = Time, y = Dew_Point, se=T, alpha=0.3, size=1, color = Season, Group=Season)) +
  ggtitle('Dew Point') 



#Goal: What is a typical 'SLO day'? Come up with a metric that allows you to take same weather data
# from other cities and rank them by how many 'SLO days' they have per year
# (SLO should have upwards of 90% SLO days per year...)

# Some ideas:
# Temperature, humidity, pressure range
# 
# 
