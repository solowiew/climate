rm(list = ls())
# Preliminaries
library(readr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/Home/Desktop/workdir") # set your own working directory
library(readxl)
aoa <- read_excel("C:/Users/Home/Desktop/WTO_EDB_AOA.xlsx")
control_data <- aoa

aoa$count <- 1
aoa <- aoa[, c(9, 21, 6, 8, 2, 20)]
colnames(aoa) <- c("year", "count", "country", "status", "agreement", "sector")

# Country x Count
aoaCC <- aggregate(count ~ year + country, data = aoa, FUN = sum, na.rm = TRUE)
aoaCC$country <- gsub(".*European.*", "EU", aoaCC$country)
# CxC 2015 - 2020
aoaCC1520 <- subset(aoaCC, year >= 2015)
CC1520 <- aggregate(count ~ country, data = aoaCC1520, FUN = sum, na.rm = TRUE)
CC1520 <- top_n(CC1520, 5, wt = count) #top5 >2015
CC1520$year <- 1

# CxC 2009 - 2014
aoaCC0914 <- subset(aoaCC, year >= 2009 & year < 2015)
CC0914 <- aggregate(count ~ country, data = aoaCC0914, FUN = sum, na.rm = TRUE)
CC0914 <- subset(CC0914, country %in% c("Australia", "Costa Rica", "EU", "Peru", "United States of America")) #top5 >2015
CC0914$year <- 0

ccdata <- as.data.frame(rbind(CC0914, CC1520)) # graph 1 data

rm(aoaCC, aoaCC0914, aoaCC1520, CC0914, CC1520)

# Sector x Count
aoaSC <- aggregate(count ~ year + sector, data = aoa, FUN = sum, na.rm = TRUE)
aoaSC$sector <- gsub(".*All products.*", "All products", aoaSC$sector)
aoaSC$sector <- gsub(".*;.*", "Multiple", aoaSC$sector)
# SxC 2015 - 2019
aoaSC1520 <- subset(aoaSC, year >= 2015)
measures1 <- sum(aoaSC1520$count)
SC1520 <- aggregate(count ~ sector, data = aoaSC1520, FUN = sum, na.rm = TRUE)
SC1520 <- top_n(SC1520, 5, wt = count) #top5 >2015
SC1520$year <- 1
# SxC 2009 - 2014
aoaSC0914 <- subset(aoaSC, year >= 2009 & year < 2015)
measures0 <- sum(aoaSC0914$count)
SC0914 <- aggregate(count ~ sector, data = aoaSC0914, FUN = sum, na.rm = TRUE)
SC0914 <- subset(SC0914, sector %in% c("Agriculture", "Fisheries", "Multiple", "Not specified", "Services")) #top5 >2015
SC0914$year <- 0

scdata <- as.data.frame(rbind(SC0914, SC1520)) # graph 2 data

rm(aoaSC, aoaSC0914, aoaSC1520,SC0914,SC1520)

# Graphs
g1 <- ggplot(data=ccdata, aes(x=country,y=count,fill=factor(year))) +
  geom_bar(position="dodge",stat="identity", alpha = 0.7)+
  geom_text(aes(label=count),position = position_dodge(width = 1), hjust = -0.25, size = 3, colour = "#272928")+
  coord_flip()+
  theme_tufte()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_blank(),
        legend.position= "none",
        plot.tag = element_text(colour = "#272928"),
        plot.tag.position = "top")+
  scale_fill_manual(values = c("#69b3a2", "#d1d1d1"),
                    breaks=c(1, 0),
                    labels=c("2015 - 2020", "2009 - 2014"))+
  labs(tag = "(а)")+
  scale_x_discrete(limits=c("EU","Costa Rica","Peru","United States of America","Australia"),
                   labels=c("ЕС","Коста Рика","Перу","США","Австралия"))+
  scale_y_discrete(limits = factor(c(0:320)))

g2 <- ggplot(data=scdata, aes(x=sector,y=count,fill=factor(year))) +
  geom_bar(position="dodge",stat="identity", alpha = 0.7)+
  geom_text(aes(label=count),position = position_dodge(width = 1), hjust = -0.25, size = 3, colour = "#272928")+
  coord_flip()+
  theme_tufte()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_blank(),
        legend.position=c(.70,.17),
        legend.text = element_text(size=9, colour = "#272928"),
        legend.key.size = unit(0.40, 'cm'),
        plot.tag = element_text(colour = "#272928"),
        plot.tag.position = "top")+
  scale_fill_manual(values = c("#69b3a2", "#d1d1d1"),
                    breaks=c(1, 0),
                    labels=c("2015 - 2020", "2009 - 2014"))+
  labs(tag = "(б)")+
  scale_x_discrete(limits=c("Fisheries","Not specified","Services","Multiple","Agriculture"),
                   labels=c("Рыболовство","Не указан","Услуги","Ряд секторов","Сельское\n хозяйство"))+
  scale_y_discrete(limits = factor(c(0:1100)))

grid.arrange(g1, g2, nrow = 1) # output size 642 x 280
# more at github.com/solowiew