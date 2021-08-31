rm(list = ls())
# Preliminaries
library(readr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/Home/Desktop/workdir") # set your own working directory
library(readxl)
scm <- read_excel("C:/Users/Home/Desktop/WTO_EDB_SCM.xlsx")
control_data <- scm

scm$count <- 1
scm <- scm[, c(9, 21, 6, 8, 2, 20)]
colnames(scm) <- c("year", "count", "country", "status", "agreement", "sector")

# Country x Count
scmCC <- aggregate(count ~ year + country, data = scm, FUN = sum, na.rm = TRUE)
scmCC$country <- gsub(".*European.*", "EU", scmCC$country)
# CxC 2015 - 2019
scmCC1519 <- subset(scmCC, year >= 2015)
CC1519 <- aggregate(count ~ country, data = scmCC1519, FUN = sum, na.rm = TRUE)
CC1519 <- top_n(CC1519, 5, wt = count) #top5 >2015
CC1519$year <- 1

# CxC 2010 - 2014
scmCC1014 <- subset(scmCC, year >= 2010 & year < 2015)
CC1014 <- aggregate(count ~ country, data = scmCC1014, FUN = sum, na.rm = TRUE)
CC1014 <- top_n(CC1014, 5, wt = count) #top5 >2015
CC1014$year <- 0

ccdata <- as.data.frame(rbind(CC1014, CC1519)) # graph 1 data

rm(scmCC, scmCC1014, scmCC1519, CC1014, CC1519)

# Sector x Count
scmSC <- aggregate(count ~ year + sector, data = scm, FUN = sum, na.rm = TRUE)
scmSC$sector <- gsub(".*All products.*", "All products", scmSC$sector)
scmSC$sector <- gsub(".*;.*", "Multiple", scmSC$sector)
# SxC 2015 - 2019
scmSC1519 <- subset(scmSC, year >= 2015)
measures1 <- sum(scmSC1519$count)
SC1519 <- aggregate(count ~ sector, data = scmSC1519, FUN = sum, na.rm = TRUE)
SC1519 <- top_n(SC1519, 5, wt = count) #top5 >2015
SC1519$year <- 1
# SxC 2010 - 2014
scmSC1014 <- subset(scmSC, year >= 2010 & year < 2015)
measures0 <- sum(scmSC1014$count)
SC1014 <- aggregate(count ~ sector, data = scmSC1014, FUN = sum, na.rm = TRUE)
SC1014 <- subset(SC1014, sector %in% c("All products", "Energy", "Fisheries", "Manufacturing", "Multiple")) #top5 >2015
SC1014$year <- 0

scdata <- as.data.frame(rbind(SC1014, SC1519)) # graph 2 data

rm(scmSC, scmSC1014, scmSC1519,SC1014,SC1519)

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
        legend.position=c(.70,.17),
        legend.text = element_text(size=9, colour = "#272928"),
        legend.key.size = unit(0.40, 'cm'),
        plot.tag = element_text(colour = "#272928"),
        plot.tag.position = "top")+
  scale_fill_manual(values = c("#69b3a2", "#d1d1d1"),
                    breaks=c(1, 0),
                    labels=c("2015 - 2019", "2010 - 2014"))+
  labs(tag = "(а)")+
  scale_x_discrete(limits=c("Australia","Canada","China","United States of America","EU"),
                   labels=c("Австралия","Канада","Китай","США","ЕС"))+
  scale_y_discrete(limits = factor(c(0:1010)))

g2 <- ggplot(data=scdata, aes(x=sector,y=count,fill=factor(year))) +
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
                    labels=c("2015 - 2019 гг.", "2010 - 2014 гг."))+
  labs(tag = "(б)")+
  scale_x_discrete(limits=c("All products","Fisheries","Multiple","Manufacturing","Energy"),
                   labels=c("Все секторы","Рыболовство","Ряд секторов","Промышленность\n (производство)","Энегретика"))+
  scale_y_discrete(limits = factor(c(0:590)))

grid.arrange(g1, g2, nrow = 1) # output size 642 x 280
# more at github.com/solowiew