rm(list = ls())
# Preliminaries
library(readr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/Home/Desktop/workdir") # set your own working directory
library(readxl)
edb <- read_delim("C:/Users/Home/Desktop/edb_data.csv", ";", escape_double = FALSE, trim_ws = TRUE)
control_data <- edb
edbttl <- edb 

# Graph 1 dataset
edb$others <- rowSums(select(edb, `Quantitative Restrictions`:`Anti-Dumping`), na.rm = TRUE)
edb <- edb[, c(1, 2, 3, 4, 5 ,6, 23)]
colnames(edb) <- c("year", "tbt", "sps", "scm", "aoa", "ilp", "other")
edb <- gather(edb, key = "agreement", value = "notifications", 2:7)
edb2 <- edb  %>%
  group_by(year, agreement) %>%
  summarise(n = sum(notifications)) %>%
  mutate(percentage = 100*(n / sum(n)))
edb2$agreement <- factor(edb2$agreement , levels=c("other", "ilp", "scm", "aoa", "sps", "tbt") )

# Graph 2 dataset
edbttl$total <- rowSums(select(edbttl, `ТБТ`:`Anti-Dumping`), na.rm = TRUE)
edbttl <- edbttl[, c(1, 23)]
colnames(edbttl) <- c("year", "notifications")

# Graphs
g1 <- ggplot(edbttl, aes(x = year, y = notifications)) +
  geom_area(stat="identity", alpha = 0.6, fill = "#434544") +
  theme_minimal()+
  theme(panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=10, colour = "#272928", vjust=2),
        axis.title.y.right = element_blank(),
        plot.tag = element_text(colour = "#272928", vjust = 1),
        legend.position="none")+
  labs(tag = "(а)")+
  ylab("Уведомлений, шт.")+
  scale_x_discrete(breaks = c(1997, 2002, 2008, 2014, 2019), limits = c(1997:2019))

g2 <- ggplot(edb2, aes(x=year, y=percentage, fill=agreement)) + 
  geom_area(alpha = 0.6, stat="identity") +
  theme_minimal()+
  theme(panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=10, colour = "#272928", vjust=2),
        axis.title.y.right = element_blank(),
        plot.tag = element_text(colour = "#272928", vjust = 1),
        legend.position="none")+
  labs(tag = "(б)")+
  annotate("text", x = 2011, y = 45, label = "ТБТ", size = 2.5, colour = "#272928") +
  annotate("text", x = 2005, y = 57, label = "СФС", size = 2.5, colour = "#272928") +
  annotate("text", x = 2000, y = 59, label = "ССХ", size = 2.5, colour = "#272928") +
  annotate("text", x = 1999, y = 71.5, label = "СКМ", size = 2.5, colour = "#272928") +
  annotate("text", x = 2016, y = 92.5, label = "ИЛП", size = 2.5, colour = "#272928") +
  annotate("text", x = 1999.5, y = 96, label = "Прочие", size = 2.5, colour = "#272928") +
  ylab("Структура уведомлений, %")+
  scale_fill_manual(values = c("#ededed","#f54949","#9672b3","#f0d662","#6b8c76","#434544"))+
  scale_x_discrete(breaks = c(1997, 2002, 2008, 2014, 2019), limits = c(1997:2019))

grid.arrange(g1, g2, nrow = 1) # output size 642 x 280
# more at github.com/solowiew