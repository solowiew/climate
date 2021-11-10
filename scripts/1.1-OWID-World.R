rm(list = ls())
# Preliminaries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
# set your own working directory

# Load data
owid_data <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
summary(owid_data)

# Removing aggregated indicators (e.g. "Africa", "Europe", etc.)
data <- owid_data[!(is.na(owid_data$iso_code) | owid_data$iso_code == ""), ]

# Subsetting for aggregated World indicators
world_data <- subset(data, country == "World", select = c("iso_code","country","year",
                                                          "cumulative_co2","co2", "population"))
# Adding 2020 emissions (source: nature.com)
temp <- data.frame("OWID_WRL","World", "2020", "1687029.533", "34109.139", "7794798739")
names(temp) <- c("iso_code","country", "year", "cumulative_co2", "co2", "population")
world_data <- as.data.frame(rbind(world_data, temp))

# Cumulative carbon dioxide emissions (linear scale)
g1 <- ggplot(world_data, aes(x=year, y=as.numeric(cumulative_co2)/1000, group = 1))+
  geom_line(colour = "#69b3a2", size = 1.3)+
  geom_linerange(x = "1970",ymin = 0, ymax = 423.381436, colour = "#727372", linetype = "dashed") +
  geom_linerange(y = 423.381436,xmin = -Inf, xmax = "1970", colour = "#727372", linetype = "dashed") +
  geom_linerange(x = "1920",ymin = 0, ymax = 103.080144, colour = "#727372", linetype = "dashed") +
  geom_linerange(y = 103.080144,xmin = -Inf, xmax = "1920", colour = "#727372", linetype = "dashed") +
  geom_linerange(x = "2020",ymin = 0, ymax = 1687.029533, colour = "#727372", linetype = "dashed") +
  geom_linerange(y = 1687.029533,xmin = -Inf, xmax = "2020", colour = "#727372", linetype = "dashed") +
  theme_tufte()+
  geom_rangeframe()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=9, colour = "#272928", vjust=2),
        plot.tag = element_text(colour = "#272928", vjust = 2),
        plot.margin=unit(c(0.25,0.5,0.25,0.25),"cm"))+
  labs(tag = "(a)")+
  ylab("Cumulative Emissions, CO2 Gt.")+
  scale_x_discrete(breaks = c(1750, 1810, 1865, 1920, 1970, 2020), limits = factor(c(1750:2020)))

# Cumulative carbon dioxide emissions (semilog scale)
g2 <- ggplot(world_data, aes(x=year, y=as.numeric(cumulative_co2)/1000, group = 1))+
  geom_line(colour = "#69b3a2", size = 1.3)+
  geom_segment(aes(x = "1970",y = 1687.029533, xend = "1970", yend = 423.381436), colour = "#727372", linetype = "dashed") +
  geom_segment(aes(x = "1920",y = 1687.029533, xend = "1920", yend = 103.080144), colour = "#727372", linetype = "dashed") +
  geom_segment(aes(x = "1750",y = 0.01, xend = "2020", yend = 1687.029533), colour = "#727372", linetype = "dashed") +
  geom_segment(aes(x = "2020",y = 0.01, xend = "2020", yend = 1687.029533), colour = "#727372", linetype = "dashed") +
  geom_hline(yintercept=1687.029533, colour = "#727372", linetype = "dashed")+
  theme_tufte()+
  geom_rangeframe()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.tag = element_text(colour = "#272928", vjust = 2),
        plot.margin=unit(c(0.25,0.5,0.25,0.25),"cm"))+
  labs(tag = "(b)")+
  scale_x_discrete(breaks = c(1750, 1810, 1865, 1920, 1970, 2020), limits = factor(c(1750:2020)))+
  scale_y_log10(breaks = c(0.1, 1, 10, 100))

# Annual carbon dioxide emissions (linear scale)
g3 <- ggplot(world_data, aes(x=year, y=as.numeric(co2)/1000, group = 1))+
  geom_line(colour = "#69b3a2", size = 1.3)+
  geom_hline(yintercept=36.441388, colour = "#727372", linetype = "dashed")+
  geom_segment(aes(x = "2020", y = 0, xend = "2020", yend = 34.109139), colour = "#727372", linetype = "dashed")+
  geom_segment(aes(x = "1970", y = 0, xend = "1970", yend = 14.826863), colour = "#727372", linetype = "dashed")+
  geom_linerange(y = 14.826863,xmin = -Inf, xmax = "1970", colour = "#727372", linetype = "dashed") +
  geom_segment(aes(x = "1920", y = 0, xend = "1920", yend = 3.513395), colour = "#727372", linetype = "dashed")+
  geom_linerange(y = 3.513395,xmin = -Inf, xmax = "1920", colour = "#727372", linetype = "dashed") +
  theme_tufte()+
  geom_rangeframe()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=9, colour = "#272928", vjust=2),
        plot.tag = element_text(colour = "#272928", vjust = 2),
        plot.margin=unit(c(0.25,0.5,0.25,0.25),"cm"))+
  labs(tag = "(c)")+
  ylab("Yearly Emissions, CO2 Gt.")+
  scale_x_discrete(breaks = c(1750, 1810, 1865, 1920, 1970, 2020), limits = factor(c(1750:2020)))

# Annual carbon dioxide emissions (semilog scale)
g4 <- ggplot(world_data, aes(x=year, y=as.numeric(co2)/1000, group = 1))+
  geom_line(colour = "#69b3a2", size = 1.3)+
  geom_hline(yintercept=36.441388, linetype="dashed", colour = "#727372")+
  geom_segment(aes(x = "2020", y = 0.01, xend = "2020", yend = 34.109139), colour = "#727372", linetype = "dashed")+
  geom_segment(aes(x = "1970", y = 36.441388, xend = "1970", yend = 14.826863), colour = "#727372", linetype = "dashed")+
  geom_segment(aes(x = "1920", y = 36.441388, xend = "1920", yend = 3.513395), colour = "#727372", linetype = "dashed")+
  geom_segment(aes(x = "1750", y = 0.01, xend = "2020", yend = 36.441388), colour = "#727372", linetype = "dashed")+
  theme_tufte()+
  geom_rangeframe()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.tag = element_text(colour = "#272928", vjust = 2),
        plot.margin=unit(c(0.25,0.5,0.25,0.25),"cm"))+
  labs(tag = "(d)")+
  scale_x_discrete(breaks = c(1750, 1810, 1865, 1920, 1970, 2020), limits = factor(c(1750:2020)))+
  scale_y_log10(breaks = c(0.1, 1, 10, 100))

grid.arrange(g1, g2, g3, g4, nrow = 2) # output size = 532 x 450
grid.arrange(g1, g2, nrow = 1) # output size = 266 x 225
grid.arrange(g3, g4, nrow = 1) # output size = 266 x 225
# more at github.com/solowiew
