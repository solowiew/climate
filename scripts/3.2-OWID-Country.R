rm(list = ls())
# Preliminaries
library(tidyverse)
library(ggplot2)
library(countrycode) #optional
library(gridExtra)
library(ggthemes)
# SET YOUR WORKING DIRECTORY HERE #
# Load data
owid_data <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
summary(owid_data)

# Removing aggregated indicators (e.g. "Africa", "Europe", etc.)
data <- owid_data[!(is.na(owid_data$iso_code) | owid_data$iso_code == ""), ]
data[is.na(x = data)] <- 0
# Subset country level indicators
country_data <- subset(data, country != "World",
                       select = c("iso_code","country","year",
                                  "cumulative_co2","co2","population",
                                  "gdp", "primary_energy_consumption"))
# Country level graph set
country_data$country_rus <- countrycode(country_data$country,
                                        origin = 'country.name.en', 
                                        destination = 'un.name.ru')
country_data <- subset(country_data, year >= 1900, 
                       select = c("iso_code","country","year","co2",
                                  "population", "gdp", "country_rus"))
country_data <- country_data %>% 
  group_by(country) %>% mutate(cumsum_co2 = cumsum(co2))
country_data_graph <- subset(country_data, year >= 2019)

# Annual carbon dioxide emissions, 2019 (310x280)
g1 <- ggplot(country_data_graph, aes(x=population/1000000000, y=co2/1000)) + 
  geom_point(colour="#272928", fill="#475566", shape=21, 
             alpha=0.3, size=3.8, stroke = 1) +
  geom_point(aes(x = 1.433784064, y = 10.174681), colour="#272928",
             fill="#69b3a2",shape=21, alpha=0.5, size=3.8, stroke = 1)+
  geom_point(aes(x = 0.082913893, y = 0.779527), colour="#272928",
             fill="#69b3a2",shape=21, alpha=0.5, size=3.8, stroke = 1)+
  geom_point(aes(x = 0.145872000, y = 1.678367), colour="#272928",
             fill="#69b3a2",shape=21, alpha=0.5, size=3.8, stroke = 1)+
  geom_point(aes(x = 1.366418048, y = 2.616449), colour="#272928",
             fill="#69b3a2",shape=21, alpha=0.5, size=3.8, stroke = 1)+
  geom_point(aes(x = 0.126860299, y = 1.106664), colour="#272928",
             fill="#69b3a2",shape=21, alpha=0.5, size=3.8, stroke = 1)+
  geom_point(aes(x = 0.329064992, y = 5.284697), colour="#272928",
             fill="#69b3a2",shape=21, alpha=0.5, size=3.8, stroke = 1)+
  theme_tufte()+
  theme(axis.line = element_line(colour = "#272928"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=8, colour = "#272928"),
        axis.title.y = element_text(size=8, colour = "#272928", vjust=2),
        plot.tag = element_text(colour = "#272928", vjust = 2))+
  labs(tag = "(a)") +
  annotate("text", x = 1.433784064, y = 10.174681, label = "�����", size = 3,
           hjust=+1.3, vjust=0.25) +
  annotate("text", x = 0.082913893, y = 0.779527, label = "����", size = 3,
           hjust=+0.5, vjust=-3)+
  geom_segment(aes(x = 0.082913893,y = 0.779527, xend = 0.053517000, yend = 2), 
               colour = "#272928")+
  annotate("text", x = 0.145872000, y = 1.678367, label = "������", size = 3,
           hjust=-0.30, vjust=-0.1)+
  annotate("text", x = 1.366418048, y = 2.616449, label = "�����", size = 3,
           hjust=+1.3, vjust=0.25)+
  annotate("text", x = 0.126860299, y = 1.106664, label = "������", size = 3,
           hjust=-0.25, vjust=0.15)+
  annotate("text", x = 0.329064992, y = 5.284697, label = "���", size = 3,
           hjust=-0.5, vjust=0.25)+
  xlab("��������� 2019 �., ����.")+
  ylab("������� 2019 �., ��.CO2")

# Cumulative carbon dioxide emissions, 1900 - 2019 (310x280)
g2 <- ggplot(country_data_graph, aes(x=population/1000000000, y=cumsum_co2/1000)) + 
  geom_point(colour="#272928", fill="#475566", shape=21, 
             alpha=0.3, size=3.8, stroke = 1) +
  geom_point(aes(x = 1.433784064, y = 219.98577), colour="#272928",
             fill="#69b3a2", shape=21, alpha=1, size=3.8, stroke = 1)+
  geom_point(aes(x = 0.083517000, y = 85.91523), colour="#272928",
             fill="#69b3a2", shape=21, alpha=1, size=3.8, stroke = 1)+
  geom_point(aes(x = 0.145872000, y = 113.45257), colour="#272928",
             fill="#69b3a2", shape=21, alpha=1, size=3.8, stroke = 1)+
  geom_point(aes(x = 1.366418048, y = 51.83377), colour="#272928",
             fill="#69b3a2", shape=21, alpha=1, size=3.8, stroke = 1)+
  geom_point(aes(x = 0.126860299, y = 64.42432), colour="#272928",
             fill="#69b3a2", shape=21, alpha=1, size=3.8, stroke = 1)+
  geom_point(aes(x = 0.329064992, y = 400.23657), colour="#272928",
             fill="#69b3a2",shape=21, alpha=1, size=3.8, stroke = 1)+
  theme_tufte()+
  theme(axis.line = element_line(colour = "#272928"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=8, colour = "#272928"),
        axis.title.y = element_text(size=8, colour = "#272928", vjust=2),
        plot.tag = element_text(colour = "#272928", vjust = 2))+
  labs(tag = "(�)")+
  annotate("text", x = 1.433784064, y = 219.98577, label = "�����", size = 3,
           hjust=+1.3, vjust=0.25) +
  annotate("text", x = 0.083517000, y = 85.91523, label = "��������", size = 3,
           hjust=-0.25, vjust=0.25)+
  annotate("text", x = 0.145872000, y = 113.45257, label = "������", size = 3,
           hjust=-0.30, vjust=0.25)+
  annotate("text", x = 1.366418048, y = 51.83377, label = "�����", size = 3,
           hjust=+1.3, vjust=0.25)+
  annotate("text", x = 0.126860299, y = 64.42432, label = "������", size = 3,
           hjust=-0.24, vjust=0.35)+
  annotate("text", x = 0.329064992, y = 400.23657, label = "���", size = 3,
           hjust=-0.5, vjust=0.25)+
  xlab("��������� 2019 �., ����.")+
  ylab("��������� ������� 1900 - 2019 ��., ��.CO2")

grid.arrange(g1, g2, nrow = 1) # output size = 642x285

# Annex output
prilozhenie <- subset(country_data, country %in% c("United States", "China"),
                      select = c("year", "country", "co2", "cumsum_co2"))
prilozhenie <- pivot_wider(prilozhenie, names_from = "country",
                           values_from = c("co2", "cumsum_co2"))
prilozhenie <- subset(prilozhenie, year >= 2000)
# more at github.com/solowiew