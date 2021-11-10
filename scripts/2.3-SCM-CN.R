rm(list = ls())
# Preliminaries
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(usmap)
library(grid)
library(rworldmap)
library(mapproj)
setwd("T:/wd/climate/Data") # set your own working directory
# Load data
scm <- read_excel("T:/wd/climate/Data/data_edb_wto_scm.xlsx")
control_data <- scm

scm$count <- 1
scm <- scm[, c(9, 21, 6, 11)]
colnames(scm) <- c("year", "count", "country", "descr")
scm$eucn <- scm$country #control var for eu member states/china area

scm$eucn <- gsub(".*European.*", "EU", scm$eucn)
scm$eucn <- gsub(".*Macao.*", "China", scm$eucn)
scm$eucn <- gsub(".*Hong Kong.*", "China", scm$eucn)
scm$eucn <- gsub(".*Chinese.*", "China", scm$eucn)

scm$state <- scm$descr #control var for US states/ CN provinces

CN <- subset(scm, eucn %in% "China")
CN <- CN[, c(1, 2, 3, 4, 6)]
CN <- subset(CN, year >= 2018)
CN$state <- gsub(".*excluding.*", "SEZ excluding 1 area", CN$state)
# Municipalities - 4
CN$state <- gsub(".*Beijing.*", "Beijing", CN$state)
CN$state <- gsub(".*Chongqing.*", "Chongqing", CN$state)
CN$state <- gsub(".*Shanghai.*", "Shanghai", CN$state)
CN$state <- gsub(".*Tianjin.*", "Tianjin", CN$state)
# Provinces - 23
CN$state <- gsub(".*Anhui.*", "Anhui", CN$state)
CN$state <- gsub(".*Fujian.*", "Fujian", CN$state)
CN$state <- gsub(".*Gansu.*", "Gansu", CN$state)
CN$state <- gsub(".*Guangdong.*", "Guangdong", CN$state)
CN$state <- gsub(".*Guizhou.*", "Guizhou", CN$state)
CN$state <- gsub(".*Hainan.*", "Hainan", CN$state)
CN$state <- gsub(".*Hebei.*", "Hebei", CN$state)
CN$state <- gsub(".*Heilongjiang.*", "Heilongjiang", CN$state)
CN$state <- gsub(".*Henan.*", "Henan", CN$state)
CN$state <- gsub(".*Hubei.*", "Hubei", CN$state)
CN$state <- gsub(".*Hunan.*", "Hunan", CN$state)
CN$state <- gsub(".*Jiangsu.*", "Jiangsu", CN$state)
CN$state <- gsub(".*Jiangxi.*", "Jiangxi", CN$state)
CN$state <- gsub(".*Jilin.*", "Jilin", CN$state)
CN$state <- gsub(".*Liaoning.*", "Liaoning", CN$state)
CN$state <- gsub(".*Qinghai.*", "Qinghai", CN$state)
CN$state <- gsub(".*Shaanxi.*", "Shaanxi", CN$state)
CN$state <- gsub(".*Shandong.*", "Shandong", CN$state)
CN$state <- gsub(".*Shanxi.*", "Shanxi", CN$state)
CN$state <- gsub(".*Sichuan.*", "Sichuan", CN$state)
CN$state <- gsub(".*Yunnan.*", "Yunnan", CN$state)
CN$state <- gsub(".*Zhejiang.*", "Zhejiang", CN$state)
CN <- mutate(CN, state = ifelse(as.character(CN$country) == "Chinese Taipei", "Taiwan", as.character(CN$state)))
# Autonomous Regions - 5
CN$state <- gsub(".*Guangxi.*", "Guangxi", CN$state)
CN$state <- gsub(".*Inner Mongolia.*", "Inner Mongolia", CN$state)
CN$state <- gsub(".*Ningxia.*", "Ningxia", CN$state)
CN$state <- gsub(".*Tibet.*", "Tibet", CN$state)
CN$state <- gsub(".*Xinjiang.*", "Xinjiang", CN$state)
# Special Administrative Regions - 2
CN <- mutate(CN, state = ifelse(as.character(CN$country) == "Hong Kong, China", "Hong Kong", as.character(CN$state)))
CN <- mutate(CN, state = ifelse(as.character(CN$country) == "Macao, China", "Macao", as.character(CN$state)))

CN <- CN[, c(1, 2, 5, 4)] # annex output
miskaris <- c("Beijing", "Chongqing", "Shanghai", "Tianjin", "Anhui", "Fujian",
           "Gansu", "Guangdong", "Guizhou", "Hainan", "Hebei", "Heilongjiang",
           "Henan", "Hubei", "Hunan", "Jiangsu", "Jiangxi", "Jilin", "Liaoning",
           "Qinghai", "Shaanxi", "Shandong", "Shanxi", "Sichuan", "Yunnan",
           "Zhejiang", "Taiwan", "Guangxi", "Inner Mongolia", "Ningxia",
           "Tibet", "Xinjiang", "Hong Kong", "Macao")
CN2 <- subset(CN, state %in% miskaris) #remove unspecified regions
CN_DATA <- aggregate(count ~ state, data = CN2, FUN = sum, na.rm = TRUE)

# Load map polygons (output from chimap script)
chimap <- read_excel("T:/wd/climate/Data/chimap/chimap_coords.xlsx")

# Matching data to coords
chidata <- data.frame(CN_DATA)
colnames(chidata) <- c("region", "value")
chimap$value <- chidata$value[match(chimap$region,chidata$region)]

# Plotting
colours <- c("white", "#e8e8e8", "#69b3a2", "#2c9c82")
breaks <- c(5, 10)

# Whole China Map 390x290
ggplot() + 
geom_polygon(data = chimap,
               aes(x = long, y = lat, group = group, fill = value),
               colour = "grey50")+
scale_fill_gradientn(colours = colours,
                     na.value = "white",
                     guide = guide_colourbar(barwidth = 0.4, barheight = 12,
                                             title.position = "top")) + 
coord_map()+
theme_void() +
theme(legend.position = "none",
      legend.title=element_blank(), 
      legend.text=element_text(size=8))+
annotate("text", x = 122, y = 34, label = "JS", size = 2.5, colour = "#272928")+
annotate("text", x = 113.70, y = 23.8, label = "GD", size = 2.5, colour = "#272928")+
geom_segment(aes(x = 111.7,y = 21, xend = 111.7, yend = 26), colour = "#272928", linetype = "dashed")+
geom_segment(aes(x = 111.7,y = 26, xend = 115.5, yend = 26), colour = "#272928", linetype = "dashed")+
geom_segment(aes(x = 115.5,y = 21, xend = 115.5, yend = 26), colour = "#272928", linetype = "dashed")+
geom_segment(aes(x = 111.7,y = 21, xend = 115.5, yend = 21), colour = "#272928", linetype = "dashed")+
annotate("text", x = 123.4, y = 41.6, label = "LN", size = 2.5, colour = "#272928")+
annotate("text", x = 126.4, y = 43.7, label = "JL", size = 2.5, colour = "#272928")+
annotate("text", x = 124, y = 38, label = "TJ", size = 2.5, colour = "#272928")+
geom_segment(aes(x = 123,y = 38, xend = 118.07, yend = 38.8), colour = "#272928")
  
# Zoomed (Macao + Hong Kong) 235 x 290
ggplot() +
geom_polygon(data = chimap, aes(x = long, y = lat, group = group, fill = value), colour = "grey50")+
scale_fill_gradientn(colours = colours, 
                     breaks = breaks,
                     na.value = "grey90",
                     guide = guide_colourbar(barwidth = 0.57, barheight = 12,
                                             label.position = "right",
                                             ticks.colour = "#272928",
                                             frame.colour = "grey70",
                                             frame.linewidth = 0.1,
                                             frame.linetype = 1)) + 
coord_map(xlim = c(113.25, 114.40),ylim = c(22, 23))+
theme_void()+
theme(panel.border = element_rect(colour = "grey70", fill = NA),
      legend.position = "right",
      legend.box.margin = margin(0, 0, 0, 25, unit = "pt"),
      legend.title=element_blank(), 
      legend.text=element_text(size=8))+
annotate("text", x = 113.50, y = 22.29, label = "MO", size = 2.5, colour = "#272928")+
annotate("text", x = 114.12, y = 22.45, label = "HK", size = 2.5, colour = "#272928")+
annotate("text", x = 113.70, y = 22.90, label = "GD", size = 2.5, colour = "#272928")
# more at github.com/solowiew