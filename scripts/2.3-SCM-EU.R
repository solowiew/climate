rm(list = ls())
# Preliminaries
library(ggplot2)
library(grid)
library(rworldmap)
library(mapproj)
library(readxl)
library(tidyverse)
library(gridExtra)
setwd("T:/wd/climate/Data") # set your own working directory
# Load data
scm <- read_excel("T:/wd/climate/Data/data_edb_wto_scm.xlsx")
control_data <- scm

scm$count <- 1
scm <- scm[, c(9, 21, 6, 11)]
colnames(scm) <- c("year", "count", "country", "descr")
scm$eucn <- scm$country #control var for eu member states/china area

scm$eucn <- gsub(".*European.*", "EU", scm$eucn)

EU <- subset(scm, eucn %in% "EU") # EU level + Country = 1483
EU <- separate(EU, country, c("EU", "countryEU"), sep = ":")
# EU DATA
EU <- EU[, c(1, 2, 4, 5)]
EU$countryEU <- gsub(".*Netherlands.*", "Netherlands", EU$countryEU)
EU$countryEU <- gsub(" ", "", EU$countryEU)
EU$countryEU <- gsub(".*CzechRepublic.*", "Czech Rep.", EU$countryEU)
EU$countryEU <- gsub(".*SlovakRepublic.*", "Slovakia", EU$countryEU)
EU$countryEU <- gsub(".*UnitedKingdom.*", "United Kingdom", EU$countryEU)

EU_DATA <- aggregate(count ~ countryEU, data = EU, FUN = sum, na.rm = TRUE)
colnames(EU_DATA) <- c("country", "value") # Country level = 1312

# Plotting
worldMap <- getMap()

# Member States of the European Union
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

#insert data here
europeanUnionTable <- data.frame(EU_DATA)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

# Graph (410 x 290)
colours <- c("white", "#e8e8e8", "#69b3a2", "#2c9c82")

ggplot() + geom_polygon(data = europeCoords, 
               aes(x = long, y = lat, group = region, fill = value),
               colour = "grey50", size = 0.1) +
scale_fill_gradientn(colours=colours, na.value="white",
                     guide = guide_colourbar(barwidth = 0.57, barheight = 12,
                                             label.position = "right",
                                             ticks.colour = "#272928",
                                             frame.colour = "grey70",
                                             frame.linewidth = 0.1,
                                             frame.linetype = 1)) +
annotate("text", x = 8.3, y = 58.1, label = "DK", size = 2.5, colour = "#272928")+
annotate("text", x = 19.3, y = 47.1, label = "HU", size = 2.5, colour = "#272928")+
annotate("text", x = 14.6, y = 47.8, label = "AT", size = 2.5, colour = "#272928")+
annotate("text", x = -7.9, y = 53.2, label = "IE", size = 2.5, colour = "#272928")+
annotate("text", x = 15, y = 62.1, label = "SE", size = 2.5, colour = "#272928")+
annotate("text", x = -1.5, y = 53, label = "UK", size = 2.5, colour = "#272928")+
theme_void()+
  theme(legend.position = "right",
      legend.title=element_blank(),
      legend.text = element_text(size=8, colour = "#272928"),
      legend.box.margin = margin(0, 0, 0, 15, unit = "pt"))

# Annex output
library(writexl)
write_xlsx(EU,"OUTPUT_EU_MAP_SCM.xlsx") 
# more at github.com/solowiew