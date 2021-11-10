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

# Data preparation
scm$count <- 1 # count variable
scm <- scm[, c(9, 21, 6, 11)]
colnames(scm) <- c("year", "count", "country", "descr")
scm$state <- scm$descr #control variable for US states
US <- subset(scm, country %in% "United States of America")
US <- US[, c(1, 2, 4, 6)]

# Identifying states
US$state <- gsub(".*Alabama.*", "Alabama", US$state)
US$state <- gsub(".*Alaska.*", "Alaska", US$state)
US$state <- gsub(".*Arizona.*", "Arizona", US$state)
US$state <- gsub(".*Arkansas.*", "Arkansas", US$state)
US$state <- gsub(".*California.*", "California", US$state)
US$state <- gsub(".*Colorado.*", "Colorado", US$state)
US$state <- gsub(".*Connecticut.*", "Connecticut", US$state)
US$state <- gsub(".*Delaware.*", "Delaware", US$state)
US$state <- gsub(".*Florida.*", "Florida", US$state)
US$state <- gsub(".*Georgia.*", "Georgia", US$state)
US$state <- gsub(".*Hawaii.*", "Hawaii", US$state)
US$state <- gsub(".*Idaho.*", "Idaho", US$state)
US$state <- gsub(".*Illinois.*", "Illinois", US$state)
US$state <- gsub(".*Indiana.*", "Indiana", US$state)
US$state <- gsub(".*Iowa.*", "Iowa", US$state)
US$state <- gsub(".*Kansas.*", "Kansas", US$state)
US$state <- gsub(".*Kentucky.*", "Kentucky", US$state)
US$state <- gsub(".*Louisiana.*", "Louisiana", US$state)
US$state <- gsub(".*Maine.*", "Maine", US$state)
US$state <- gsub(".*Maryland.*", "Maryland", US$state)
US$state <- gsub(".*Massachusetts.*", "Massachusetts", US$state)
US$state <- gsub(".*Massachusett.*", "Massachusetts", US$state)
US$state <- gsub(".*Michigan.*", "Michigan", US$state)
US$state <- gsub(".*Minnesota.*", "Minnesota", US$state)
US$state <- gsub(".*Mississippi.*", "Mississippi", US$state)
US$state <- gsub(".*Missouri.*", "Missouri", US$state)
US$state <- gsub(".*Montana.*", "Montana", US$state)
US$state <- gsub(".*Nebraska.*", "Nebraska", US$state)
US$state <- gsub(".*Nevada.*", "Nevada", US$state)
US$state <- gsub(".*New Hampshire.*", "New Hampshire", US$state)
US$state <- gsub(".*New Jersey.*", "New Jersey", US$state)
US$state <- gsub(".*New Mexico.*", "New Mexico", US$state)
US$state <- gsub(".*New York.*", "New York", US$state)
US$state <- gsub(".*North Carolina.*", "North Carolina", US$state)
US$state <- gsub(".*North Dakota.*", "North Dakota", US$state)
US$state <- gsub(".*Ohio.*", "Ohio", US$state)
US$state <- gsub(".*Oklahoma.*", "Oklahoma", US$state)
US$state <- gsub(".*Oregon.*", "Oregon", US$state)
US$state <- gsub(".*Pennsylvania.*", "Pennsylvania", US$state)
US$state <- gsub(".*Rhode Island.*", "Rhode Island", US$state)
US$state <- gsub(".*South Carolina.*", "South Carolina", US$state)
US$state <- gsub(".*South Dakota.*", "South Dakota", US$state)
US$state <- gsub(".*Tennessee.*", "Tennessee", US$state)
US$state <- gsub(".*Texas.*", "Texas", US$state)
US$state <- gsub(".*Utah.*", "Utah", US$state)
US$state <- gsub(".*Vermont.*", "Vermont", US$state)
US$state <- gsub(".*Virginia.*", "Virginia", US$state)
US$state <- gsub(".*Washington.*", "Washington", US$state)
US$state <- gsub(".*West Virginia.*", "West Virginia", US$state)
US$state <- gsub(".*Wisconsin.*", "Wisconsin", US$state)
US$state <- gsub(".*Wyoming.*", "Wyoming", US$state)

names <- state.name
US <- subset(US, state %in% names) #remove unspecified states
US_DATA <- aggregate(count ~ state, data = US, FUN = sum, na.rm = TRUE)

#Plotting
us_map <- usmap::us_map() # used to add selected states
colours <- c("white", "#e8e8e8", "#69b3a2", "#2c9c82")

#Whole Map - output size 642 x 290
plot_usmap(data = US_DATA, values = "count", colour = "grey50") + 
  scale_fill_gradientn(name = "Ìונ, רע.",
                       colours=colours, na.value="white",
                       guide = guide_colourbar(barwidth = 0.57, barheight = 12,
                                               label.position = "right",
                                               ticks.colour = "#272928",
                                               frame.colour = "grey70",
                                               frame.linewidth = 0.1,
                                               frame.linetype = 1)) + 
  annotate("text", x = 935088.9, y = -459154.6, label = "IL", size = 2.5, colour = "#272928")+
  annotate("text", x = 1356683, y = -686467.1, label = "KY", size = 2.5, colour = "#272928")+
  annotate("text", x = 2551136, y = 185645.43, label = "MA", size = 2.5, colour = "#272928")+
  annotate("text", x = 1238740.1, y = -90867.418, label = "MI", size = 2.5, colour = "#272928")+
  annotate("text", x = 1255540, y = -1226755, label = "AL", size = 2.5, colour = "#272928")+
  annotate("text", x = -1095175.3, y = -1087820.5, label = "AZ", size = 2.5, colour = "#272928")+
  theme(legend.position = "right",
        legend.title=element_blank(),
        legend.text = element_text(size=8, colour = "#272928"),
        legend.box.margin = margin(0, 0, 0, 15, unit = "pt"),
        legend.justification = "center")
# more at github.com/solowiew