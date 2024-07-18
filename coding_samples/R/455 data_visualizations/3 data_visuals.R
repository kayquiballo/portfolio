library(tidyverse)
library(WDI)
library(hrbrthemes)
library(gganimate)

#read in the data
Grammys_Dataset <- read.csv("C:/Users/kjq978/Desktop/MSDS 455/TheShow_Quiballo/Grammys_Dataset.csv")
Grammys_Dataset10 <- Grammys_Dataset[Grammys_Dataset$Year.I. >=2013,]

str(Grammys_Dataset10)
#=============================================

#graphic 1 - race and grammys over time

Year <- c(2013, 2013, 2014, 2014, 2015, 2015, 2016, 2016, 2017, 2017, 2018, 2018, 
          2019, 2019, 2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023)
Race <- c("White", "Non_white", "White", "Non_white", "White", "Non_white", "White", "Non_white", 
          "White", "Non_white", "White", "Non_white", "White", "Non_white", "White", "Non_white", 
          "White", "Non_white", "White", "Non_white", "White", "Non_white")
Race_pct <- c(100, 0, 100, 0, 100, 0, 100, 0, 75, 25, 25, 75, 
              50, 50, 100, 0, 50, 50, 0, 100, 50, 50)
racedata <- data.frame(Year, Race, Race_pct)

group.colors <- c(White = "#959181", Non_white = "#d6bc5a")

ggplot(racedata, aes(x = as.character(Year), y = Race_pct, fill = Race)) +
  geom_col()+
  theme_classic()+
  xlab("Year") + ylab("Race %")+
  ggtitle("Big Four Grammy Award Winners by Race (2013-2023)")+
  theme(plot.title = element_text(hjust = 0.5))+
  #Specify colours
  scale_fill_manual(values=group.colors)

#=============================================

#graphic 2 - Beyonce

exam_scores <- data.frame(
  row.names = c("Nominations", "Awards"), #32 wins, 88 nominations (data shows 29 + 79)
  Album = c(15,7),
  Performance = c(24,11),
  Video = c(8,3),
  Song = c(24,7),
  Collaboration = c(8,1)
)
exam_scores

#nominated	won
#album	15	7
#performance	24	11
#video	8	3
#song	24	7
#collaboration	8	1
#79	29


# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  Album = c(0,25), 
  Performance = c(0,25), 
  Video = c(0,25),
  Song = c(0,25),
  Collaboration = c(0,25)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
df

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c('0', '', '', '', '25'),
  color = c("#959181", "#d6bc5a")
)
# Add an horizontal legend
legend(
  x = "right", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c( "#959181", "#d6bc5a"),
  text.col = "black", cex = 1, pt.cex = 2
)
title("Beyonce Grammy Awards and Nominations")
par(op)


