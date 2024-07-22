library(tidyverse)
library(WDI)
library(hrbrthemes)
library(gganimate)
library(fmsb)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

#read in the data
districts_info = read.csv("districts_info.csv")
engagement_data = read.csv("engagement_data.csv")
products_info = read.csv("products_info.csv")
names(products_info)[names(products_info) == "LP.ID"] <- "lp_id"

#engagement_data_detailed
engagement_data_detailed <- 
  merge(merge(engagement_data, districts_info, by = "district_id", all.x=TRUE), 
        products_info, by = "lp_id", all.x=TRUE)
engagement_data_detailed$time <- as.Date(engagement_data_detailed$time)
engagement_data_detailed$log_engagement_index <- log(engagement_data_detailed$engagement_index)
sapply(engagement_data_detailed, class)
head(engagement_data_detailed)
#=============================================
gc()
memory.limit()
memory.limit(size=18000)

NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)

#this code adds in region based on US state
engagement_data_detailed$regions <- sapply(engagement_data_detailed$state, 
                     function(x) names(region.list)[grep(x,region.list)])

############# TREE MAP!! ###################

library(treemap)

data <- engagement_data_detailed[engagement_data_detailed$locale!="NaN" & engagement_data_detailed$regions!="character(0)",] %>% 
  group_by(locale, regions) %>% 
  mutate(regions = sapply(regions, toString)) %>%
  summarise(n = n())

str(data)
head(data, 10)

treemap(dtf = data,
        index = c("regions", "locale"),
        vSize = "n",
        vColor = "regions",
        title= "Region by Locale",
        title.legend = "Region",
        palette = "Accent",
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F)                        # If true, labels are bigger when rectangle is bigger.)

treemap(dtf = data,
        index = c("locale", "regions"),
        vSize = "n",
        vColor = "locale",
        title= "Locale by Regions",
        title.legend = "Region",
        palette = "Set2",
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F)



## 11/6/23 - # library

subset2 <- engagement_data_detailed2[!is.na(engagement_data_detailed2$state) &
                                       !is.na(engagement_data_detailed2$log_engagement_index),] %>%
  group_by(state) %>%
  summarize(median_log_engagement =  median(log_engagement_index))


states_latlong = read.csv("states_latlong.csv")
str(states_latlong)
colnames(states_latlong)[colnames(states_latlong) == "ï..states"] ="state"

subset3 <- merge(x=subset2, y=states_latlong, by = "state", all.x = TRUE)



# virids package for the color palette
library(viridis)

# Libraries
library(ggplot2)
library(dplyr)

# Get the world polygon and extract UK
library(maps)
USA <- map_data("world") %>% filter(region=="USA")


#The second step is to load a data frame with the info of the bubble you want to draw. The maps library provides a list of the biggest cities in the world. Let's use it to get information on the UK.

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data <- world.cities %>% filter(country.etc=="UK")

# Left chart
ggplot() +
  geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=subset3, aes(x=Lon, y=Lat)) +
  theme_void() + ylim(25,50) + xlim(-125,-70) + coord_map() 



#FOR MORE PRETTY ONE

# Create breaks for the color scale
mybreaks <- c(-0.5, 0, .5, 1, 2)


# Build the map
ggplot() +
  geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=subset3, aes(x=Lon, y=Lat, size=median_log_engagement, color=median_log_engagement, alpha=median_log_engagement), shape=20, stroke=FALSE) +
  scale_size_continuous(trans="log", range=c(1,40), breaks=mybreaks) +
  scale_alpha_continuous(trans="log", range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="mako", trans="log", breaks=mybreaks ) +
  theme_void() + ylim(25,50) + xlim(-125,-70) + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("Student Engagement with Online Learning in 2020") +
  theme(
    legend.position = c(0.5, 0.5),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
