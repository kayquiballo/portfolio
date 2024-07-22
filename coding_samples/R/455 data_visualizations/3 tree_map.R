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