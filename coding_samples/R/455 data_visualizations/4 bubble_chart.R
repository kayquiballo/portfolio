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

#convert to numerics
engagement_data_detailed2 <- engagement_data_detailed
engagement_data_detailed2$pct_black.hispanic[engagement_data_detailed2$pct_black.hispanic=="[0, 0.2["] <- 0.2
engagement_data_detailed2$pct_black.hispanic[engagement_data_detailed2$pct_black.hispanic=="[0.2, 0.4["] <- 0.4
engagement_data_detailed2$pct_black.hispanic[engagement_data_detailed2$pct_black.hispanic=="[0.4, 0.6["] <- 0.6
engagement_data_detailed2$pct_black.hispanic[engagement_data_detailed2$pct_black.hispanic=="[0.6, 0.8["] <- 0.8
engagement_data_detailed2$pct_black.hispanic[engagement_data_detailed2$pct_black.hispanic=="[0.8, 1["] <- 1

engagement_data_detailed2$pct_free.reduced[engagement_data_detailed2$pct_free.reduced=="[0, 0.2["] <- 0.2
engagement_data_detailed2$pct_free.reduced[engagement_data_detailed2$pct_free.reduced=="[0.2, 0.4["] <- 0.4
engagement_data_detailed2$pct_free.reduced[engagement_data_detailed2$pct_free.reduced=="[0.4, 0.6["] <- 0.6
engagement_data_detailed2$pct_free.reduced[engagement_data_detailed2$pct_free.reduced=="[0.6, 0.8["] <- 0.8
engagement_data_detailed2$pct_free.reduced[engagement_data_detailed2$pct_free.reduced=="[0.8, 1["] <- 1

engagement_data_detailed2$county_connections_ratio[engagement_data_detailed2$county_connections_ratio=="[0.18, 1["] <- 1
engagement_data_detailed2$county_connections_ratio[engagement_data_detailed2$county_connections_ratio=="[1, 2["] <- 2

engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[4000, 6000["] <- 6000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[6000, 8000["] <- 8000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[8000, 10000["] <- 10000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[10000, 12000["] <- 12000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[12000, 14000["] <- 14000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[14000, 16000["] <- 16000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[16000, 18000["] <- 18000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[18000, 20000["] <- 20000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[20000, 22000["] <- 22000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[22000, 24000["] <- 24000
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="[32000, 34000["] <- 34000

engagement_data_detailed2$pct_black.hispanic[engagement_data_detailed2$pct_black.hispanic=="NaN"] <- NA
engagement_data_detailed2$pct_free.reduced[engagement_data_detailed2$pct_free.reduced=="NaN"] <- NA
engagement_data_detailed2$county_connections_ratio[engagement_data_detailed2$county_connections_ratio=="NaN"] <- NA
engagement_data_detailed2$pp_total_raw[engagement_data_detailed2$pp_total_raw=="NaN"] <- NA

engagement_data_detailed2$state[engagement_data_detailed2$state=="NaN"] <- NA
engagement_data_detailed2$locale[engagement_data_detailed2$locale=="NaN"] <- NA
engagement_data_detailed2$Sector.s.[engagement_data_detailed2$Sector.s.=="NaN"] <- NA
engagement_data_detailed2$Primary.Essential.Function[engagement_data_detailed2$Primary.Essential.Function=="NaN"] <- NA

engagement_data_detailed2$pct_black.hispanic <- as.numeric(engagement_data_detailed2$pct_black.hispanic)
engagement_data_detailed2$pct_free.reduced <- as.numeric(engagement_data_detailed2$pct_free.reduced)
engagement_data_detailed2$county_connections_ratio <- as.numeric(engagement_data_detailed2$county_connections_ratio)
engagement_data_detailed2$pp_total_raw <- as.numeric(engagement_data_detailed2$pp_total_raw)
engagement_data_detailed2$Month_Yr <- format(as.Date(engagement_data_detailed2$time), "%Y-%m")

#this code adds in region based on US state
head(engagement_data_detailed2)
engagement_data_detailed2$regions <- sapply(engagement_data_detailed2$state,function(x) names(region.list)[grep(x,region.list)])
engagement_data_detailed2$regions[engagement_data_detailed2$regions=="c(NA, NA, NA, NA)"] <- NA
engagement_data_detailed2$regions[engagement_data_detailed2$regions=="NA"] <- NA
engagement_data_detailed2 <- engagement_data_detailed2 %>% 
  mutate(regions = sapply(regions, toString))
str(engagement_data_detailed2)
head(engagement_data_detailed2)
unique(engagement_data_detailed2$Sector.s.)
unique(engagement_data_detailed2$Primary.Essential.Function)

##### Proportions ####

prop.table(table(engagement_data_detailed2$locale))[
  order(prop.table(table(engagement_data_detailed2$locale)))]
## >>       Town      Rural       City     Suburb 
## >> 0.04339363 0.13271677 0.19628971 0.62759990 

engagement_data_detailed2$regions[engagement_data_detailed2$regions=="NA"] <- NA
engagement_data_detailed2$regions[engagement_data_detailed2$regions==""] <- NA
prop.table(table(engagement_data_detailed2$regions))[
  order(prop.table(table(engagement_data_detailed2$regions)))]
## >>     South      West Northeast   Midwest 
## >> 0.1063601 0.2645638 0.2975084 0.3315677

prop.table(table(engagement_data_detailed2$Month_Yr))[
  order(prop.table(table(engagement_data_detailed2$Month_Yr)))]
## >>    2020-07    2020-06    2020-08    2020-01    2020-02    2020-05    2020-03    2020-04    2020-12 
## >> 0.04754256 0.05422216 0.06861074 0.06951126 0.07431118 0.08604516 0.08872107 0.09337731 0.09628056 
## >>    2020-11    2020-09    2020-10 
## >> 0.10404771 0.10545525 0.11187505 
 

engagement_data_detailed2$Semester <- ifelse(startsWith(engagement_data_detailed2$Month_Yr, "2020-01"), "Winter_Spring", 
                                           ifelse(startsWith(engagement_data_detailed2$Month_Yr, "2020-02"), "Winter_Spring",
                                                  ifelse(startsWith(engagement_data_detailed2$Month_Yr, "2020-03"), "Winter_Spring",
                                                         ifelse(startsWith(engagement_data_detailed2$Month_Yr, "2020-04"), "Winter_Spring",
                                                                ifelse(startsWith(engagement_data_detailed2$Month_Yr, "2020-05"), "Winter_Spring",
                                                                       ifelse(startsWith(engagement_data_detailed2$Month_Yr, "2020-06"), "Summer",
                                                                              ifelse(startsWith(engagement_data_detailed2$Month_Yr, "2020-07"), "Summer",
                                                                                     ifelse(startsWith(engagement_data_detailed2$Month_Yr, "2020-08"), "Summer", "Fall_Winter"))))))))

prop.table(table(engagement_data_detailed2$Semester))[
  order(prop.table(table(engagement_data_detailed2$Semester)))]
## >>       Summer Winter_Spring   Fall_Winter 
## >>    0.1703755     0.4119660     0.4176586 

prop.table(table(engagement_data_detailed2$county_connections_ratio))[
  order(prop.table(table(engagement_data_detailed2$county_connections_ratio)))]
## >>            2            1 
## >> 0.0001009072 0.9998990928


prop.table(table(engagement_data_detailed2$pct_black.hispanic))[
  order(prop.table(table(engagement_data_detailed2$pct_black.hispanic)))]
## >>          1        0.8        0.6        0.4        0.2 
## >> 0.02743079 0.08600495 0.10318886 0.14286119 0.64051421 


prop.table(table(engagement_data_detailed2$pct_free.reduced))[
  order(prop.table(table(engagement_data_detailed2$pct_free.reduced)))]
## >>          1        0.8        0.6        0.4        0.2 
## >> 0.01446345 0.10599112 0.25879999 0.29750457 0.32324087 

prop.table(table(engagement_data_detailed2$pp_total_raw))[
  order(prop.table(table(engagement_data_detailed2$pp_total_raw)))]
## >>        34000        6000       22000       24000       20000       18000        8000       16000 
## >>  0.003085234 0.005563528 0.008465748 0.013037582 0.041126637 0.103984595 0.111175541 0.125482446 
## >>        14000       12000       10000 
## >>  0.150524456 0.184541922 0.253012310 

engagement_data_detailed2$Sector.s.[engagement_data_detailed2$Sector.s.==""] <- NA
prop.table(table(engagement_data_detailed2$Sector.s.))[
  order(prop.table(table(engagement_data_detailed2$Sector.s.)))]
## >>         Higher Ed; Corporate                     Corporate            PreK-12; Higher Ed 
## >>                  0.003509471                   0.005664069                   0.178968696 
## >> PreK-12; Higher Ed; Corporate                       PreK-12 
## >>                   0.385004563                   0.426853201 

engagement_data_detailed2$sector2 <- ifelse(startsWith(engagement_data_detailed2$Sector.s., "PreK-12; Higher Ed"), "PreK-12 (with Higher Ed)", 
                                         ifelse(endsWith(engagement_data_detailed2$Sector.s., "PreK-12"), "PreK-12 (without Higher Ed)", "Other"))

prop.table(table(engagement_data_detailed2$sector2))[
  order(prop.table(table(engagement_data_detailed2$sector2)))]
## >>     Other         PreK-12 (without Higher Ed)    PreK-12 (with Higher Ed) 
## >>        0.00917354                  0.42685320                  0.56397326 



engagement_data_detailed2$Primary.Essential.Function[engagement_data_detailed2$Primary.Essential.Function==""] <- NA
prop.table(table(engagement_data_detailed2$Primary.Essential.Function))[
  order(prop.table(table(engagement_data_detailed2$Primary.Essential.Function)))]
# Digital Learning Platforms
# Sites, Resources & Reference
# Content Creation & Curation
# Study Tools
# Other

#engagement_data_detailed2$pef2 <- ifelse(grepl("Digital Learning Platforms", engagement_data_detailed2$Primary.Essential.Function), "Digital Learning Platforms", 
#                                         ifelse(grepl("Sites, Resources & Reference", engagement_data_detailed2$Primary.Essential.Function), "Sites, Resources & Reference", 
#                                                ifelse(grepl("Content Creation & Curation", engagement_data_detailed2$Primary.Essential.Function), "Content Creation & Curation", 
#                                                       ifelse(grepl("Study Tools", engagement_data_detailed2$Primary.Essential.Function), "Study Tools", "Other"))))
engagement_data_detailed2$pef2 <- ifelse(startsWith(engagement_data_detailed2$Primary.Essential.Function, "LC"), "Learning & Curriculum", 
                                         ifelse(startsWith(engagement_data_detailed2$Primary.Essential.Function, "CM"), "Classroom Management", 
                                                ifelse(startsWith(engagement_data_detailed2$Primary.Essential.Function, "SDO"), "School & District Operations", "Other")))

prop.table(table(engagement_data_detailed2$pef2))[
  order(prop.table(table(engagement_data_detailed2$pef2)))]
## >> School & District Operations         Classroom Management        Learning & Curriculum 
## >> 0.07662102                   0.08306320                   0.84031577 
 


##### Models ####
str(engagement_data_detailed2)

glm1 <- glm(data=engagement_data_detailed2,
    log_engagement_index ~ relevel(as.factor(locale), ref = "Suburb") +
      relevel(as.factor(regions), ref = "Midwest") + 
      relevel(as.factor(pef2), ref = "Learning & Curriculum") +
      relevel(as.factor(sector2), ref = "PreK-12 (with Higher Ed)") +
      relevel(as.factor(Semester), ref = "Winter_Spring") +
      pct_black.hispanic + pct_free.reduced + pp_total_raw,
    family="gaussian")

#no cc ratio bc imbalanced, modded sector and function
options(scipen = 999)
#options(scipen = 0)
summary(glm1)

# WEEK 4 455:
# install.packages("ggplot2")
# load package and data
options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source


subset1 <- engagement_data_detailed2[sample(nrow(engagement_data_detailed2), 22243691), ]
subset2 <- engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_black.hispanic) & !is.na(engagement_data_detailed2$pct_free.reduced) & 
                     !is.na(engagement_data_detailed2$log_engagement_index),] %>%
  group_by(pct_black.hispanic, pct_free.reduced) %>%
  summarize(median_log_engagement =  median(log_engagement_index))

p <- ggplot(subset2, aes(pct_black.hispanic, pct_free.reduced))
p + geom_point(aes(size=median_log_engagement**2)) + scale_size_continuous(range = c(1, 15))

