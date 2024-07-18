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

# setup: 4 overlayed spidercharts (top state, locale, Sector.s., Primary.Essential.Function)
# across 4 categories (pct_black.hispanic, pct_free.reduced, county_connections_ratio, pp_total_raw)

######### overall histograms
barplot(prop.table(table(engagement_data_detailed$pct_black.hispanic)))
barplot(prop.table(table(engagement_data_detailed$pct_free.reduced)))
barplot(prop.table(table(engagement_data_detailed$county_connections_ratio)))
barplot(prop.table(table(engagement_data_detailed$pp_total_raw)))

barplot(prop.table(table(engagement_data_detailed$state)))
prop.table(table(engagement_data_detailed$state))[order(prop.table(table(engagement_data_detailed$state)))]
#top values are: Connecticut Utah Illinois Massachusetts
barplot(prop.table(table(engagement_data_detailed$locale)))
barplot(prop.table(table(engagement_data_detailed$Sector.s.)))
barplot(prop.table(table(engagement_data_detailed$Primary.Essential.Function)))
prop.table(table(engagement_data_detailed$Primary.Essential.Function))[order(prop.table(table(engagement_data_detailed$Primary.Essential.Function)))]
# top values: LC - Content Creation & Curation 
#LC - Sites, Resources & Reference 
#LC - Digital Learning Platforms 

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


#################State (Connecticut Utah Illinois Massachusetts)

#### SCORES
aggregate(pct_black.hispanic ~ state, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_black.hispanic) & 
                                           (engagement_data_detailed2$state == "Connecticut" |
                                              engagement_data_detailed2$state == "Utah" | 
                                              engagement_data_detailed2$state == "Illinois" | 
                                              engagement_data_detailed2$state == "Massachusetts"),], mean)
aggregate(pct_free.reduced ~ state, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_free.reduced) & 
                                           (engagement_data_detailed2$state == "Connecticut" |
                                              engagement_data_detailed2$state == "Utah" | 
                                              engagement_data_detailed2$state == "Illinois" | 
                                              engagement_data_detailed2$state == "Massachusetts"),], mean)
aggregate(county_connections_ratio ~ state, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$county_connections_ratio) & 
                                           (engagement_data_detailed2$state == "Connecticut" |
                                              engagement_data_detailed2$state == "Utah" | 
                                              engagement_data_detailed2$state == "Illinois" | 
                                              engagement_data_detailed2$state == "Massachusetts"),], mean)
aggregate(pp_total_raw ~ state, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pp_total_raw) & 
                                           (engagement_data_detailed2$state == "Connecticut" |
                                              engagement_data_detailed2$state == "Utah" | 
                                              engagement_data_detailed2$state == "Illinois" | 
                                              engagement_data_detailed2$state == "Massachusetts"),], mean)

exam_scores <- data.frame(
  row.names = c("Connecticut", "Utah", "Illinois", "Massachusetts"),
  Black_Hispanic = c(0.3416448, 0.2361408, 0.4202572, 0.2321382),
  Free_Lunch = c(0.3638055, 0.4218824, 0.4234538, NA),
  Expenditures = c(1, 1, 1, 1),
  Internet = c(NA, 9169.372, 14478.150, 17738.649)
)
exam_scores

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  Black_Hispanic = c(0.2, 0.45), 
  Free_Lunch = c(0.3, 0.45), 
  Expenditures = c(0.5, 1.5),
  Internet = c(8000, 18000)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
df

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c('', '', '', '', ''),
  color = c("#264653", "#2A9D8F", "#E9C46A", "#E76F51")
)
# Add an horizontal legend
legend(
  x = "right", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#264653", "#2A9D8F", "#E9C46A", "#E76F51"),
  text.col = "black", cex = 1, pt.cex = 2
)
title("by State")
par(op)

#################locale


#### SCORES
aggregate(pct_black.hispanic ~ locale, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_black.hispanic) & 
                                           !is.na(engagement_data_detailed2$locale),], mean)
aggregate(pct_free.reduced ~ locale, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_free.reduced) & 
                                           !is.na(engagement_data_detailed2$locale),], mean)
aggregate(county_connections_ratio ~ locale, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$county_connections_ratio) & 
                                           !is.na(engagement_data_detailed2$locale),], mean)
aggregate(pp_total_raw ~ locale, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pp_total_raw) & 
                                           !is.na(engagement_data_detailed2$locale),], mean)
exam_scores <- data.frame(
  row.names = c("City", "Rural", "Suburb", "Town"),
  Black_Hispanic = c(0.5409786, 0.2228237, 0.3166404, 0.2053514),
  Free_Lunch = c(0.5933392, 0.4063298, 0.3849387, 0.5420526),
  Expenditures = c(1, 1.000753, 1, 1),
  Internet = c(12778.70, 14011.11, 13187.32, 10694.58))

max_min <- data.frame(
  Black_Hispanic = c(0.2, 0.55), 
  Free_Lunch = c(0.35, 0.6), 
  Expenditures = c(0.9990, 1.0010),
  Internet = c(10000, 14500))
rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, exam_scores)
df

op <- par(mar = c(1, 2, 2, 2))
create_beautiful_radarchart(
  data = df, caxislabels = c('', '', '', '', ''),
  color = c("#264653", "#E76F51", "#E9C46A", "#2A9D8F"))
legend(
  x = "right", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#264653", "#E76F51", "#E9C46A", "#2A9D8F"),
  text.col = "black", cex = 1, pt.cex = 2)
title("by Locale")
par(op)

#################Sector.s.

#### SCORES
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_black.hispanic) 
                               & grepl("PreK-12", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pct_black.hispanic)
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_black.hispanic) 
                               & grepl("Higher Ed", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pct_black.hispanic)
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_black.hispanic) 
                               & grepl("Corporate", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pct_black.hispanic)

mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_free.reduced) 
                               & grepl("PreK-12", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pct_free.reduced)
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_free.reduced) 
                               & grepl("Higher Ed", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pct_free.reduced)
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_free.reduced) 
                               & grepl("Corporate", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pct_free.reduced)

mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$county_connections_ratio) 
                               & grepl("PreK-12", engagement_data_detailed2$Sector.s., fixed = TRUE),]$county_connections_ratio)
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$county_connections_ratio) 
                               & grepl("Higher Ed", engagement_data_detailed2$Sector.s., fixed = TRUE),]$county_connections_ratio)
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$county_connections_ratio) 
                               & grepl("Corporate", engagement_data_detailed2$Sector.s., fixed = TRUE),]$county_connections_ratio)

mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pp_total_raw) 
                               & grepl("PreK-12", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pp_total_raw)
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pp_total_raw) 
                               & grepl("Higher Ed", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pp_total_raw)
mean(engagement_data_detailed2[!is.na(engagement_data_detailed2$pp_total_raw) 
                               & grepl("Corporate", engagement_data_detailed2$Sector.s., fixed = TRUE),]$pp_total_raw)
#PreK-12; Higher Ed; Corporate

exam_scores <- data.frame(
  row.names = c("PreK-12", "Higher Ed", "Corporate"),
  Black_Hispanic = c(0.3415225, 0.3381201, 0.3394373),
  Free_Lunch = c(0.436665, 0.4330946, 0.4348331),
  Expenditures = c(1.000148, 1.000175, 1.000201),
  Internet = c(13337.42, 13393.95, 13399.39))

max_min <- data.frame(
  Black_Hispanic = c(0.335, 0.342), 
  Free_Lunch = c(0.432, 0.437), 
  Expenditures = c(1.0000, 1.0002),
  Internet = c(13350, 13450))
rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, exam_scores)
df

op <- par(mar = c(1, 2, 2, 2))
create_beautiful_radarchart(
  data = df, caxislabels = c('', '', '', '', ''),
  color = c("#264653", "#E76F51", "#E9C46A"))
legend(
  x = "right", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#264653", "#E76F51", "#E9C46A"),
  text.col = "black", cex = 1, pt.cex = 2)
title("by Sector")
par(op)

#################Primary.Essential.Function
# top values: LC - Content Creation & Curation 
#LC - Sites, Resources & Reference 
#LC - Digital Learning Platforms 


#### SCORES
aggregate(pct_black.hispanic ~ Primary.Essential.Function, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_black.hispanic) & 
                                           (engagement_data_detailed2$Primary.Essential.Function == "LC - Content Creation & Curation" |
                                              engagement_data_detailed2$Primary.Essential.Function == "LC - Sites, Resources & Reference" | 
                                              engagement_data_detailed2$Primary.Essential.Function == "LC - Digital Learning Platforms"),], mean)
aggregate(pct_free.reduced ~ Primary.Essential.Function, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pct_free.reduced) & 
                                           (engagement_data_detailed2$Primary.Essential.Function == "LC - Content Creation & Curation" |
                                              engagement_data_detailed2$Primary.Essential.Function == "LC - Sites, Resources & Reference" | 
                                              engagement_data_detailed2$Primary.Essential.Function == "LC - Digital Learning Platforms"),], mean)
aggregate(county_connections_ratio ~ Primary.Essential.Function, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$county_connections_ratio) & 
                                           (engagement_data_detailed2$Primary.Essential.Function == "LC - Content Creation & Curation" |
                                              engagement_data_detailed2$Primary.Essential.Function == "LC - Sites, Resources & Reference" | 
                                              engagement_data_detailed2$Primary.Essential.Function == "LC - Digital Learning Platforms"),], mean)
aggregate(pp_total_raw ~ Primary.Essential.Function, 
          data=engagement_data_detailed2[!is.na(engagement_data_detailed2$pp_total_raw) & 
                                           (engagement_data_detailed2$Primary.Essential.Function == "LC - Content Creation & Curation" |
                                              engagement_data_detailed2$Primary.Essential.Function == "LC - Sites, Resources & Reference" | 
                                              engagement_data_detailed2$Primary.Essential.Function == "LC - Digital Learning Platforms"),], mean)

exam_scores <- data.frame(
  row.names = c("Content Creation & Curation", "Sites, Resources & Reference", "Digital Learning Platforms"),
  Black_Hispanic = c(0.3414278, 0.3395789, 0.3506943),
  Free_Lunch = c(0.4349155, 0.4351218, 0.4465405),
  Expenditures = c(1.000167, 1.000080, 1.000155),
  Internet = c(13364.86, 13312.71, 13206.17))

max_min <- data.frame(
  Black_Hispanic = c(0.335, 0.355), 
  Free_Lunch = c(0.430, 0.448), 
  Expenditures = c(1.0000, 1.0002),
  Internet = c(13200, 13400))
rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, exam_scores)
df

op <- par(mar = c(1, 2, 2, 2))
create_beautiful_radarchart(
  data = df, caxislabels = c('', '', '', '', ''),
  color = c("#264653", "#E76F51", "#E9C46A"))
legend(
  x = "right", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#264653", "#E76F51", "#E9C46A"),
  text.col = "black", cex = 1, pt.cex = 2)
title("by Function")
par(op)