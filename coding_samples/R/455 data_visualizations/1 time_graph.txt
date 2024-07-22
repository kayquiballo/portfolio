library(tidyverse)
library(WDI)
library(hrbrthemes)
library(gganimate)

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

######### pct_black.hispanic monthly avg for log_engagement_index ###############
output1 <- aggregate(engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pct_black.hispanic),]$log_engagement_index,
                    list(format(engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pct_black.hispanic),]$time, "%Y-%m"), 
                         engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pct_black.hispanic),]$pct_black.hispanic), 
                    mean)
colnames(output1) <- c('monthYear', 'pct_black.hispanic', 'log_engagement_index_monthly_avg')
output1 <- output1[output1$pct_black.hispanic!="NaN",]
print(output1)

str(output1)
output1$monthYear <- as.Date(paste(output1$monthYear, "-01", sep=""))
str(output1)


output1$pct_black.hispanic[output1$pct_black.hispanic=="[0, 0.2["] <- "0-20%"
output1$pct_black.hispanic[output1$pct_black.hispanic=="[0.2, 0.4["] <- "20-40%"
output1$pct_black.hispanic[output1$pct_black.hispanic=="[0.4, 0.6["] <- "40-60%"
output1$pct_black.hispanic[output1$pct_black.hispanic=="[0.6, 0.8["] <- "60-80%"
output1$pct_black.hispanic[output1$pct_black.hispanic=="[0.8, 1["] <- "80-100%"
head(output1$pct_black.hispanic)

#ggplot(output1) + 
#  geom_point(aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pct_black.hispanic), size = 3) + 
#  geom_line(data = output1, aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pct_black.hispanic))

#ggplot(output1) + 
#  geom_point(aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pct_black.hispanic), size = 3) + 
#  geom_smooth(data = output1, aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pct_black.hispanic), se=FALSE)

ggplot(output1) + 
  geom_point(aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pct_black.hispanic), size = 3) + 
  stat_smooth(data=output1, aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pct_black.hispanic), method="lm", se=FALSE, fill=NA, formula=y ~ poly(x, 6)) +
  xlab("Date") + ylab("Average Monthly Tech Engagement") + 
  labs(fill = "% Black/Hispanic") +
  ggtitle("by Percent Black/Hispanic") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "% Black/Hispanic\n") 

######### pct_free.reduced monthly avg for log_engagement_index ###############

output2 <- aggregate(engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pct_free.reduced),]$log_engagement_index,
                     list(format(engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pct_free.reduced),]$time, "%Y-%m"), 
                          engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pct_free.reduced),]$pct_free.reduced), 
                     mean)
colnames(output2) <- c('monthYear', 'pct_free.reduced', 'log_engagement_index_monthly_avg')
output2 <- output2[output2$pct_free.reduced!="",]
print(output2)

str(output2)
output2$monthYear <- as.Date(paste(output2$monthYear, "-01", sep=""))
str(output2)


output2$pct_free.reduced[output2$pct_free.reduced=="[0, 0.2["] <- "0-20%"
output2$pct_free.reduced[output2$pct_free.reduced=="[0.2, 0.4["] <- "20-40%"
output2$pct_free.reduced[output2$pct_free.reduced=="[0.4, 0.6["] <- "40-60%"
output2$pct_free.reduced[output2$pct_free.reduced=="[0.6, 0.8["] <- "60-80%"
output2$pct_free.reduced[output2$pct_free.reduced=="[0.8, 1["] <- "80-100%"
head(output2$pct_free.reduced)

ggplot(output2) + 
  geom_point(aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pct_free.reduced), size = 3) + 
  stat_smooth(data=output2, aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pct_free.reduced), method="lm", se=FALSE, fill=NA, formula=y ~ poly(x, 6)) +
  xlab("Date") + ylab("Average Monthly Tech Engagement") + 
  labs(fill = "% Free/Reduced ") +
  ggtitle("by Percent Free/Reduced Lunch") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "% Free/Reduced Lunch\n") 


######### county_connections_ratio monthly avg for log_engagement_index ###############

output3 <- aggregate(engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$county_connections_ratio),]$log_engagement_index,
                     list(format(engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$county_connections_ratio),]$time, "%Y-%m"), 
                          engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$county_connections_ratio),]$county_connections_ratio), 
                     mean)
colnames(output3) <- c('monthYear', 'county_connections_ratio', 'log_engagement_index_monthly_avg')
output3 <- output3[output3$county_connections_ratio!="NaN",]
output3 <- output3[output3$county_connections_ratio!="",]
print(output3)

str(output3)
output3$monthYear <- as.Date(paste(output3$monthYear, "-01", sep=""))
str(output3)

output3$county_connections_ratio[output3$county_connections_ratio=="[0.18, 1["] <- ".18-1"
output3$county_connections_ratio[output3$county_connections_ratio=="[1, 2["] <- "1-2"
head(output3$county_connections_ratio)

ggplot(output3) + 
  geom_point(aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = county_connections_ratio), size = 3) + 
  stat_smooth(data=output3, aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = county_connections_ratio), method="lm", se=FALSE, fill=NA, formula=y ~ poly(x, 2)) +
  xlab("Date") + ylab("Average Monthly Tech Engagement") + 
  labs(fill = "% Free/Reduced ") +
  ggtitle("by County Connections Ratio") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "County Connections Ratio\n")  + ylim(0.3,3.7)

output3 <- output3[output3$county_connections_ratio!="1-2",]

ggplot(output3) + 
  geom_point(aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = county_connections_ratio), size = 3) + 
  stat_smooth(data=output3, aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = county_connections_ratio), method="lm", se=FALSE, fill=NA, formula=y ~ poly(x, 6)) +
  xlab("Date") + ylab("Average Monthly Tech Engagement") + 
  labs(fill = "% Free/Reduced ") +
  ggtitle("by County Connections Ratio") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "County Connections Ratio\n") + ylim(0.3,3.7)


######### pp_total_raw monthly avg for log_engagement_index ###############

output4 <- aggregate(engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pp_total_raw),]$log_engagement_index,
                     list(format(engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pp_total_raw),]$time, "%Y-%m"), 
                          engagement_data_detailed[!is.na(engagement_data_detailed$log_engagement_index) & !is.na(engagement_data_detailed$time) & !is.na(engagement_data_detailed$pp_total_raw),]$pp_total_raw), 
                     mean)
colnames(output4) <- c('monthYear', 'pp_total_raw', 'log_engagement_index_monthly_avg')
output4 <- output4[output4$pp_total_raw!="NaN",]
output4 <- output4[output4$pp_total_raw!="",]
print(output4)

str(output4)
output4$monthYear <- as.Date(paste(output4$monthYear, "-01", sep=""))
str(output4)

output4$pp_total_raw[output4$pp_total_raw=="[4000, 6000["] <- "04000-06000"
output4$pp_total_raw[output4$pp_total_raw=="[6000, 8000["] <- "06000-08000"
output4$pp_total_raw[output4$pp_total_raw=="[8000, 10000["] <- "08000-10000"
output4$pp_total_raw[output4$pp_total_raw=="[10000, 12000["] <- "10000-12000"
output4$pp_total_raw[output4$pp_total_raw=="[12000, 14000["] <- "12000-14000"
output4$pp_total_raw[output4$pp_total_raw=="[14000, 16000["] <- "14000-16000"
output4$pp_total_raw[output4$pp_total_raw=="[16000, 18000["] <- "16000-18000"
output4$pp_total_raw[output4$pp_total_raw=="[18000, 20000["] <- "18000-20000"
output4$pp_total_raw[output4$pp_total_raw=="[20000, 22000["] <- "20000-22000"
output4$pp_total_raw[output4$pp_total_raw=="[22000, 24000["] <- "22000-24000"
output4$pp_total_raw[output4$pp_total_raw=="[32000, 34000["] <- "32000-34000"
head(output4$pp_total_raw)

ggplot(output4) + 
  geom_point(aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pp_total_raw), size = 3) + 
  stat_smooth(data=output4, aes(x = monthYear, y = log_engagement_index_monthly_avg, colour = pp_total_raw), method="lm", se=FALSE, fill=NA, formula=y ~ poly(x, 6)) +
  xlab("Date") + ylab("Average Monthly Tech Engagement") + 
  labs(fill = "% Free/Reduced ") +
  ggtitle("by Per-pupil total expenditure") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Per-pupil total expenditure\n") 
