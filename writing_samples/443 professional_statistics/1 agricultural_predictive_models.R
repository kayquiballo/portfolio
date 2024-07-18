install.packages("regclass")
library(MASS)
library(regclass)

district20 = data.frame(`LaSalleDistrict20(1831093)`)
district60 = data.frame(`PikeDistrict60(1832157)`)
district20_1 = subset(district20, PRCP != 'NA' & TMAX != 'NA'  & TMIN != 'NA' & TOBS != 'NA')
district60_1 = subset(district60, PRCP != 'NA' & TMAX != 'NA'  & TMIN != 'NA' & TOBS != 'NA')

cornyield_20 = na.omit(Corn_Yields_for_Illinois_and_Districts[Corn_Yields_for_Illinois_and_Districts$`Ag District Code` == 20, ])
cornyield_60 = na.omit(Corn_Yields_for_Illinois_and_Districts[Corn_Yields_for_Illinois_and_Districts$`Ag District Code` == 60, ])

corn_district_60 = illinois_corn_districts[illinois_corn_districts$Location == "Illinois-WSW", ]
corn_district_20 = illinois_corn_districts[illinois_corn_districts$Location == "Illinois-NE", ]

## District 20
# calculating yearly averages
district20_1$DATE = as.Date(district20_1$DATE)
a = as.matrix(with(district20_1, district20_1[(DATE > "2000-01-01" & 
                                                 DATE < "2000-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
a1 = apply(a, 2, mean)
b = as.matrix(with(district20_1, district20_1[(DATE > "2001-01-01" & 
                                                 DATE < "2001-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
b1 = apply(b, 2, mean)
c = as.matrix(with(district20_1, district20_1[(DATE > "2002-01-01" & 
                                                 DATE < "2002-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
c1 = apply(c, 2, mean)
d = as.matrix(with(district20_1, district20_1[(DATE > "2003-01-01" 
                                               & DATE < "2003-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
d1 = apply(d, 2, mean)
e = as.matrix(with(district20_1, district20_1[(DATE > "2004-01-01" & 
                                                 DATE < "2004-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
e1 = apply(e, 2, mean)
f = as.matrix(with(district20_1, district20_1[(DATE > "2005-01-01" & 
                                                 DATE < "2005-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
f1 = apply(f, 2, mean)
g = as.matrix(with(district20_1, district20_1[(DATE > "2006-01-01" & 
                                                 DATE < "2006-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
g1 = apply(g, 2, mean)
h = as.matrix(with(district20_1, district20_1[(DATE > "2007-01-01" & 
                                                 DATE < "2007-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
h1 = apply(h, 2, mean)
i = as.matrix(with(district20_1, district20_1[(DATE > "2008-01-01" & 
                                                 DATE < "2008-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
i1 = apply(i, 2, mean)
j = as.matrix(with(district20_1, district20_1[(DATE > "2009-01-01" & 
                                                 DATE < "2009-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
j1 = apply(j, 2, mean)
k = as.matrix(with(district20_1, district20_1[(DATE > "2010-01-01" & 
                                                 DATE < "2010-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
k1 = apply(k, 2, mean)
l = as.matrix(with(district20_1, district20_1[(DATE > "2011-01-01" & 
                                                 DATE < "2011-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
l1 = apply(l, 2, mean)
m = as.matrix(with(district20_1, district20_1[(DATE > "2012-01-01" & 
                                                 DATE < "2012-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
m1 = apply(m, 2, mean)
n = as.matrix(with(district20_1, district20_1[(DATE > "2013-01-01" & 
                                                 DATE < "2013-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
n1 = apply(n, 2, mean)
o = as.matrix(with(district20_1, district20_1[(DATE > "2014-01-01" & 
                                                 DATE < "2014-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
o1 = apply(o, 2, mean)
p = as.matrix(with(district20_1, district20_1[(DATE > "2015-01-01" & 
                                                 DATE < "2015-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
p1 = apply(p, 2, mean)
q = as.matrix(with(district20_1, district20_1[(DATE > "2016-01-01" & 
                                                 DATE < "2016-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
q1 = apply(q, 2, mean)
r = as.matrix(with(district20_1, district20_1[(DATE > "2017-01-01" & 
                                                 DATE < "2017-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
r1 = apply(r, 2, mean)
s = as.matrix(with(district20_1, district20_1[(DATE > "2018-01-01" & 
                                                 DATE < "2018-12-31"),
                                              c("PRCP", "TMAX", "TMIN", "TOBS")]))
s1 = apply(s, 2, mean)
factor_year20 = as.data.frame(rbind(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1))
factor_year20$YEAR = c('2000', '2001', '2002', '2003', '2004', '2005',
                       '2006', '2007', '2008', '2009', '2010', '2011', 
                       '2012', '2013', '2014', '2015', '2016', '2017', '2018')
district20_yield = cornyield_20$`CORN, GRAIN - YIELD, MEASURED IN BU / ACRE  -`
district20_acres = cornyield_20$`CORN, GRAIN - ACRES HARVESTED  -`
factor_year20$YIELD = district20_yield
factor_year20$ACRES = district20_acres
factor_year20$DISTRICT = 20

#install.packages("stringi", dependencies=TRUE, INSTALL_opts = c('--no-lock'))

# building the full models involving all predictors & interaction terms
fit20_1 = lm(YIELD ~ PRCP * TMAX + PRCP * TMIN + TMAX * TMIN, data = factor_year20)
# yearly-average model for district 20
fit_selection_20 = stepAIC(fit20_1, direction = "both")

#VIF(fit_selection_20)
#anova(fit20, fit_selection_20)

## District 60
# calculating yearly averages
district60_1$DATE = as.Date(district60_1$DATE, format = "%m/%d/%Y")
a60 = as.matrix(with(district60_1, district60_1[(DATE > "2000-01-01" & 
                                                   DATE < "2000-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
a2= apply(a60, 2, mean)
b60 = as.matrix(with(district60_1, district60_1[(DATE > "2001-01-01" & 
                                                   DATE < "2001-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
b2 = apply(b60, 2, mean)
c60 = as.matrix(with(district60_1, district60_1[(DATE > "2002-01-01" & 
                                                   DATE < "2002-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
c2 = apply(c60, 2, mean)
d60 = as.matrix(with(district60_1, district60_1[(DATE > "2003-01-01" & 
                                                   DATE < "2003-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
d2 = apply(d60, 2, mean)
e60 = as.matrix(with(district60_1, district60_1[(DATE > "2004-01-01" & 
                                                   DATE < "2004-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
e2 = apply(e60, 2, mean)
f60 = as.matrix(with(district60_1, district60_1[(DATE > "2005-01-01" & 
                                                   DATE < "2005-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
f2 = apply(f60, 2, mean)
g60 = as.matrix(with(district60_1, district60_1[(DATE > "2006-01-01" & 
                                                   DATE < "2006-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
g2 = apply(g60, 2, mean)
h60 = as.matrix(with(district60_1, district60_1[(DATE > "2007-01-01" & 
                                                   DATE < "2007-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
h2 = apply(h60, 2, mean)
i60 = as.matrix(with(district60_1, district60_1[(DATE > "2008-01-01" & 
                                                   DATE < "2008-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
i2 = apply(i60, 2, mean)
j60 = as.matrix(with(district60_1, district60_1[(DATE > "2009-01-01" & 
                                                   DATE < "2009-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
j2 = apply(j60, 2, mean)
k60 = as.matrix(with(district60_1, district60_1[(DATE > "2010-01-01" & 
                                                   DATE < "2010-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
k2 = apply(k60, 2, mean)
l60 = as.matrix(with(district60_1, district60_1[(DATE > "2011-01-01" & 
                                                   DATE < "2011-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
l2 = apply(l60, 2, mean)
m60 = as.matrix(with(district60_1, district60_1[(DATE > "2012-01-01" & 
                                                   DATE < "2012-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
m2 = apply(m60, 2, mean)
n60 = as.matrix(with(district60_1, district60_1[(DATE > "2013-01-01" & 
                                                   DATE < "2013-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
n2 = apply(n60, 2, mean)
o60 = as.matrix(with(district60_1, district60_1[(DATE > "2014-01-01" & 
                                                   DATE < "2014-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
o2 = apply(o60, 2, mean)
p60 = as.matrix(with(district60_1, district60_1[(DATE > "2015-01-01" & 
                                                   DATE < "2015-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
p2 = apply(p60, 2, mean)
q60 = as.matrix(with(district60_1, district60_1[(DATE > "2016-01-01" & 
                                                   DATE < "2016-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
q2 = apply(q60, 2, mean)
r60 = as.matrix(with(district60_1, district60_1[(DATE > "2017-01-01" & 
                                                   DATE < "2017-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
r2 = apply(r60, 2, mean)
s60 = as.matrix(with(district60_1, district60_1[(DATE > "2018-01-01" & 
                                                   DATE < "2018-12-31"),
                                                c("PRCP", "TMAX", "TMIN", "TOBS")]))
s2 = apply(s60, 2, mean)
factor_year60 = as.data.frame(rbind(a2,b2,c2,d2,e2,f2,g2,
                                    h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2))
factor_year60$YEAR = c('2000', '2001', '2002', '2003', '2004', '2005', 
                       '2006', '2007', '2008', '2009', '2010', '2011', 
                       '2012', '2013', '2014', '2015','2016','2017','2018')
district60_yield = cornyield_60$`CORN, GRAIN - YIELD, MEASURED IN BU / ACRE  -`
district60_acres = cornyield_60$`CORN, GRAIN - ACRES HARVESTED  -`
factor_year60$YIELD = district60_yield
factor_year60$ACRES = district60_acres
factor_year60$DISTRICT = 60
# building the full model with yearly averages and all interaction terms
fit60_1 = lm(YIELD ~ PRCP * TMAX + PRCP * TMIN + TMAX * TMIN, data = factor_year60)
# yearly-average model for district 60
fit_selection_60 = stepAIC(fit60_1, direction = 'both')
VIF(fit_selection_60)


# Comparison between District 20 and District 60
table = rbind(factor_year20, factor_year60)
table$DISTRICT = factor(table$DISTRICT)
fit_comp = lm(YIELD ~ DISTRICT, data = table)

# drawing the corn yield vs year line graph
library(ggplot2)
ggplot(data = table, aes(x = YEAR, y = YIELD, group = DISTRICT)) +
  geom_line(aes(color = DISTRICT)) +
  geom_point(aes(color = DISTRICT)) + 
  ggtitle("Corn Yield With Respect To Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 


# Model Based on Progress

# District 20

# Planted - pricipitation, tmax, and tmin averages
plant_20 = corn_district_20[is.na(corn_district_20$Planted) == FALSE, c('Year', 'Date')]
colnames(plant_20) = c('YEAR', 'DATE')
df_plant_20 = plyr::join(plant_20, district20_1, type = 'inner')
mean_plant = aggregate(df_plant_20[, c('PRCP', 'TMAX', 'TMIN')],
                       list(df_plant_20$YEAR), 
                       mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-04-20', i)
  string_late = sprintf('%d-06-01', i)
  mean_plant[nrow(mean_plant)+1, 1:4] = c(i, mean(district20_1[(district20_1$DATE > string_early & 
                                                                  district20_1$DATE < string_late), 'PRCP']), 
                                          mean(district20_1[(district20_1$DATE > string_early & 
                                                               district20_1$DATE < string_late), 'TMAX']), 
                                          mean(district20_1[(district20_1$DATE > string_early & 
                                                               district20_1$DATE < string_late), 'TMIN']))
}


# Emerged - pricipitation, tmax, and tmin averages
emerge_20 = corn_district_20[is.na(corn_district_20$Emerged) == FALSE, c('Year', 'Date')]
colnames(emerge_20) = c('YEAR', 'DATE')
df_emerge_20 = plyr::join(emerge_20, district20_1, type = 'inner')
mean_emerge = aggregate(df_emerge_20[, c('PRCP', 'TMAX', 'TMIN')],
                        list(df_emerge_20$YEAR), 
                        mean)
mean_emerge[nrow(mean_emerge)+1, 1:4] = c(2001, mean(district20_1[(district20_1$DATE > "2001-05-01" & 
                                                                     district20_1$DATE < "2001-06-15"), 'PRCP']), 
                                          mean(district20_1[(district20_1$DATE > "2001-05-01" & 
                                                               district20_1$DATE < "2001-06-15"), 'TMAX']), 
                                          mean(district20_1[(district20_1$DATE > "2001-05-01" & 
                                                               district20_1$DATE < "2001-06-15"), 'TMIN']))
mean_emerge[nrow(mean_emerge)+1, 1:4] = c(2002, mean(district20_1[(district20_1$DATE > "2002-05-01" & 
                                                                     district20_1$DATE < "2002-06-15"), 'PRCP']), 
                                          mean(district20_1[(district20_1$DATE > "2002-05-01" & 
                                                               district20_1$DATE < "2002-06-15"), 'TMAX']), 
                                          mean(district20_1[(district20_1$DATE > "2002-05-01" & 
                                                               district20_1$DATE < "2002-06-15"), 'TMIN']))
mean_emerge[nrow(mean_emerge)+1, 1:4] = c(2000, mean(district20_1[(district20_1$DATE > "2000-05-01" & 
                                                                     district20_1$DATE < "2000-06-15"), 'PRCP']), 
                                          mean(district20_1[(district20_1$DATE > "2000-05-01" & 
                                                               district20_1$DATE < "2000-06-15"), 'TMAX']), 
                                          mean(district20_1[(district20_1$DATE > "2000-05-01" & 
                                                               district20_1$DATE < "2000-06-15"), 'TMIN']))
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-05-01', i)
  string_late = sprintf('%d-06-15', i)
  mean_emerge[nrow(mean_emerge)+1, 1:4] = c(i, mean(district20_1[(district20_1$DATE > string_early & 
                                                                    district20_1$DATE < string_late), 'PRCP']), 
                                            mean(district20_1[(district20_1$DATE > string_early & 
                                                                 district20_1$DATE < string_late), 'TMAX']), 
                                            mean(district20_1[(district20_1$DATE > string_early & 
                                                                 district20_1$DATE < string_late), 'TMIN']))
}
mean_emerge = mean_emerge[order(mean_emerge$Group.1),]

# Silking - pricipitation, tmax, and tmin averages
silk_20 = corn_district_20[is.na(corn_district_20$Silking) == FALSE, c('Year', 'Date')]
colnames(silk_20) = c('YEAR', 'DATE')
df_silk_20 = plyr::join(silk_20, district20_1, type = 'inner')
mean_silk = aggregate(df_silk_20[, c('PRCP', 'TMAX', 'TMIN')],
                      list(df_silk_20$YEAR), 
                      mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-07-01', i)
  string_late = sprintf('%d-08-10', i)
  mean_silk[nrow(mean_silk)+1, 1:4] = c(i, mean(district20_1[(district20_1$DATE > string_early & 
                                                                district20_1$DATE < string_late), 'PRCP']), 
                                        mean(district20_1[(district20_1$DATE > string_early & 
                                                             district20_1$DATE < string_late), 'TMAX']), 
                                        mean(district20_1[(district20_1$DATE > string_early & 
                                                             district20_1$DATE < string_late), 'TMIN']))
}

# Dough
dough_20 = corn_district_20[is.na(corn_district_20$Dough) == FALSE, c('Year', 'Date')]
colnames(dough_20) = c('YEAR', 'DATE')
df_dough_20 = plyr::join(dough_20, district20_1, type = 'inner')
mean_dough = aggregate(df_dough_20[, c('PRCP', 'TMAX', 'TMIN')],
                       list(df_dough_20$YEAR), 
                       mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-07-15', i)
  string_late = sprintf('%d-09-01', i)
  mean_dough[nrow(mean_dough)+1, 1:4] = c(i, mean(district20_1[(district20_1$DATE > string_early & 
                                                                  district20_1$DATE < string_late), 'PRCP']), 
                                          mean(district20_1[(district20_1$DATE > string_early & 
                                                               district20_1$DATE < string_late), 'TMAX']), 
                                          mean(district20_1[(district20_1$DATE > string_early & 
                                                               district20_1$DATE < string_late), 'TMIN']))
}

# Dented - pricipitation, tmax, and tmin averages
dented_20 = corn_district_20[is.na(corn_district_20$Dented) == FALSE, c('Year', 'Date')]
colnames(dented_20) = c('YEAR', 'DATE')
df_dented_20 = plyr::join(dented_20, district20_1, type = 'inner')
mean_dented = aggregate(df_dented_20[, c('PRCP', 'TMAX', 'TMIN')],
                        list(df_dented_20$YEAR), 
                        mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-08-10', i)
  string_late = sprintf('%d-09-20', i)
  mean_dented[nrow(mean_dented)+1, 1:4] = c(i, mean(district20_1[(district20_1$DATE > string_early & 
                                                                    district20_1$DATE < string_late), 'PRCP']), 
                                            mean(district20_1[(district20_1$DATE > string_early & 
                                                                 district20_1$DATE < string_late), 'TMAX']), 
                                            mean(district20_1[(district20_1$DATE > string_early & 
                                                                 district20_1$DATE < string_late), 'TMIN']))
}

# Mature - pricipitation, tmax, and tmin averages
mature_20 = corn_district_20[is.na(corn_district_20$Mature) == FALSE, c('Year', 'Date')]
colnames(mature_20) = c('YEAR', 'DATE')
df_mature_20 = plyr::join(mature_20, district20_1, type = 'inner')
mean_mature = aggregate(df_mature_20[, c('PRCP', 'TMAX', 'TMIN')],
                        list(df_mature_20$YEAR), 
                        mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-09-01', i)
  string_late = sprintf('%d-10-20', i)
  mean_mature[nrow(mean_mature)+1, 1:4] = c(i, mean(district20_1[(district20_1$DATE > string_early & 
                                                                    district20_1$DATE < string_late), 'PRCP']), 
                                            mean(district20_1[(district20_1$DATE > string_early & 
                                                                 district20_1$DATE < string_late), 'TMAX']), 
                                            mean(district20_1[(district20_1$DATE > string_early & 
                                                                 district20_1$DATE < string_late), 'TMIN']))
}

# Harvest - pricipitation, tmax, and tmin averages
harvest_20 = corn_district_20[is.na(corn_district_20$Harvested) == FALSE, c('Year', 'Date')]
colnames(harvest_20) = c('YEAR', 'DATE')
df_harvest_20 = plyr::join(harvest_20, district20_1, type = 'inner')
mean_harvest = aggregate(df_harvest_20[, c('PRCP', 'TMAX', 'TMIN')],
                         list(df_harvest_20$YEAR), 
                         mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-09-10', i)
  string_late = sprintf('%d-11-15', i)
  mean_harvest[nrow(mean_harvest)+1, 1:4] = c(i, mean(district20_1[(district20_1$DATE > string_early & 
                                                                      district20_1$DATE < string_late), 'PRCP']), 
                                              mean(district20_1[(district20_1$DATE > string_early & 
                                                                   district20_1$DATE < string_late), 'TMAX']), 
                                              mean(district20_1[(district20_1$DATE > string_early 
                                                                 & district20_1$DATE < string_late), 'TMIN']))
}

# Precipitation throughout the growing progresses
phase_20 = cbind(mean_plant$Group.1, mean_plant$PRCP, mean_emerge$PRCP, mean_silk$PRCP, 
                 mean_dough$PRCP, mean_dented$PRCP, mean_mature$PRCP, mean_harvest$PRCP, 
                      mean_plant$TMAX, mean_emerge$TMAX, mean_silk$TMAX, 
                 mean_dough$TMAX, mean_dented$TMAX, mean_mature$TMAX, mean_harvest$TMAX,
                      mean_plant$TMIN, mean_emerge$TMIN, mean_silk$TMIN, 
                 mean_dough$TMIN, mean_dented$TMIN, mean_mature$TMIN, mean_harvest$TMIN)
phase_20 = cbind(phase_20, cornyield_20$`CORN, GRAIN - YIELD, MEASURED IN BU / ACRE  -`[1:19])
colnames(phase_20) = c('YEAR', 'PRCP_Planted', 'PRCP_Emerged', 'PRCP_Silking', 
                       'PRCP_Dough', 'PRCP_Dented', 'PRCP_Mature', 'PRCP_Harvested',
                            'TMAX_Planted', 'TMAX_Emerged', 'TMAX_Silking', 
                       'TMAX_Dough', 'TMAX_Dented', 'TMAX_Mature', 'TMAX_Harvested', 
                            'TMIN_Planted', 'TMIN_Emerged', 'TMIN_Silking', 
                       'TMIN_Dough', 'TMIN_Dented', 'TMIN_Mature', 
                       'TMIN_Harvested', 'YIELD')
phase_20 = data.frame(phase_20)

# prcp-tmax full model
fit_prcp_tmax_20 = lm(YIELD ~ . - YEAR - TMIN_Planted - TMIN_Emerged - 
                        TMIN_Silking - TMIN_Dough - TMIN_Dented - TMIN_Mature -
                        TMIN_Harvested, data = phase_20)
# prcp-tmax selected model
fit_selected = MASS::stepAIC(fit_prcp_tmax_20, direction = 'both')
# prcp-tmin full model
fit_prcp_tmin_20 = lm(YIELD ~ . - YEAR - TMAX_Planted - TMAX_Emerged - 
                        TMAX_Silking - TMAX_Dough - TMAX_Dented - TMAX_Mature - 
                        TMAX_Harvested, data = phase_20)
# prcp-tmin selected model
fit_selected2 = MASS::stepAIC(fit_prcp_tmin_20, direction = 'both')

# District 60

# planted - pricipitation, tmax, and tmin averages
plant_60 = corn_district_60[is.na(corn_district_60$Planted) == FALSE, c('Year', 'Date')]
colnames(plant_60) = c('YEAR', 'DATE')
df_plant_60 = plyr::join(plant_60, district60_1, type = 'inner')
mean_plant_60 = aggregate(df_plant_60[, c('PRCP', 'TMAX', 'TMIN')],
                          list(df_plant_60$YEAR), 
                          mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-04-20', i)
  string_late = sprintf('%d-06-01', i)
  mean_plant_60[nrow(mean_plant_60)+1, 1:4] = c(i, mean(district60_1[(district60_1$DATE > string_early & 
                                                                        district60_1$DATE < string_late), 'PRCP']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMAX']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMIN']))
}

# emerged - pricipitation, tmax, and tmin averages
emerge_60 = corn_district_60[is.na(corn_district_60$Emerged) == FALSE, c('Year', 'Date')]
colnames(emerge_60) = c('YEAR', 'DATE')
df_emerge_60 = plyr::join(emerge_60, district60_1, type = 'inner')
mean_emerge_60 = aggregate(df_emerge_60[, c('PRCP', 'TMAX', 'TMIN')],
                           list(df_emerge_60$YEAR), 
                           mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-05-01', i)
  string_late = sprintf('%d-06-15', i)
  mean_emerge_60[nrow(mean_emerge_60)+1, 1:4] = c(i, mean(district60_1[(district60_1$DATE > string_early & 
                                                                          district60_1$DATE < string_late), 'PRCP']), 
                                                  mean(district60_1[(district60_1$DATE > string_early & 
                                                                       district60_1$DATE < string_late), 'TMAX']), 
                                                  mean(district60_1[(district60_1$DATE > string_early & 
                                                                       district60_1$DATE < string_late), 'TMIN']))
}
for (i in c(2000, 2001, 2002)){
  string_early = sprintf('%d-05-01', i)
  string_late = sprintf('%d-06-15', i)
  mean_emerge_60[nrow(mean_emerge_60)+1, 1:4] = c(i, mean(district60_1[(district60_1$DATE > string_early & 
                                                                          district60_1$DATE < string_late), 'PRCP']), 
                                                  mean(district60_1[(district60_1$DATE > string_early & 
                                                                       district60_1$DATE < string_late), 'TMAX']), 
                                                  mean(district60_1[(district60_1$DATE > string_early & 
                                                                       district60_1$DATE < string_late), 'TMIN']))
}
mean_emerge_60 = mean_emerge_60[order(mean_emerge_60$Group.1),]

# silking - pricipitation, tmax, and tmin averages
silk_60 = corn_district_60[is.na(corn_district_60$Silking) == FALSE, c('Year', 'Date')]
colnames(silk_60) = c('YEAR', 'DATE')
df_silk_60 = plyr::join(silk_60, district60_1, type = 'inner')
mean_silk_60 = aggregate(df_silk_60[, c('PRCP', 'TMAX', 'TMIN')],
                         list(df_silk_60$YEAR), 
                         mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-07-01', i)
  string_late = sprintf('%d-08-10', i)
  mean_silk_60[nrow(mean_silk_60)+1, 1:4] = c(i, mean(district60_1[(district60_1$DATE > string_early & 
                                                                      district60_1$DATE < string_late), 'PRCP']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMAX']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMIN']))
}

# dough - pricipitation, tmax, and tmin averages
dough_60 = corn_district_60[is.na(corn_district_60$Dough) == FALSE, c('Year', 'Date')]
colnames(dough_60) = c('YEAR', 'DATE')
df_dough_60 = plyr::join(dough_60, district60_1, type = 'inner')
mean_dough_60 = aggregate(df_dough_60[, c('PRCP', 'TMAX', 'TMIN')],
                          list(df_dough_60$YEAR), 
                          mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-07-15', i)
  string_late = sprintf('%d-09-01', i)
  mean_dough_60[nrow(mean_dough_60)+1, 1:4] = c(i, mean(district60_1[(district60_1$DATE > string_early & 
                                                                        district60_1$DATE < string_late), 'PRCP']), 
                                              mean(district60_1[(district60_1$DATE > string_early & 
                                                                   district60_1$DATE < string_late), 'TMAX']), 
                                              mean(district60_1[(district60_1$DATE > string_early & 
                                                                   district60_1$DATE < string_late), 'TMIN']))
}

# dented - pricipitation, tmax, and tmin averages
dent_60 = corn_district_60[is.na(corn_district_60$Dented) == FALSE, c('Year', 'Date')]
colnames(dent_60) = c('YEAR', 'DATE')
df_dent_60 = plyr::join(dent_60, district60_1, type = 'inner')
mean_dent_60 = aggregate(df_dent_60[, c('PRCP', 'TMAX', 'TMIN')],
                         list(df_dent_60$YEAR), 
                         mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-08-10', i)
  string_late = sprintf('%d-09-20', i)
  mean_dent_60[nrow(mean_dent_60)+1, 1:4] = c(i, mean(district60_1[(district60_1$DATE > string_early &
                                                                      district60_1$DATE < string_late), 'PRCP']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMAX']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMIN']))
}

# Mature - pricipitation, tmax, and tmin averages
mature_60 = corn_district_60[is.na(corn_district_60$Mature) == FALSE, c('Year', 'Date')]
colnames(mature_60) = c('YEAR', 'DATE')
df_mature_60 = plyr::join(mature_60, district60_1, type = 'inner')
mean_mature_60 = aggregate(df_mature_60[, c('PRCP', 'TMAX', 'TMIN')],
                           list(df_mature_60$YEAR), 
                           mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-09-01', i)
  string_late = sprintf('%d-10-20', i)
  mean_mature_60[nrow(mean_mature_60)+1, 1:4] = c(i, mean(district60_1[(district60_1$DATE > string_early & 
                                                                          district60_1$DATE < string_late), 'PRCP']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMAX']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMIN']))
}

# Harvest - pricipitation, tmax, and tmin averages
harvest_60 = corn_district_60[is.na(corn_district_60$Harvested) == FALSE, c('Year', 'Date')]
colnames(harvest_60) = c('YEAR', 'DATE')
df_harvest_60 = plyr::join(harvest_60, district60_1, type = 'inner')
mean_harvest_60 = aggregate(df_harvest_60[, c('PRCP', 'TMAX', 'TMIN')],
                            list(df_harvest_60$YEAR), 
                            mean)
for (i in seq(2010, 2018)){
  string_early = sprintf('%d-09_10', i)
  string_late = sprintf('%d-11-15', i)
  mean_harvest_60[nrow(mean_harvest_60)+1, 1:4] = c(i, mean(district60_1[(district60_1$DATE > string_early & 
                                                                            district60_1$DATE < string_late), 'PRCP']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMAX']), 
                                                mean(district60_1[(district60_1$DATE > string_early & 
                                                                     district60_1$DATE < string_late), 'TMIN']))
}

# Precipitation throughout the growing progresses
phase_60 = cbind(mean_plant_60$Group.1, mean_plant_60$PRCP, mean_emerge_60$PRCP, 
                 mean_silk_60$PRCP, mean_dough_60$PRCP, mean_dent_60$PRCP, 
                 mean_mature_60$PRCP, mean_harvest_60$PRCP, 
                 mean_plant_60$TMAX, mean_emerge_60$TMAX, mean_silk_60$TMAX, 
                 mean_dough_60$TMAX, mean_dent_60$TMAX, mean_mature_60$TMAX, 
                 mean_harvest_60$TMAX, mean_plant_60$TMIN, mean_emerge_60$TMIN, 
                 mean_silk_60$TMIN, mean_dough_60$TMIN, mean_dent_60$TMIN, 
                 mean_mature_60$TMIN, mean_harvest_60$TMIN)
phase_60 = cbind(phase_60, cornyield_60$`CORN, GRAIN - YIELD, MEASURED IN BU / ACRE  -`[1:19])
colnames(phase_60) = c('YEAR', 'PRCP_Planted', 'PRCP_Emerged', 'PRCP_Silking', 
                       'PRCP_Dough', 'PRCP_Dented', 'PRCP_Mature', 'PRCP_Harvested',
                       'TMAX_Planted', 'TMAX_Emerged', 'TMAX_Silking', 'TMAX_Dough', 
                       'TMAX_Dented', 'TMAX_Mature', 'TMAX_Harvested', 
                       'TMIN_Planted', 'TMIN_Emerged', 'TMIN_Silking', 'TMIN_Dough', 
                       'TMIN_Dented', 'TMIN_Mature', 'TMIN_Harvested', 'YIELD')
phase_60 = data.frame(phase_60)
# prcp-tmax full model
fit_prcp_tmax_60 = lm(YIELD ~ . - YEAR - TMIN_Planted - TMIN_Emerged - 
                        TMIN_Silking - TMIN_Dough - TMIN_Dented - TMIN_Mature - 
                        TMIN_Harvested, data = phase_60)
# prcp-tmax selected model
fit_selected3 = MASS::stepAIC(fit_prcp_tmax_60, direction = 'both')
# prcp-tmin full model
fit_prcp_tmin_60 = lm(YIELD ~ . - YEAR - TMAX_Planted - TMAX_Emerged - 
                        TMAX_Silking - TMAX_Dough - TMAX_Dented - TMAX_Mature -
                        TMAX_Harvested, data = phase_60)
# prcp-tmax selected model
fit_selected4 = MASS::stepAIC(fit_prcp_tmin_60, direction = 'both')

