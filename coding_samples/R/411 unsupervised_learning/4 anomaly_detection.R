install.packages('dbscan')
install.packages('factoextra')
library("factoextra")

test = read.csv("/Users/kagenquiballo/Desktop/MDSD_411/Assignment 4/assignment-4-option-1-test.csv",header=TRUE)
train = read.csv("/Users/kagenquiballo/Desktop/MDSD_411/Assignment 4/assignment-4-option-1-training.csv",header=TRUE)

set.seed(123456789)

#ID, Prod, Quant, Val, Insp
#train_clean <- subset(train, select = c(Prod, Quant, Val) )
#train_clean <- subset(train, select = c(ID, Prod, Quant, Val, Insp) )
train_clean <- train[complete.cases(train),] #129091 obs
train_clean$Uprice=train_clean$Val/train_clean$Quant
train_clean$log_Uprice=log(train_clean$Val/train_clean$Quant)
train_clean$log_Quant=log(train_clean$Quant)

par(mfrow=c(2,1))
#VAL
hist(train_clean$Val, main="histogram of Value", breaks=10)
summary(train_clean$Val)
hist(train_clean[train_clean$Val <= 3035 + 1.5*(9095-1480),]$Val, main="histogram of Value (filtered)", breaks=20)

#Quant
hist(train_clean$Quant, main="histogram of Quant", breaks=10)
summary(train_clean$Quant)
hist(train_clean[train_clean$Quant <= 185 + 1.5*(891-109),]$Quant, main="histogram of Quantity (filtered)", breaks=10)

#Uprice
hist(train_clean$Uprice, main="histogram of Uprice", breaks=10)
summary(train_clean$Uprice)
hist(train_clean[train_clean$Uprice <= 11.709 + 1.5*(18.374-7.345),]$Uprice, main="histogram of Uprice (filtered)", breaks=20)

#logs
hist(train_clean[train_clean$Uprice <= 11.709 + 1.5*(18.374-7.345),]$Uprice, main="histogram of Uprice (filtered)", breaks=20)
hist(train_clean[train_clean$log_Uprice <= 11.709 + 1.5*(18.374-7.345),]$log_Uprice, main="histogram of log_Uprice (filtered)", breaks=20)

hist(train_clean[train_clean$Quant <= 185 + 1.5*(891-109),]$Quant, main="histogram of Quantity (filtered)", breaks=10)
hist(train_clean[train_clean$log_Quant <= 185 + 1.5*(891-109),]$log_Quant, main="histogram of log_Quant (filtered)", breaks=10)


#clean again
train_clean <- train_clean[train_clean$Uprice <= 11.709 + 1.5*(18.374-7.345) 
                           & train_clean$Quant <= 185 + 1.5*(891-109),]


test_clean <- test[complete.cases(test),] #15546 obs
test_clean$Uprice=test_clean$Val/test_clean$Quant
test_clean$log_Uprice=log(test_clean$Val/test_clean$Quant)
test_clean$log_Quant=log(test_clean$Quant)
test_clean <- test_clean[test_clean$Uprice <= 11.709 + 1.5*(18.374-7.345) 
                         & test_clean$Quant <= 185 + 1.5*(891-109),]

#fitting the DBSCAN clustering model
train_clean2 <- subset(train_clean, select = c(log_Uprice, log_Quant) )
#kNNdist(train_clean2, k=4)
kNNdistplot(train_clean2, k=4)
abline(h = 0.025, lty = 2)
model=dbscan(train_clean2, eps=0.025, minPts=5)

test_clean2 <- subset(test_clean, select = c(log_Uprice, log_Quant) )
test_clean2$cluster <- predict(model, test_clean2, train_clean2)
test_clean2$Insp <- test_clean$Insp
table(test_clean2$cluster, test_clean2$Insp) #0 is a noise point

# Add the flag in the data
dbscan_data_final <- test_clean2 %>%
  mutate(unusual_behaviour = ifelse(cluster == 0, "fraud", "ok"))


# ============ ASSESSING DBSCAN ============
# Plotting Confusion Matrix
cm_dbscan <- confusionMatrix(reference = as.factor(dbscan_data_final$Insp), 
                             data = as.factor(dbscan_data_final$unusual_behaviour))

colorsBasketball <- c("#F57E00", "#FFA90A", "#FFCE72", "#3AAFF9", "#0087DC", "#005991")
colors60s <- c("#BF4402", "#94058E", "#005DD7", "#2690C3", "#F5C402", "#CE378E")

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col=colorsBasketball[2])
  text(195, 435, 'Fraud', cex=1.2)
  rect(250, 430, 340, 370, col=colorsBasketball[4])
  text(295, 435, 'Not Fraud', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=colorsBasketball[4])
  rect(250, 305, 340, 365, col=colorsBasketball[2])
  text(140, 400, 'Fraud', cex=1.2, srt=90)
  text(140, 335, 'Not Fraud', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

draw_confusion_matrix(cm_dbscan)

# ============ ASSESSING Local Outlier Factor Score ============


#fitting the DBSCAN clustering model
train_clean2 <- subset(train_clean, select = c(log_Uprice, log_Quant) )
model=lof(train_clean2, minPts = 5)

test_clean2 <- subset(test_clean, select = c(log_Uprice, log_Quant) )
test_clean2$cluster <- lof(test_clean2, minPts = 5)
test_clean2$Insp <- test_clean$Insp
table(test_clean2$cluster, test_clean2$Insp) #0 is a noise point

# Add the flag in the data
dbscan_data_final <- test_clean2 %>%
  mutate(unusual_behaviour = ifelse(cluster > 1, "fraud", "ok"))

# Plotting Confusion Matrix
cm_dbscan <- confusionMatrix(reference = as.factor(dbscan_data_final$Insp), 
                             data = as.factor(dbscan_data_final$unusual_behaviour))

draw_confusion_matrix(cm_dbscan)

# ============ ASSESSING isolation forest ============

library(MLmetrics)
library(kableExtra)

model_orig <- isolation.forest(
  train_clean2,
  ndim=1, sample_size=256,
  ntrees=100,
  missing_action="fail"
)

pred_orig <- predict(model_orig, test_clean2)

model_dens <- isolation.forest(
  train_clean2,
  ndim=1, sample_size=256,
  ntrees=100,
  missing_action="fail",
  scoring_metric="density"
)
pred_dens <- predict(model_dens, test_clean2)

model_fcf <- isolation.forest(
  train_clean2,
  ndim=1, sample_size=32,
  prob_pick_pooled_gain=1,
  ntrees=100,
  missing_action="fail"
)
pred_fcf <- predict(model_fcf, test_clean2)

is_outlier <- test_clean2$Insp %in% c("fraud")

results_df <- data.frame(
  Model = c(
    "Isolation Forest",
    "Density Isolation Forest",
    "Fair-Cut Forest"
  ),
  AUROC = c(
    AUC(pred_orig, is_outlier),
    AUC(pred_dens, is_outlier),
    AUC(pred_fcf, is_outlier)
  )
)
results_df %>%
  kable() %>%
  kable_styling()

########
test_clean2$cluster <- pred_orig

table(test_clean2$cluster, test_clean2$Insp) #0 is a noise point

# Add the flag in the data
dbscan_data_final <- test_clean2 %>%
  mutate(unusual_behaviour = ifelse(cluster > 0.65, "fraud", "ok"))

# Plotting Confusion Matrix
cm_dbscan <- confusionMatrix(reference = as.factor(dbscan_data_final$Insp), 
                             data = as.factor(dbscan_data_final$unusual_behaviour))

draw_confusion_matrix(cm_dbscan)