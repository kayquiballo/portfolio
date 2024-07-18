library(farff) # for reading arff file
library(cvTools) # explicit creation of folds for cross-validation
library(ModelMetrics) # used for precision-recall evaluation of classifiers
library(car) # for recode function

# optimal cutoff for predicting bad credit set as
# (cost of false negative/cost of false positive) times
# (prevalence of positive/prevalence of negative)
# (1/5)*(.3/.7) = 0.086
CUTOFF = 0.086
COSTMATRIX = matrix(c(0,5,1,0), nrow = 2, ncol = 2, byrow = TRUE)

credit = readARFF("/Users/kagenquiballo/Desktop/MDSD_411/Assignment 3/MSDS411_A3/dataset_31_credit-g.arff")
# write to comma-delimited text for review in Excel
write.csv(credit, file = "credit.csv", row.names = FALSE)

# check structure of the data frame
cat("\n\nStucture of initial credit data frame:\n")
print(str(credit))

# quick summary of credit data
cat("\n\nSummary of initial credit data frame:\n")
print(summary(credit))

# personal_status has level "female single" with no observations
cat("\n\nProblems with personal_status, no single females:\n")
print(table(credit$personal_status))

# fix this prior to analysis
credit$personal_status = factor(as.numeric(credit$personal_status),
    levels = c(1,2,3,4), 
    labels = c("male div/sep","female div/dep/mar","male single","male mar/wid"))

cat("\n\nProblems with purpose, low- and no-frequency levels:\n")
print(table(credit$purpose))
# keep first four classes: "new car", "used car", "furniture/equipment", "radio/tv"
# keep "education" and "business" with new values 
# add "retraining" to "education"
# gather all other levels into "other"
credit$purpose = recode(credit$purpose, '"new car" = "new car";
    "used car" = "used car"; 
    "furniture/equipment" = "furniture/equipment";
    "radio/tv" = "radio/tv"; 
    "education" = "education"; "retraining" = "education";
    "business" = "business"; 
    "domestic appliance" = "other"; "repairs" = "other"; "vacation" = "other"; 
    "other" = "other" ',
    levels = c("new car","used car","furniture/equipment","radio/tv", 
    "education","business","other" ))

# credit_amount is highly skewed... use log_credit_amount instead
credit$log_credit_amount = log(credit$credit_amount)    

# summary of transformed credit data
cat("\n\nSummary of revised credit data frame:\n")
print(summary(credit))

# logistic regression evaluated with cross-validation
# include explanatory variables except foreign_worker
# (only 37 of 100 cases are foreign workers)
credit_model = "class ~ checking_status + duration + 
    credit_history + purpose + log_credit_amount + savings_status + 
    employment + installment_commitment + personal_status +        
    other_parties + residence_since + property_magnitude +
    age + other_payment_plans + housing + existing_credits +      
    job + num_dependents + own_telephone" 

set.seed(1)
nfolds = 5
folds = cvFolds(nrow(credit), K = nfolds) # creates list of indices

baseprecision = rep(0, nfolds)  # precision with 0 cutoff
baserecall = rep(0, nfolds)  # recall with  0 cutoff
basef1Score = rep(0, nfolds)  # f1Score with 0 cutoff
basecost = rep(0, nfolds)  # total cost with 0 cutoff
ruleprecision = rep(0, nfolds)  # precision with CUTOFF rule
rulerecall = rep(0, nfolds)  # recall with CUTOFF rule
rulef1Score = rep(0, nfolds)  # f1Score with CUTOFF rule
rulecost = rep(0, nfolds)  # total cost with CUTOFF rule

for (ifold in seq(nfolds)) {
    # cat("\n\nSUMMARY FOR IFOLD:", ifold) # checking in development
    # print(summary(credit[(folds$which == ifold),]))
    # train model on all folds except ifold
    train = credit[(folds$which != ifold), ]
    test = credit[(folds$which == ifold),]
    credit_fit = glm(credit_model, family = binomial,
        data = train)
    # evaluate on fold ifold    
    credit_predict = predict.glm(credit_fit, 
        newdata = test, type = "response") 
    baseprecision[ifold] = ppv(as.numeric(test$class)-1, 
        credit_predict, cutoff = 0.5)  
    baserecall[ifold] = recall(as.numeric(test$class)-1, 
        credit_predict, cutoff = 0.5) 
    basef1Score[ifold] = f1Score(as.numeric(test$class)-1, 
        credit_predict, cutoff = 0.5) 
    basecost[ifold] = sum(
        confusionMatrix(as.numeric(test$class)-1,
        credit_predict) * COSTMATRIX)  
    ruleprecision[ifold] = ppv(as.numeric(test$class)-1, 
        credit_predict, cutoff = CUTOFF)  
    rulerecall[ifold] = recall(as.numeric(test$class)-1, 
        credit_predict, cutoff = CUTOFF) 
    rulef1Score[ifold] = f1Score(as.numeric(test$class)-1, 
        credit_predict, cutoff = CUTOFF)
    rulecost[ifold] = sum(
        confusionMatrix(as.numeric(test$class)-1, 
            credit_predict,cutoff=CUTOFF) * COSTMATRIX)                                    
} 
cvbaseline = data.frame(baseprecision, baserecall, basef1Score, basecost,
    ruleprecision, rulerecall, rulef1Score, rulecost)

cat("\n\nCross-validation summary across folds:\n")
print(round(cvbaseline, digits = 3))

cat("\n\nCross-validation baseline results under cost cutoff rules:")
cat("\n    F1 Score: ", round(mean(cvbaseline$rulef1Score), digits = 3))
cat("\n    Average cost per fold: ", 
    round(mean(cvbaseline$rulecost), digits = 2), "\n")

# prepare data for input to autoencoder work
design_matrix = model.matrix(as.formula(credit_model), data = credit)
design_data_frame = as.data.frame(design_matrix)[,-1]  # dropping the intercept term

# normalize the data 
minmaxnorm <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
minmax_data_frame <- lapply(design_data_frame, FUN = minmaxnorm)

cat("\n\nStructure of minmax_data_frame for input to autoencoding work:\n")
print(str(minmax_data_frame))

#####################################################################
#######################    K-means on PCA     #######################
#####################################################################
names(minmax_data_frame)[names(minmax_data_frame) == "checking_status1"] <- "X01"
names(minmax_data_frame)[names(minmax_data_frame) == "checking_status0<=X<200"] <- "X01"
names(minmax_data_frame)[names(minmax_data_frame) == "checking_status>=200"] <- "X02"
names(minmax_data_frame)[names(minmax_data_frame) == "checking_statusno checking"] <- "X03"
names(minmax_data_frame)[names(minmax_data_frame) == "credit_historyall paid"] <- "X04"
names(minmax_data_frame)[names(minmax_data_frame) == "credit_historyexisting paid"] <- "X05"
names(minmax_data_frame)[names(minmax_data_frame) == "credit_historydelayed previously"] <- "X06"
names(minmax_data_frame)[names(minmax_data_frame) == "credit_historycritical/other existing credit"] <- "X07"
names(minmax_data_frame)[names(minmax_data_frame) == "purposeused car"] <- "X08"
names(minmax_data_frame)[names(minmax_data_frame) == "purposefurniture/equipment"] <- "X09"
names(minmax_data_frame)[names(minmax_data_frame) == "purposeradio/tv"] <- "X10"

names(minmax_data_frame)[names(minmax_data_frame) == "purposeeducation"] <- "X11"
names(minmax_data_frame)[names(minmax_data_frame) == "purposebusiness"] <- "X12"
names(minmax_data_frame)[names(minmax_data_frame) == "purposeother"] <- "X13"
names(minmax_data_frame)[names(minmax_data_frame) == "savings_status100<=X<500"] <- "X14"
names(minmax_data_frame)[names(minmax_data_frame) == "savings_status500<=X<1000"] <- "X15"
names(minmax_data_frame)[names(minmax_data_frame) == "savings_status>=1000"] <- "X16"
names(minmax_data_frame)[names(minmax_data_frame) == "savings_statusno known savings"] <- "X17"
names(minmax_data_frame)[names(minmax_data_frame) == "employment<1"] <- "X18"
names(minmax_data_frame)[names(minmax_data_frame) == "employment1<=X<4"] <- "X19"
names(minmax_data_frame)[names(minmax_data_frame) == "employment4<=X<7"] <- "X20"

names(minmax_data_frame)[names(minmax_data_frame) == "employment>=7"] <- "X21"
names(minmax_data_frame)[names(minmax_data_frame) == "personal_statusfemale div/dep/mar"] <- "X22"
names(minmax_data_frame)[names(minmax_data_frame) == "personal_statusmale single"] <- "X23"
names(minmax_data_frame)[names(minmax_data_frame) == "personal_statusmale mar/wid"] <- "X24"
names(minmax_data_frame)[names(minmax_data_frame) == "other_partiesco applicant"] <- "X25"
names(minmax_data_frame)[names(minmax_data_frame) == "other_partiesguarantor"] <- "X26"
names(minmax_data_frame)[names(minmax_data_frame) == "property_magnitudelife insurance"] <- "X27"
names(minmax_data_frame)[names(minmax_data_frame) == "property_magnitudecar"] <- "X28"
names(minmax_data_frame)[names(minmax_data_frame) == "property_magnitudeno known property"] <- "X29"
names(minmax_data_frame)[names(minmax_data_frame) == "other_payment_plansstores"] <- "X30"

names(minmax_data_frame)[names(minmax_data_frame) == "other_payment_plansnone"] <- "X31"
names(minmax_data_frame)[names(minmax_data_frame) == "housingown"] <- "X32"
names(minmax_data_frame)[names(minmax_data_frame) == "housingfor free"] <- "X33"
names(minmax_data_frame)[names(minmax_data_frame) == "jobunskilled resident"] <- "X34"
names(minmax_data_frame)[names(minmax_data_frame) == "jobskilled"] <- "X35"
names(minmax_data_frame)[names(minmax_data_frame) == "jobhigh qualif/self emp/mgmt"] <- "X36"
names(minmax_data_frame)[names(minmax_data_frame) == "num_dependents"] <- "X37"
names(minmax_data_frame)[names(minmax_data_frame) == "own_telephoneyes"] <- "X38"

names(minmax_data_frame)[names(minmax_data_frame) == "duration"] <- "C01"
names(minmax_data_frame)[names(minmax_data_frame) == "log_credit_amount"] <- "C02"
names(minmax_data_frame)[names(minmax_data_frame) == "installment_commitment"] <- "C03"
names(minmax_data_frame)[names(minmax_data_frame) == "residence_since"] <- "C04"
names(minmax_data_frame)[names(minmax_data_frame) == "age"] <- "C05"
names(minmax_data_frame)[names(minmax_data_frame) == "existing_credits"] <- "C06"

minmax_data_frame$C01_cat <- ifelse(minmax_data_frame$C01 <= 0.2059, 0, 1) #median
minmax_data_frame$C02_cat <- ifelse(minmax_data_frame$C01 <= 0.5273, 0, 1) #mean

minmax_data_frame$C03_cat1 <- ifelse(minmax_data_frame$C03 < 0.1, 1, 0)
minmax_data_frame$C03_cat2 <- ifelse(minmax_data_frame$C03 > 0.3
                                     & minmax_data_frame$C03 < 0.4, 1, 0)
minmax_data_frame$C03_cat3 <- ifelse(minmax_data_frame$C03 > 0.6
                                     & minmax_data_frame$C03 < 0.7, 1, 0)
minmax_data_frame$C03_cat4 <- ifelse(minmax_data_frame$C03 > 0.9, 1, 0)

minmax_data_frame$C04_cat1 <- ifelse(minmax_data_frame$C04 < 0.1, 1, 0)
minmax_data_frame$C04_cat2 <- ifelse(minmax_data_frame$C04 > 0.3
                                     & minmax_data_frame$C04 < 0.4, 1, 0)
minmax_data_frame$C04_cat3 <- ifelse(minmax_data_frame$C04 > 0.6
                                     & minmax_data_frame$C04 < 0.7, 1, 0)
minmax_data_frame$C04_cat4 <- ifelse(minmax_data_frame$C04 > 0.9, 1, 0)

minmax_data_frame$C05_cat <- ifelse(minmax_data_frame$C05 <= 0.2500, 0, 1) #median

minmax_data_frame$C06_cat1 <- ifelse(minmax_data_frame$C06 < 0.1, 1, 0)
minmax_data_frame$C06_cat2 <- ifelse(minmax_data_frame$C06 > 0.3
                                     & minmax_data_frame$C06 < 0.4, 1, 0)
minmax_data_frame$C06_cat3 <- ifelse(minmax_data_frame$C06 > 0.6
                                     & minmax_data_frame$C06 < 0.7, 1, 0)
minmax_data_frame$C06_cat4 <- ifelse(minmax_data_frame$C06 > 0.9, 1, 0)


names(minmax_data_frame)[names(minmax_data_frame) == "duration"] <- "C01"
names(minmax_data_frame)[names(minmax_data_frame) == "log_credit_amount"] <- "C02"
names(minmax_data_frame)[names(minmax_data_frame) == "installment_commitment"] <- "C03"
names(minmax_data_frame)[names(minmax_data_frame) == "residence_since"] <- "C04"
names(minmax_data_frame)[names(minmax_data_frame) == "age"] <- "C05"
names(minmax_data_frame)[names(minmax_data_frame) == "existing_credits"] <- "C06"
par(mfrow=c(2,3))
hist(minmax_data_frame$C01, main="duration")
hist(minmax_data_frame$C02, main="log_credit_amount")
hist(minmax_data_frame$C03, main="installment_commitment")
hist(minmax_data_frame$C04, main="residence_since")
hist(minmax_data_frame$C05, main="age")
hist(minmax_data_frame$C06, main="existing_credits")

minmax_data_frame2 <- cbind(minmax_data_frame$X01,
                            minmax_data_frame$X02,
                            minmax_data_frame$X03,
                            minmax_data_frame$X04,
                            minmax_data_frame$X05,
                            minmax_data_frame$X06, 
                            minmax_data_frame$X07, 
                            minmax_data_frame$X08,
                            minmax_data_frame$X09,
                            minmax_data_frame$X10,
                            minmax_data_frame$X11,
                            minmax_data_frame$X12,
                            minmax_data_frame$X13,
                            minmax_data_frame$X14,
                            minmax_data_frame$X15,
                            minmax_data_frame$X16,
                            minmax_data_frame$X17,
                            minmax_data_frame$X18,
                            minmax_data_frame$X19,
                            minmax_data_frame$X20,
                            minmax_data_frame$X21,
                            minmax_data_frame$X22,
                            minmax_data_frame$X23,
                            minmax_data_frame$X24,
                            minmax_data_frame$X25,
                            minmax_data_frame$X26,
                            minmax_data_frame$X27,
                            minmax_data_frame$X28,
                            minmax_data_frame$X29,
                            minmax_data_frame$X30,
                            minmax_data_frame$X31,
                            minmax_data_frame$X32,
                            minmax_data_frame$X33,
                            minmax_data_frame$X34,
                            minmax_data_frame$X35,
                            minmax_data_frame$X36,
                            minmax_data_frame$X37,
                            minmax_data_frame$X38,
                            minmax_data_frame$C01_cat,
                            minmax_data_frame$C02_cat,
                            minmax_data_frame$C03_cat1,
                            minmax_data_frame$C03_cat2,
                            minmax_data_frame$C03_cat3,
                            minmax_data_frame$C03_cat4,
                            minmax_data_frame$C04_cat1,
                            minmax_data_frame$C04_cat2,
                            minmax_data_frame$C04_cat3,
                            minmax_data_frame$C04_cat4,
                            minmax_data_frame$C05_cat,
                            minmax_data_frame$C06_cat1,
                            minmax_data_frame$C06_cat2,
                            minmax_data_frame$C06_cat3,
                            minmax_data_frame$C06_cat4)

# EXAMPLE 3: biclustering with binary data
library(blockcluster) # model-based biclustering

binarymat = as.matrix(minmax_data_frame2)

set.seed(1)
blockobject = cocluster(binarymat, datatype = "binary", nbcocluster = c(2, 54))
minmax_data_frame3 <- cbind(minmax_data_frame2, blockobject@rowclass)
str(minmax_data_frame3[,54])

set.seed(1)
blockobject = cocluster(binarymat, datatype = "binary", nbcocluster = c(4, 3))
plot(blockobject)

summary(blockobject)

blockobject@

############################################################
####################   WITH CUTOFF      ####################
############################################################

minmax_data_frame4 <- cbind(minmax_data_frame2, blockobject@rowclass)
blockobject@colclass

cat("\n\nTable showing zoo animal type (rows) and biclustering row assignment (cols):\n")
print(table(credit$class, blockobject@rowclass))

cat("\n\nRand index of agreement between partitions (corrected):\n")
print(randIndex(credit$class, 
                blockobject@rowclass, 
                correct = TRUE)) 

#Precision
(180)/(180+120) #0.6
#Recall
(180)/(180+367) #0.3290676

#      0    1
#bad   180  120
#good  367  333

# optimal cutoff for predicting bad credit set as
# (cost of false negative/cost of false positive) times
# (prevalence of positive/prevalence of negative)
# (1/5)*(.3/.7) = 0.086
COSTMATRIX

############################################################
####################   WITH CUTOFF      ####################
############################################################



cat("\n\nTable showing zoo animal type (rows) and biclustering row assignment (cols):\n")
print(table(credit$class, ifelse(blockobject@rowposteriorprob[,2] <= .086,0,1)))

cat("\n\nRand index of agreement between partitions (corrected):\n")
print(randIndex(credit$class, 
                ifelse(blockobject@rowposteriorprob[,2] <= .086,0,1), 
                correct = TRUE)) 

#Precision
(158)/(158+142) #0.5266667
#Recall
(158)/(158+337) #0.3191919

#      0    1
#bad   158  142
#good  337  363

# optimal cutoff for predicting bad credit set as
# (cost of false negative/cost of false positive) times
# (prevalence of positive/prevalence of negative)
# (1/5)*(.3/.7) = 0.086
COSTMATRIX

############################################################
####################   TRY KMEANS ON PCA      ####################
############################################################

binarydf <- as.data.frame(binarymat)

par(mfrow=c(1,1))
scree(cor(binarydf))
eigen(cor(binarydf))$val #24 PCA

pc3 <- principal(binarydf,nfactors=24,rotate="none",scores=TRUE) #rmsr = ? | fit = ?

credit$pc.01 <- pc3$scores[,1]
credit$pc.02 <- pc3$scores[,2]
credit$pc.03 <- pc3$scores[,3]
credit$pc.04 <- pc3$scores[,4]
credit$pc.05 <- pc3$scores[,5]

credit$pc.06 <- pc3$scores[,6]
credit$pc.07 <- pc3$scores[,7]
credit$pc.08 <- pc3$scores[,8]
credit$pc.09 <- pc3$scores[,9]
credit$pc.10 <- pc3$scores[,10]

credit$pc.11 <- pc3$scores[,11]
credit$pc.12 <- pc3$scores[,12]
credit$pc.13 <- pc3$scores[,13]
credit$pc.14 <- pc3$scores[,14]
credit$pc.15 <- pc3$scores[,15]

credit$pc.16 <- pc3$scores[,16]
credit$pc.17 <- pc3$scores[,17]
credit$pc.18 <- pc3$scores[,18]
credit$pc.19 <- pc3$scores[,19]
credit$pc.20 <- pc3$scores[,20]

credit$pc.21 <- pc3$scores[,21]
credit$pc.22 <- pc3$scores[,22]
credit$pc.23 <- pc3$scores[,23]
credit$pc.24 <- pc3$scores[,24]

kmean_df <- subset(credit, select = c(pc.01,pc.02,pc.03,pc.04,pc.05,
                                      pc.06,pc.07,pc.08,pc.09,pc.10,
                                      pc.11,pc.12,pc.13,pc.14,pc.15,
                                      pc.16,pc.17,pc.18,pc.19,pc.20,
                                      pc.21,pc.22,pc.23,pc.24) )


km.out=kmeans(kmean_df,2,nstart=50)

cat("\n\nTable showing zoo animal type (rows) and biclustering row assignment (cols):\n")
print(table(credit$class, km.out$cluster))

cat("\n\nRand index of agreement between partitions (corrected):\n")
print(randIndex(credit$class, km.out$cluster, correct = TRUE)) 

#Precision
(167)/(167+133) #0.5566667
#Recall
(167)/(167+351) #0.3223938

#      0    1
#bad   167  133
#good  351  349

# optimal cutoff for predicting bad credit set as
# (cost of false negative/cost of false positive) times
# (prevalence of positive/prevalence of negative)
# (1/5)*(.3/.7) = 0.086
COSTMATRIX

cl_predict(km.out, kmean_df)


