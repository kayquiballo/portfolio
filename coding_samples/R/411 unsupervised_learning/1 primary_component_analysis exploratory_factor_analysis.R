################################################################################
####################               Data Prep                ####################
################################################################################

# Use the power of R utilities for quick data preparation
# prepared by Thomas W. Miller
# initial version January 29, 2021

# ensure that the foreign package has been installed
# to run the jump-start code, type this into the console:
#     source("assignment-1-jump-start-v001.R")

library(foreign)
# begin by reading in character variables as factors
# there are a few warning messages regarding demographic items
# sdensity, age, hhl, and hh3
# but these items are not critical to the initial analysis
# we have planned for this assignment
pewdata = read.spss("Mar19public.sav", use.value.labels = TRUE,
    to.data.frame = TRUE)
print(str(pewdata)) 

# check on column indices for political opinion variables
print(names(pewdata)[26]) # this is q1
print(names(pewdata)[98]) # this is q77f2

# identifying the variables relating to political opinion
pewopinion = pewdata[,26:98] # covers q1 to q77f2

# a quick check shows there are no complete cases across all opinion items
ok = complete.cases(pewopinion)
cat("\nNumber of original cases with complete data:", sum(ok))
cat("\nNumber of original cases with incomplete data:", sum(!ok))

# see if there is a subset of opinion items with complete data
item_complete = c() # initialize list of names of items
for (item in seq(along = names(pewopinion))) {
   if (sum(complete.cases(pewopinion[,item])) == nrow(pewopinion))
       item_complete = c(item_complete, names(pewopinion)[item])
}
cat("\n\nNumber of items with complete data:",length(item_complete)) 

# variable names for items with complete data
cat("\nNames of items with complete data:\n", item_complete)

# check status of working data frame including items with complete data
# looks like we have 30 multi-category factor items to work with
pewwork = pewopinion[,item_complete]
print(str(pewwork))

# build design matrix formula from variable names in item_complete
design_string = paste("~ q1 + q2 + q19 + q20 + q25 + q47 + q50a + q50b +", 
"q50c + q50d + q50e + q58 + q60 + q61a + q61b +", 
"q61c + q64 + q65a + q65b + q65c + q65d + q65e +", 
"q66 + q68a + q68b + q68d + q69 + q70 + q71 + q75")

# fast way to create binary indicator variables for all items with complete data
# this converts the 30 multi-category items into 100 binary indictor variables
pewmat = model.matrix(formula(design_string), data = pewwork)
print(str(pewmat))

pewdf = as.data.frame(pewmat)
print(str(pewdf))

# now we are ready to try principal components or factor analysis or whatever
# using the matrix pewmat or the data frame pewdf

# Of course, we may want to review the items in the matrix or data frame,  
# ensuring that they make sense for input to our analysis.
# We may want to attend to individual items, recoding items
# where appropriate (with recode from the CAR package), 
# eliminating selected items, or whatever makes sense.

################################################################################
#################       Section 1: variable selection      #####################
################################################################################

ls(pewdf)
#Don't include "Don't know/Refused" in exploratory analyses
q01_2 <- pewdf[,"q1Dissatisfied"]
q01_9 <- pewdf[,"q1(VOL) Don't know/Refused"]

q02_2 <- pewdf[,"q2Disapprove"]
q02_9 <- pewdf[,"q2(VOL) Don't know/Refused"]

q19_2 <- pewdf[,"q19Some of the time"]
q19_3 <- pewdf[,"q19Only now and then"]
q01_4 <- pewdf[,"q19Hardly at all"]
q01_9 <- pewdf[,"q19(VOL) Don't know/Refused"]

q20_2 <- pewdf[,"q20Frustrated"]
q20_3 <- pewdf[,"q20Angry"]
q20_9 <- pewdf[,"q20(VOL) Don't know/Refused"]
                                             
q25_2 <- pewdf[,"q25Most of the time"]
q25_3 <- pewdf[,"q25Only some of the time"]
q25_4 <- pewdf[,"q25(VOL) Never"]
q25_9 <- pewdf[,"q25(VOL) Don't know/Refused"]
                                      
q47_2 <- pewdf[,"q47No, not keeping promises"]
q47_9 <- pewdf[,"q47(VOL) Don't know/Refused"]

q50a_2 <- pewdf[,"q50aStatement #2"]
q50a_5 <- pewdf[,"q50a(VOL) Neither/Both equally"]
q50a_9 <- pewdf[,"q50a(VOL) Don't know/Refused"]

q50b_2 <- pewdf[,"q50bStatement #2"]
q50b_5 <- pewdf[,"q50b(VOL) Neither/Both equally"]
q50b_9 <- pewdf[,"q50b(VOL) Don't know/Refused"]

q50c_2 <- pewdf[,"q50cStatement #2"]
q50c_5 <- pewdf[,"q50c(VOL) Neither/Both equally"]
q50c_9 <- pewdf[,"q50c(VOL) Don't know/Refused"]

q50d_2 <- pewdf[,"q50dStatement #2"]
q50d_5 <- pewdf[,"q50d(VOL) Neither/Both equally"]
q50d_9 <- pewdf[,"q50d(VOL) Don't know/Refused"]

q50e_2 <- pewdf[,"q50eStatement #2"]
q50e_5 <- pewdf[,"q50e(VOL) Neither/Both equally"]
q50e_9 <- pewdf[,"q50e(VOL) Don't know/Refused"]

q58_2 <- pewdf[,"q58Favor"]
q58_3 <- pewdf[,"q58Oppose" ]
q58_4 <- pewdf[,"q58Strongly oppose"]
q58_9 <- pewdf[,"q58(VOL) Don't know/Refused"]

q60_2 <- pewdf[,"q60Never fair game"]
q60_8 <- pewdf[,"q60(VOL) Other/Depends"]
q60_9 <- pewdf[,"q60(VOL) Don't know/Refused"]

q61a_2 <- pewdf[,"q61aSome"]
q61a_3 <- pewdf[,"q61aOnly a little"]
q61a_4 <- pewdf[,"q61aNone at all"]
q61a_9 <- pewdf[,"q61a(VOL) Don't know/Refused"]

q61b_2 <- pewdf[,"q61bSome"]
q61b_3 <- pewdf[,"q61bOnly a little"]
q61b_4 <- pewdf[,"q61bNone at all"]
q61b_9 <- pewdf[,"q61b(VOL) Don't know/Refused"]

q61c_2 <- pewdf[,"q61cSome"]
q61c_3 <- pewdf[,"q61cOnly a little"]
q61c_4 <- pewdf[,"q61cNone at all"]
q61c_9 <- pewdf[,"q61c(VOL) Don't know/Refused"]

q64_2 <- pewdf[,"q64Moderately fair"]
q64_3 <- pewdf[,"q64Not too fair" ]
q64_4 <- pewdf[,"q64Not fair at all"]
q64_9 <- pewdf[,"q64(VOL) Don't know/Refused"]

q65a_2 <- pewdf[,"q65aSome"]
q65a_3 <- pewdf[,"q65aNot too much"]
q65a_4 <- pewdf[,"q65aNot at all"]
q65a_9 <- pewdf[,"q65a(VOL) Don't know/Refused"]

q65b_2 <- pewdf[,"q65bSome"]
q65b_3 <- pewdf[,"q65bNot too much"]
q65b_4 <- pewdf[,"q65bNot at all"]
q65b_9 <- pewdf[,"q65b(VOL) Don't know/Refused"]

q65c_2 <- pewdf[,"q65cSome"]
q65c_3 <- pewdf[,"q65cNot too much"]
q65c_4 <- pewdf[,"q65cNot at all"]
q65c_9 <- pewdf[,"q65c(VOL) Don't know/Refused"]

q65d_2 <- pewdf[,"q65dSome"]
q65d_3 <- pewdf[,"q65dNot too much"]
q65d_4 <- pewdf[,"q65dNot at all"]
q65d_9 <- pewdf[,"q65d(VOL) Don't know/Refused"]

q65e_2 <- pewdf[,"q65eSome"]
q65e_3 <- pewdf[,"q65eNot too much"]
q65e_4 <- pewdf[,"q65eNot at all"]
q65e_9 <- pewdf[,"q65e(VOL) Don't know/Refused"]

q66_2 <- pewdf[,"q66Losing influence"]
q66_3 <- pewdf[,"q66(VOL) Same" ]
q66_9 <- pewdf[,"q66(VOL) Don't know/Refused"]

q68a_2 <- pewdf[,"q68aUnfriendly toward religion"]
q68a_3 <- pewdf[,"q68aNeutral toward religion"]
q68a_9 <- pewdf[,"q68a(VOL) Don't know/Refused"]

q68b_2 <- pewdf[,"q68bUnfriendly toward religion"]
q68b_3 <- pewdf[,"q68bNeutral toward religion"]
q68b_9 <- pewdf[,"q68b(VOL) Don't know/Refused"]

q68d_2 <- pewdf[,"q68dUnfriendly toward religion"]
q68d_3 <- pewdf[,"q68dNeutral toward religion"]
q68d_9 <- pewdf[,"q68d(VOL) Don't know/Refused"]

q69_2 <- pewdf[,"q69Should express their views on day-to-day social and political questions"]
q69_8 <- pewdf[,"q69(VOL) Other/Depends"] ########### all 0's - omit this variable
q69_9 <- pewdf[,"q69(VOL) Don't know/Refused"]

q70_2 <- pewdf[,"q70Disapprove" ]
q70_9 <- pewdf[,"q70(VOL) Don't know/Refused"]

q71_2 <- pewdf[,"q71Somewhat well"]
q71_3 <- pewdf[,"q71Not too well" ]
q71_4 <- pewdf[,"q71Not at all well"]
q71_9 <- pewdf[,"q71(VOL) Don't know/Refused"]

q75_2 <- pewdf[,"q75A little" ]
q75_3 <- pewdf[,"q75Nothing at all"]
q75_9 <- pewdf[,"q75(VOL) Don't know/Refused"]


#q1 + q2 + q19 + q20 + q25 + q47 + q50a + q50b + q50c + q50d + q50e + q58 + q60 + q61a + q61b
#q61c + q64 + q65a + q65b + q65c + q65d + q65e + q66 + q68a + q68b + q68d + q69 + q70 + q71 + q75
pew_full <- cbind.data.frame(q01_2, q01_9, q02_2, q02_9, q19_2, q19_3, q01_4, q01_9, q20_2, q20_3, q20_9, q25_2, q25_3, q25_4, q25_9, q47_2, q47_9, q50a_2, q50a_5, q50a_9, q50b_2, q50b_5, q50b_9, q50c_2, q50c_5, q50c_9, q50d_2, q50d_5, q50d_9, q50e_2, q50e_5, q50e_9, q58_2, q58_3, q58_4, q58_9, q60_2, q60_8, q60_9, q61a_2, q61a_3, q61a_4, q61a_9, q61b_2, q61b_3, q61b_4, q61b_9, q61c_2, q61c_3, q61c_4, q61c_9, q64_2, q64_3, q64_4, q64_9, q65a_2, q65a_3, q65a_4, q65a_9, q65b_2, q65b_3, q65b_4, q65b_9, q65c_2, q65c_3, q65c_4, q65c_9, q65d_2, q65d_3, q65d_4, q65d_9, q65e_2, q65e_3, q65e_4, q65e_9, q66_2, q66_3, q66_9, q68a_2, q68a_3, q68a_9, q68b_2, q68b_3, q68b_9, q68d_2, q68d_3, q68d_9, q69_2, q69_9, q70_2, q70_9, q71_2, q71_3, q71_4, q71_9, q75_2, q75_3, q75_9)
pew_subset <- cbind.data.frame(q01_2, q02_2, q19_2, q19_3, q01_4, q20_2, q20_3, q25_2, q25_3, q25_4, q47_2, q50a_2, q50a_5, q50b_2, q50b_5, q50c_2, q50c_5, q50d_2, q50d_5, q50e_2, q50e_5, q58_2, q58_3, q58_4, q60_2, q60_8, q61a_2, q61a_3, q61a_4, q61b_2, q61b_3, q61b_4, q61c_2, q61c_3, q61c_4, q64_2, q64_3, q64_4, q65a_2, q65a_3, q65a_4, q65b_2, q65b_3, q65b_4, q65c_2, q65c_3, q65c_4, q65d_2, q65d_3, q65d_4, q65e_2, q65e_3, q65e_4, q66_2, q66_3, q68a_2, q68a_3, q68b_2, q68b_3, q68d_2, q68d_3, q69_2, q70_2, q71_2, q71_3, q71_4, q75_2, q75_3)

######################################################
#################       PCA      #####################
######################################################

##### FULL DATASET #####

body.cor_all <- cor(pew_full)

fa.parallel(pew_full,fa="pc",n.iter=100,show.legend=FALSE,main="Screeplot wtih parallel analysis")
#"Parallel analysis suggests that the number of factors =  NA  and the number of components =  28"
fa.parallel(body.cor_all, n.obs=1503, fa="both", n.iter=100, show.legend=TRUE, main="Scree plot with parallel analysis")
#"Parallel analysis suggests that the number of factors =  33  and the number of components =  28"
scree(body.cor_all) #indicates how many components to keep (above dotted line)
#scree plot indicates possibly 3 groups

Z<-eigen(body.cor_all)
Z$val #eigenvalues bigger than 1 rule (ignore, 39 eigvals > 1) 
# 3 vals are relatively higher than the others 5.88, 5.48, 3.08, 2.43, 2.33...)

#include loadings above .3-.5
#cumulative variance should be 90-95%
#no need to rotate 
#RMSR – Root Mean Square Residual (absolute fit) A value less than . 08 is generally considered a good fit.
principal(body.cor_all,nfactors=2,rotate="none",scores=TRUE) #rmsr = .06 | fit = 0.58
principal(body.cor_all,nfactors=3,rotate="none",scores=TRUE) #rmsr = .05 | fit = 0.63
principal(body.cor_all,nfactors=28,rotate="none",scores=TRUE) #rmsr = .04 | fit = 0.83

######### filtered pca - full #########
#loadings > 0.5 (0.3 had too many variables to make any meaningful findings)
filter_pca_full_3 <- pew_full[, c("q01_2", "q02_2", "q50d_2", "q50d_9", "q61a_9", "q61b_9", "q61c_9", "q68a_2", "q70_2")]
#no loadings in PC3 > 0.4

body.cor_all3 <- cor(filter_pca_full_3)
fa.parallel(body.cor_all3, n.obs=1503, fa="both", n.iter=100, show.legend=TRUE, main="Scree plot with parallel analysis")
Z<-eigen(body.cor_all3)
Z$val
principal(body.cor_all3,nfactors=2,rotate="none",scores=TRUE) #rmsr = .06 | fit = 0.58
principal(body.cor_all3,nfactors=3,rotate="none",scores=TRUE) #rmsr = .05 | fit = 0.63
principal(body.cor_all3,nfactors=7,rotate="none",scores=TRUE) #rmsr = .05 | fit = 0.63



##### SUBSETTED DATASET #####

body.cor_subset <- cor(pew_subset)

fa.parallel(pew_subset,fa="pc",n.iter=100,show.legend=FALSE,main="Screeplot wtih parallel analysis")
#"Parallel analysis suggests that the number of factors =  NA  and the number of components =  24 "
fa.parallel(body.cor_subset, n.obs=1503, fa="both", n.iter=100, show.legend=TRUE, main="Scree plot with parallel analysis")
###USE THIS SCREE (ABOVE)
#Parallel analysis suggests that the number of factors =  29  and the number of components =  24
scree(body.cor_subset) #indicates how many components to keep (above dotted line)
#scree plot indicates possibly 3 groups

Z<-eigen(body.cor_subset)
Z$val #eigenvalues bigger than 1 rule (ignore, 39 eigvals > 1) 
# 3 vals are relatively higher than the others 5.66, 3.10, 2.28, 2.14, 1.97...)

#include loadings above .3-.5
#cumulative variance should be 90-95%
#no need to rotate 
#RMSR – Root Mean Square Residual (absolute fit) A value less than . 08 is generally considered a good fit.
principal(body.cor_subset,nfactors=2,rotate="none",scores=TRUE) #rmsr = .06 | fit = 0.60
principal(body.cor_subset,nfactors=3,rotate="none",scores=TRUE) #rmsr = .06 | fit = 0.63
principal(body.cor_subset,nfactors=24,rotate="none",scores=TRUE) #rmsr = .04 | fit = 0.81

#need to explore full dataset (2,3 factors)--> filtered full (2,3 factors)
#need to explore subset dataset (2,3 factors)--> filtered full (2,3 factors)

#higher relative loadings, PC3 has 1 loading > 0.4
filter_pca_subset_3 <- pew_full[, c("q01_2", "q02_2", "q47_2", "q50a_2", "q50b_2", 
                                    "q50d_2", "q61a_2", "q61b_2", "q64_2", "q65c_4", "q68a_2", "q68a_3", "q70_2")]

#### filtered pca - subset
body.cor_subset3 <- cor(filter_pca_subset_3)
fa.parallel(body.cor_subset3, n.obs=1503, fa="both", n.iter=100, show.legend=TRUE, main="Scree plot with parallel analysis")
Z<-eigen(body.cor_subset3)
Z$val
principal(body.cor_subset3,nfactors=2,rotate="none",scores=TRUE) #rmsr = .06 | fit = 0.58
principal(body.cor_subset3,nfactors=3,rotate="none",scores=TRUE) #rmsr = .05 | fit = 0.63
principal(body.cor_subset3,nfactors=4,rotate="none",scores=TRUE) #rmsr = .05 | fit = 0.63



#### filtered pca - subset - test using loadings > 0.5
filter_pca_subset_4 <- pew_full[, c("q01_2", "q02_2", "q50a_2", "q50d_2", "q61a_2", "q61b_2", "q68a_2", "q70_2")]

body.cor_subset4 <- cor(filter_pca_subset_4)
fa.parallel(body.cor_subset4, n.obs=1503, fa="both", n.iter=100, show.legend=TRUE, main="Scree plot with parallel analysis")
Z<-eigen(body.cor_subset4)
Z$val
principal(body.cor_subset4,nfactors=2,rotate="none",scores=TRUE) #rmsr = .06 | fit = 0.58
principal(body.cor_subset4,nfactors=3,rotate="none",scores=TRUE) #rmsr = .05 | fit = 0.63
principal(body.cor_subset4,nfactors=6,rotate="none",scores=TRUE) #rmsr = .05 | fit = 0.63


######################################################
#################       EFA      #####################
######################################################


##### Full #####

#pew_full
hsb.cor <- cor(pew_full)
#eigen(hsb.cor)$val
#fa.parallel(hsb.cor, n.obs=1503, fa="both", n.iter=100, show.legend=TRUE,main="Scree plot with parallel analysis")

fa(pew_full, nfactors=3, rotate="none", fm="pa")
fa(pew_full, nfactors=3, rotate="none", fm="ml")
fa(pew_full, nfactors=3, rotate="varimax", fm="pa")
fa(pew_full, nfactors=3, rotate="varimax", fm="ml")

fa(pew_full, nfactors=2, rotate="none", fm="pa")
fa(pew_full, nfactors=2, rotate="none", fm="ml")
fa(pew_full, nfactors=2, rotate="varimax", fm="pa")
fa(pew_full, nfactors=2, rotate="varimax", fm="ml")

fa(pew_full, nfactors=1, rotate="none", fm="pa")
fa(pew_full, nfactors=1, rotate="none", fm="ml")
fa(pew_full, nfactors=1, rotate="varimax", fm="pa")
fa(pew_full, nfactors=1, rotate="varimax", fm="ml")

##### Full - filtered #####
filter_efa_full_3 <- pew_full[, c("q01_2", "q02_2", "q50d_2", "q50d_9", "q61a_9", "q61b_9", "q61c_9", "q68a_2", "q70_2")]
hsb.cor <- cor(filter_efa_full_3)
eigen(hsb.cor)$val

fa(filter_efa_full_3, nfactors=3, rotate="none", fm="pa")
fa(filter_efa_full_3, nfactors=3, rotate="varimax", fm="pa") #PA better BIC
fa(filter_efa_full_3, nfactors=3, rotate="none", fm="ml") 
fa(filter_efa_full_3, nfactors=3, rotate="varimax", fm="ml")

fa(filter_efa_full_3, nfactors=2, rotate="none", fm="pa") #PA better BIC
fa(filter_efa_full_3, nfactors=2, rotate="varimax", fm="pa") 
fa(filter_efa_full_3, nfactors=2, rotate="none", fm="ml") 
fa(filter_efa_full_3, nfactors=2, rotate="varimax", fm="ml")

##### Subset #####

pew_subset

fa(pew_subset, nfactors=3, rotate="none", fm="pa")
fa(pew_subset, nfactors=3, rotate="none", fm="ml")
fa(pew_subset, nfactors=3, rotate="varimax", fm="pa")
fa(pew_subset, nfactors=3, rotate="varimax", fm="ml")

fa(pew_subset, nfactors=2, rotate="none", fm="pa")
fa(pew_subset, nfactors=2, rotate="none", fm="ml")
fa(pew_subset, nfactors=2, rotate="varimax", fm="pa")
fa(pew_subset, nfactors=2, rotate="varimax", fm="ml")

fa(pew_subset, nfactors=1, rotate="none", fm="pa")
fa(pew_subset, nfactors=1, rotate="none", fm="ml")
fa(pew_subset, nfactors=1, rotate="varimax", fm="pa")
fa(pew_subset, nfactors=1, rotate="varimax", fm="ml")

##### Subset - filtered #####

filter_efa_subset_3 <- pew_full[, c("q01_2", "q02_2", "q25_2", "q25_3", "q25_4", "q70_2")]
hsb.cor <- cor(filter_efa_subset_3)
eigen(hsb.cor)$val

fa(filter_efa_subset_3, nfactors=3, rotate="none", fm="ml") 
fa(filter_efa_subset_3, nfactors=3, rotate="varimax", fm="ml") #this one

fa(filter_efa_subset_3, nfactors=2, rotate="none", fm="ml") 
fa(filter_efa_subset_3, nfactors=2, rotate="varimax", fm="ml") #this one ****

fa(filter_efa_subset_3, nfactors=1, rotate="varimax", fm="ml") #this one

filter_efa_subset_4 <- pew_full[, c("q01_2", "q02_2", "q70_2")]
fa(filter_efa_subset_4, nfactors=1, rotate="none", fm="pa") #this one
fa(filter_efa_subset_4, nfactors=1, rotate="none", fm="ml")
fa(filter_efa_subset_4, nfactors=1, rotate="varimax", fm="pa")
fa(filter_efa_subset_4, nfactors=1, rotate="varimax", fm="ml")

########### resources for pca

hsb.cor <- cor(mydata1)
hsb.cor

Z<-eigen(hsb.cor)
Z$val #eigenvalues bigger than 1 rule - retain 2 factors
Z$vec

fa.parallel(hsb.cor, n.obs=600, fa="both", n.iter=100,
            show.legend=TRUE,main="Scree plot with parallel analysis")

#just plug and chug to test
fa1<-fa(mydata1, nfactors=3, rotate="none", fm="pa")
fa1
fa2<-fa(mydata1, nfactors=3, rotate="varimax", fm="pa")
fa2
fa3<-fa(mydata1, nfactors=2, rotate="varimax", fm="pa")
fa3
fa4<-fa(mydata1, nfactors=2, rotate="varimax", fm="ml")
fa4
fa5<-fa(mydata1, nfactors=1, rotate="varimax", fm="pa")
fa5
#optional - after looking at single FA, narrow down to loadings > 0.5 before using for dataset.





#
#
#
#
#
#
#                     space  :O
#
#
#
#
#
#

###################################################################################
#################                   Resources                  ####################
###################################################################################

#################      t-SNE (week 1)     ####################

# t-Distributed Stochastic Neighbor Embedding [t-SNE] 

# import libraries
library(tidyverse)
library(Rtsne)

###########################
# Data load and prep
###########################

# load data - select bodyfat.csv
tsne_data <- read.csv("bodyfat.csv", header = TRUE)

###########################
# Exploratory Data Analysis
# Data Prep
###########################

# review range of variables and ensure no N/As exist
summary(tsne_data)

#data to use: pewdf and pewmat

#dims - the output dimensionality
#perplexity - generally a value between 5 and 50, 
  #and is considered the smooth measure of the effective number of neighbors
#verbose - generate output while training
#max_iter - the number of iterations to use for training
#eta - the learning rate (default is 200)

# using rtsne
set.seed(1) # for reproducibility
tsne <- Rtsne(pewdf, dims = 2, perplexity=30, verbose=TRUE, max_iter = 5000, learning = 200)

# visualizing
colors = rainbow(length(unique(tsne_data$agegroup)))
names(colors) = unique(tsne_data$agegroup)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=2, cex.lab=1.5)
text(tsne$Y, labels = tsne_data$agegroup, col = colors[tsne_data$agegroup])

# train and plot using different parameters
tsne_plot <- function(perpl=30,iterations=500,learning=200){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(tsne_data %>% select(-Density, -agegroup), dims = 2, perplexity=perpl, verbose=TRUE, max_iter=iterations, eta=learning)
  plot(tsne$Y, t='n', main = print(paste0("perplexity = ",perpl, ", max_iter = ",iterations, ", learning rate = ",learning)), xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=1, cex.lab=1.5)
  text(tsne$Y, labels = tsne_data$agegroup, col = colors[tsne_data$agegroup])
}

perplexity_values <- c(2,5,30,50,80)
sapply(perplexity_values, function(i){tsne_plot(perpl=i)})
tsne_plot(perpl=2)
tsne_plot(perpl=5)
tsne_plot(perpl=30)
tsne_plot(perpl=50) #best
tsne_plot(perpl=80) 

learning_values <- c(20,200,2000)
sapply(learning_values,function(i){tsne_plot(learning=i)})
tsne_plot(learning=20)
tsne_plot(learning=200)
tsne_plot(learning=2000)

#################          PCA (week 2)          ####################

bodyfat <- tsne_data <- read.csv("bodyfat2.csv", header = TRUE)
mydata<-data.frame(bodyfat)

library(psych)
library(lessR)

y1<-mydata$Density
y2<-mydata$percent_BF
x1<-mydata$Age
x2<-mydata$agegroup
x3<-mydata$Weight
x4<-mydata$Height
x5<-mydata$neck_cir
x6<-mydata$chest_cir
x7<-mydata$ab_cir
x8<-mydata$hip_cir
x9<-mydata$thigh_cir
x10<-mydata$knee_cir
x11<-mydata$ankle_cir
x12<-mydata$bicep_cir
x13<-mydata$forearm_cir
x14<-mydata$wrist_cir

plot(x=x4,y=x3)
abline(lm(x3~x4))

mydata1<-cbind.data.frame(x1,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14)
body.cor <- cor(mydata1)
body.cor

fa.parallel(mydata1[,-1],fa="pc",n.iter=100,show.legend=FALSE,main="Screeplot wtih parallel analysis")
scree(body.cor) #indicates how many components to keep (above dotted line)

Z<-eigen(body.cor)
Z$val #eigenvalues bigger than 1 rule - retain 2 components
Z$vec

#include loadings above .3-.5
#cumulative variance should be 90-95%
pc1<-principal(body.cor,nfactors=2,rotate="none",scores=TRUE)
pc1
pc1$scores

#this version yields the components for each value in dataset
pc2<-principal(mydata1,nfactors=2,rotate="none",scores=TRUE)
pc2
head(pc2$scores)
pc2$scores

#PC1 = 0.02x1 + 0.98x3 + 1.43x4 + 0.86x5 + 0.89x6 + 0.88x7 + 0.92x8 + 0.88x9 + 0.88x10 + 0.66x11 + 0.85x12 + 0.71x13 + 0.79x14
#PC2 = 0.87x1 - 0.02x3 - 0.58x4 + 0.14x5 + 0.27x6 + 0.32x7 + 0.00x8 - 0.14x9 - 0.08x10 - 0.26x11 - 0.03x12 - 0.12x13 + 0.12x14

#################          EFA (week 2)          ####################

library(psych)
library(lessR)

HSB <- read.csv("HSB.csv", header = TRUE)
mydata<-data.frame(HSB)

x1<-mydata$sex
x2<-mydata$race
x3<-mydata$ses
x4<-mydata$sctyp
x5<-mydata$hsp
x6<-mydata$locus
x7<-mydata$concept
x8<-mydata$mot
x9<-mydata$car
x10<-mydata$rdg
x11<-mydata$wrtg
x12<-mydata$math
x13<-mydata$sci
x14<-mydata$civ

#removed demographics - no assumed traits related to test scores
mydata1<-cbind.data.frame(x6,x7,x8,x10,x11,x12,x13,x14)

hsb.cor <- cor(mydata1)
hsb.cor

Z<-eigen(hsb.cor)
Z$val #eigenvalues bigger than 1 rule - retain 2 factors
Z$vec

fa.parallel(hsb.cor, n.obs=600, fa="both", n.iter=100,
            show.legend=TRUE,main="Scree plot with parallel analysis")

#just plug and chug to test
fa1<-fa(mydata1, nfactors=3, rotate="none", fm="pa")
fa1
fa2<-fa(mydata1, nfactors=3, rotate="varimax", fm="pa")
fa2
fa3<-fa(mydata1, nfactors=2, rotate="varimax", fm="pa")
fa3
fa4<-fa(mydata1, nfactors=2, rotate="varimax", fm="ml")
fa4
fa5<-fa(mydata1, nfactors=1, rotate="varimax", fm="pa")
fa5
#optional - after looking at single FA, narrow down to loadings > 0.5 before using for dataset.

#################          MDS (week 3)          ####################

# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name
.rowNamesDF(mydata1,make.names=TRUE) <-mydata1$x2
d1 <- dist(mydata1) # euclidean distances between the rows
d1
fit <- cmdscale(d1, eig=TRUE, k=2) # k is the number of dim
fit # view results
# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(mydata1), cex=.7)
# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name
library(MASS)
d2 <- dist(mydata1) # euclidean distances between the rows
fit <- isoMDS(d2, k=2) # k is the number of dim
fit # view results
# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(mydata1), cex=.7)

##### Final visualization

filter_efa_full_3 <- pew_full[, c("q01_2", "q02_2", "q50d_2", "q50d_9", "q61a_9", "q61b_9", "q61c_9", "q68a_2", "q70_2")]
hsb.cor1 <- cor(filter_efa_full_3)
pc1<-principal(filter_efa_full_3,nfactors=2,rotate="none",scores=TRUE)
head(pc1$scores)
filter_efa_full_4<-cbind(filter_efa_full_3, pc1$scores, pewdata$partysum)
filter_efa_full_4$partysum <- filter_efa_full_4[, "pewdata$partysum"]
plot(filter_efa_full_4$PC1, filter_efa_full_4$PC2, col=filter_efa_full_4$partysum)
legend(-1,9,unique(filter_efa_full_4$partysum),col=1:length(filter_efa_full_4$partysum),pch=1)
title(main="PCA - 2 Components - Full Dataset")

filter_efa_subset_3 <- pew_full[, c("q01_2", "q02_2", "q25_2", "q25_3", "q25_4", "q70_2")]
hsb.cor2 <- cor(filter_efa_subset_3)
pc2<-principal(filter_efa_subset_3,nfactors=2,rotate="none",scores=TRUE)
head(pc2$scores)
filter_efa_subset_4<-cbind(filter_efa_subset_3, pc2$scores, pewdata$partysum)
filter_efa_subset_4$partysum <- filter_efa_subset_4[, "pewdata$partysum"]
plot(filter_efa_subset_4$PC1, filter_efa_subset_4$PC2, col=filter_efa_subset_4$partysum)
legend(-1,9,unique(filter_efa_subset_4$partysum),col=1:length(filter_efa_subset_4$partysum),pch=1)
title(main="PCA - 2 Components - Subset Dataset")

library(psych)
filter_pca_full_3 <- pew_full[, c("q01_2", "q02_2", "q50d_2", "q50d_9", "q61a_9", "q61b_9", "q61c_9", "q68a_2", "q70_2")]
M = cor(filter_pca_full_3)
corrplot(M,  method="number")

filter_pca_subset_4 <- pew_full[, c("q01_2", "q02_2", "q50a_2", "q50d_2", "q61a_2", "q61b_2", "q68a_2", "q70_2")]
M = cor(filter_pca_subset_4)
corrplot(M,  method="number")
