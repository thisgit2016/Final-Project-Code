

#Install and Call Necessary Packages

install.packages("corrplot")
library(corrplot)

install.packages('ggplot2')
library(ggplot2)

install.packages('plotrix')
library(plotrix)

install.packages('MASS')
library(MASS)

install.packages('leaps')
library(leaps)


install.packages('glmnet')
library(glmnet)

#Import Dataset

NHSData <- read.csv('C:/Users/L/Desktop/NHSData.csv', stringsAsFactors = FALSE)

summary(NHSData)

dim(NHSData)

#Data type of attributes
sapply(NHSData, class)
sapply(NHSData, mode)

#Checking for null values
sum(is.na(NHSData))


#Selecting needed columns (14)
NHSData2 <- NHSData[c(3,4,5,6,19,23,34,36,37,38,40,41,44,45)]


#Changing column names for corrplot to function well

colnames(NHSData2) <- c('CH','YO', 'SE','TV','NV','RI','UE','NE','CD','UG','PO','PR','AR','AI')
summary(NHSData2)
str(NHSData2)

#Find Pearson Correlations

cor(NHSData2, method="pearson")

#Plotting Pearson Correlations

corrplot(cor(NHSData2), method = "ellipse")


# Histogram for each attribute 

par(mfrow=c(3,3))
par(mar = rep(2, 4))
for(i in 1:14) { 
  hist(NHSData2[,i], main=names(NHSData2)[i],col="red" ) 

}

# Boxplot for each attribute

par(mfrow=c(3,3))
par(mar = rep(2, 4))
for(i in 1:14) { 
  boxplot(NHSData2[,i], main=names(NHSData2)[i], col="blue") 
}

# Boxplot statistics for Average Income and Average Rent
 
boxplot.stats(NHSData2$AI, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(NHSData2$AR, coef = 1.5, do.conf = TRUE, do.out = TRUE)


#Pie Chart of Age Groups

C <- sum(NHSData2$CH)
Y <- sum(NHSData2$YO)
A <- sum(NHSData[,2]-NHSData[,3]-NHSData[,4]-NHSData[,5])
S <- sum(NHSData2$SE)
slices <- c(C,Y,A,S) 
lbls <- c("(0-14)", "(15-24)","(25-64)" ,"(65+)")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"\n", pct) 
lbls <- paste(lbls,"%",sep="")  
pie3D(slices,labels=lbls,radius = 0.8,explode=0.1,start=0.05,labelcex=1.1,shade=0.5,main="Pie Chart of Age Gruops ")

#Pie Chart of Education Groups

N <- sum(NHSData2$NE)
C <- sum(NHSData2$CD)
U <- sum(NHSData2$UG)
O <- sum(NHSData[,2]-NHSData[,36]-NHSData[,37]-NHSData[,38])
slices <- c(N, C, U,O) 
lbls <- c("(No Higher Ed.)", "(College Ed.)", "(Under Grad Ed.)","(Other)")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, "\n", pct) 
lbls <- paste(lbls,"%",sep="")  
pie3D(slices,labels=lbls,radius = 0.8,explode=0.1,start=0.05,labelcex=1.1,shade=0.5,main="Pie Chart of Education Groups")

#Pie Chart of Visible Minority

T <- sum(NHSData2$TV)
V <- sum(NHSData2$NV)
slices <- c(T, V) 
lbls <- c("(Visible Minority)", "(Non-Visible Minority)")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"\n", pct) 
lbls <- paste(lbls,"%",sep="") 
pie3D(slices,labels=lbls,radius = 0.8,explode=0.1,start=2,labelcex=1.1,shade=0.5,main="Pie Chart of Visbile Minority ")

#Pie Chart of Tenure

O <- sum(NHSData2$PO)
R <- sum(NHSData2$PR)
slices <- c(O, R) 
lbls <- c("(Owner)", "(Renter)")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"\n", pct) 
lbls <- paste(lbls,"%",sep="")  
pie3D(slices,labels=lbls,radius = 0.8,explode=0.1,start=2,labelcex=1.1,shade=0.5,main="Pie Chart of Tenure ")

#Building a Model to predict Income
#Split dataset into training and test sets
set.seed(123)
rn_train <- sample(nrow(NHSData2),floor(nrow(NHSData2)*0.7))

# Training the model:

train <- NHSData2[rn_train,] 
nrow(train)

# Testing the model:

test <- NHSData2[-rn_train,]
nrow(test)


# Building full Model with selected variables

model_mlr <- lm(AI~CH+YO+SE+TV+NV+RI+UE+NE+CD+UG+PO+PR+AR, data=train)
summary(model_mlr)

#Get prediction:

prediction <- predict(model_mlr, interval="prediction",newdata =test)

errors <- prediction[,"fit"] - test$AI

hist (errors)

#Root mean square (RMSE) calculation:

rmse <- sqrt(sum((prediction[,"fit"] - test$AI)^2)/nrow(test))
paste("RMSE:", rmse)

#Therefore, on average we have error of $21000 of Average After Tax Houshold Income for each prediction

#PRED25 Calculations:

rel_change <- abs(errors) / test$AI
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("PRED(25):", pred25)

#76% of samples have less than 25% of error


#Stepwise linear regression - Forward


full <- lm(AI~CH+YO+SE+TV+NV+RI+UE+NE+CD+UG+PO+PR+AR, data=NHSData2)

null <- lm(AI~1,data=NHSData2)

stepF <- stepAIC(null, scope=list(lower=null, upper=full),direction= "forward", trace=TRUE)

summary(stepF)


#Stepwise linear regression - Backward


full <- lm(AI~CH+YO+SE+TV+NV+RI+UE+NE+CD+UG+PO+PR+AR, data=NHSData2)

stepB <- stepAIC(full, direction= "backward", trace=TRUE)

summary(stepB)

#Finding best combination of attributes or kitchen sink through exhaustive search


subsets <- regsubsets(AI~CH+YO+SE+TV+NV+RI+UE+NE+CD+UG+PO+PR+AR,data=NHSData2,nbest=2)

sub.sum <- summary(subsets)

as.data.frame(sub.sum$outmat)

#Method 2 to address overfitting
#Checking for Regularized Liner Regression 


cv.fit <- cv.glmnet(as.matrix(NHSData2[,c("CH","YO","SE","TV","NV","RI","UE","NE","CD","UG","PO","PR","AR")]),as.vector(NHSData2[,"AI"]), nlambda=100)

plot(cv.fit)


#Therefore for aprroximate variable range 7,8 the error is not changing much so no overfitting is observed and reqularization is not needed.


# Training the model:

train <- NHSData2[rn_train,c("AI","RI","AR")] 
nrow(train)

# Testing the model:

test <- NHSData2[-rn_train,c("AI","RI","AR")]
nrow(test)

# Building  Model with best variables: RI and AR

model_mlr <- lm(AI~RI+AR, data=train)
summary(model_mlr)

#Get prediction:

prediction <- predict(model_mlr, interval="prediction",newdata =test)

errors <- prediction[,"fit"] - test$AI

hist (errors)

#Root mean square (RMSE) calculation:

rmse <- sqrt(sum((prediction[,"fit"] - test$AI)^2)/nrow(test))
paste("RMSE:", rmse)

#Therefore, on average we have error of $22000 of Average After Tax Houshold Income for each prediction

#PRED25 Calculations:

rel_change <- abs(errors) / test$AI
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("PRED(25):", pred25)

#78% of samples have less than 25% of error


################################################
install.packages('scatterplot3d')
library(scatterplot3d) 
attach(NHSData2) 
s3d <-scatterplot3d(RI,AR,AI, pch=16, highlight.3d=TRUE,type="h", main="3D Scatterplot, Average Income ~ Average Rent + Recent Immigrants")
fit <- lm(AI ~ AR+RI) 
s3d$plane3d(fit)
##############################################

install.packages('Rcmdr')
library(Rcmdr)
attach(NHSData2)
scatter3d(AR, AI, RI)
########################################


install.packages('rgl')
library(rgl)

plot3d(RI, AR, AI, col="red", size=3)
###########################################