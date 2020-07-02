# Machine-Learning---RAICES

############## RAICES MODELS ##############
install.packages("e1071")
library(e1071) # for SVM
library(dplyr) # for data manipulation
library(Metrics) # for metrics
library(tree) # for trees
library(randomForest) # Random Forests
library(gbm) # Boosted trees
library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(purrr)
library(tidyverse)
install.packages('Hmisc')
library(Hmisc)
library(glmnet)
library(Metrics)
library(MASS)
library(corrplot)
install.packages('caTools') # Sampling w/ existing ratios
library(caTools) 

data1 <- read.csv("RAICES_Factors_04_19_20.csv", stringsAsFactors = T )

data2 <- read.csv("RAICES_DENIAL.csv", stringsAsFactors = T)

# POE.Denial = Days waiting after being denied, POE.Denied = if days > 0 = 1 else 0

data2$POE.denied <- NA

for (i in 1:nrow(data2)) {
  if(data2$POE.denial[i] > 1) {
    data2$POE.denied[i] <- 1
  } else data2$POE.denied[i] <- 0 
} 


data <- data1 %>% left_join(data2, by ="cid")

write.csv(data, 'RAICES_TABLEAU.csv', row.names = F)

data <- data[-1]

class.list <- sapply(data, class)
class.list.character <- names(class.list[which(class.list=="character")])
data[class.list.character] <- lapply(data[class.list.character], factor)
#-------------------Convert to factors to numerics-------------------
class.list <- sapply(data, class)
class.list.factor <- names(class.list[which(class.list=="factor")])
data[class.list.factor] <- lapply(data[class.list.factor], as.numeric)

#---Data for non predictive models---
data <- data[-1]


#---Data for Sampling---#
data3 <- data %>%
  filter(!is.na(data$Outcome))
# set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(data3))
train_ind <- sample(seq_len(nrow(data3)),size = smp_size)
train <- data3[train_ind, ]
test <- head(data3[-train_ind, ],smp_size)




## First Question: Relationship of outcome to COB, where and how they crossed, 
#  rare_language? 
install.packages('corrr')
library(corrr)


cor.test(data$COB, data$Outcome)
cor.test(data$cross_city, data$Outcome)
cor.test(data$method, data$Outcome)
cor.test(data$Rare_Language, data$Outcome)

#data$cross_city_EP <- data$cross_city == 1
#data$cross_city_EP <- as.numeric(data$cross_city_EP)

#data$method_wall <- data$method == 5
#data$method_wall <- as.numeric(data$method_wall)

#data$Rare_Language_yes <- data$Rare_Language == 0
#data$Rare_Language_yes <- as.numeric(data$Rare_Language_yes)

#cor.test(data$cross_city_EP, data$Outcome)
#cor.test(data$method_wall, data$Outcome)
#cor.test(data$Rare_Language_yes, data$Outcome)

# With factors gives more significants p-values
reg1<- lm(train$Outcome ~ COB + cross_city + method + Rare_Language, data = train)
summary(reg1)




#--- Second Question:  What was the duration of being detained by CBP based on COB?
  ## Great for Tableau

#--- Third Question: Time question/ unsure if able to answer based on given data

#--- Fourth Question: Relationship/ Correlation between arrivals of specific countries
# at the time of few months before? (use external country information for this)

#--- Fifth Question: Relationship/correlation btw experiencing negative conditions and 
# length in CBP custody?

cor.test(data$Hielera_Days, data$CBP_Days)
cor.test(data$Perrera_Days, data$CBP_Days)
cor.test(data$mistr_verb, data$CBP_Days)
cor.test(data$mistreatment_, data$CBP_Days)
cor.test(data$mistr_sleep, data$CBP_Days)
cor.test(data$food_water, data$CBP_Days)
cor.test(data$split_from, data$CBP_Days)

cor.test(data$POE.denial, data$CBP_Days)

#reg1<- lm(data$CBP_Days ~ Hielera_Days + Perrera_Days +
#            mistr_verb + mistreatment_ + mistr_sleep +
#            food_water + split_from + mistr_sleep + POE.denial, data = data)
#summary(reg1)
#--- Sixth Question: Relationship/correlation btw experiencing violence & 
# being denied at the port of entry? 

cor.test(data$Hielera_Days, data$POE.denial)
cor.test(data$Perrera_Days, data$POE.denial)
cor.test(data$mistr_verb, data$POE.denial)
cor.test(data$mistreatment_, data$POE.denial)
cor.test(data$mistr_sleep, data$POE.denial)
cor.test(data$food_water, data$POE.denial)
cor.test(data$split_from, data$POE.denial)


#reg1<- lm(data$POE.denial ~ Hielera_Days + Perrera_Days +
#            mistr_verb + mistreatment_ + mistr_sleep +
#            food_water + split_from + mistr_sleep , data = data)
#summary(reg1)
#---Seventh question: Relationship/correlation between experiencing violence and where
# and how they crossed?

cor.test(data$Hielera_Days, data$cross_city)
cor.test(data$Perrera_Days, data$cross_city)
cor.test(data$mistr_verb, data$cross_city)
cor.test(data$mistreatment_, data$cross_city)
cor.test(data$mistr_sleep, data$cross_city)
cor.test(data$food_water, data$cross_city)
cor.test(data$split_from, data$cross_city)
cor.test(data$POE.denial, data$cross_city)

#reg1<- lm(data$cross_city ~ Hielera_Days + Perrera_Days +
#            mistr_verb + mistreatment_ + mistr_sleep +
#            food_water + split_from + mistr_sleep + POE.denial, data = data)
#summary(reg1)

cor.test(data$Hielera_Days, data$method)
cor.test(data$Perrera_Days, data$method)
cor.test(data$mistr_verb, data$method)
cor.test(data$mistreatment_, data$method)
cor.test(data$mistr_sleep, data$method)
cor.test(data$food_water, data$method)
cor.test(data$split_from, data$method)
cor.test(data$POE.denial, data$method)

#reg1<- lm(data$method ~ Hielera_Days + Perrera_Days +
#            mistr_verb + mistreatment_ + mistr_sleep +
#            food_water + split_from + mistr_sleep + POE.denial, data = data)
# summary(reg1)
#--- Eight Question: Percentage seperated from family?
# good for Tableau 


#---Ninth Question: Relationship/Correlation btw split family and CBP location?

cor.test(data$split_from, data$Detention_City)
cor.test(data$split_from, data$cross_city)
#reg1<- lm(data$Detention_City ~ split_from, data = data)
#summary(reg1)
## Tenth Question: Comparisson of conditions at different POEs or CBP locations (Most,least, etc mistreatment 
#or lack of foos and water) Good for Tableau 


# 11th Question: Relationship/Correlation btw POE denial and POE locations?

cor.test(data$POE.denial, data$cross_city)

#reg1<- lm(data$POE.denial ~ cross_city, data = data)
#summary(reg1)
# probabilty of being granted a positive outcome

# Create the SVM model
svmfit=svm(Outcome ~., data=train, type="C-classification",kernel="radial", cost=1)
# get the summary
summary(svmfit)
# get the best model by tuning the parameter C
tune.out=tune(svm,as.factor(Outcome) ~.,data=train,kernel="radial",type="C-classification",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

# now do the same for the test set
pred=predict(bestmod,newdata=test)
t<-table(true=test$Outcome,Predicted= pred)
t
accuracy_test<-(t[1,1]+t[2,2])/sum(t)
accuracy_test
True_positive<-t[2,2]/(t[2,1]+t[2,2])
false_positive<-t[1,2]/(t[1,1]+t[1,2])
True_positive
false_positive
Kappa<-ScoreQuadraticWeightedKappa(test$Outcome, pred,min.rating =0, max.rating =1)
Kappa


#----TEST Random Forest----------------------------------------------------------------------
for_1<- randomForest(as.factor(Outcome) ~.,data= train)
Prediction_3 <- predict(for_1, newdata= test)

t<-table(true=test$Outcome,Predicted= Prediction_3)
t
accuracy_test<-(t[1,1]+t[2,2])/sum(t)
accuracy_test
True_positive<-t[2,2]/(t[2,1]+t[2,2])
false_positive<-t[1,2]/(t[1,1]+t[1,2])
True_positive
false_positive
Kappa<-ScoreQuadraticWeightedKappa(test$Outcome, Prediction_3,min.rating =0, max.rating =1)
Kappa


#----Train Bag w/17 -----------------------------------------------------------
#bag_2<-randomForest(SalePrice ~.,Training_Inner, mtry=17, importance= TRUE)
#bag_2
#yhat.2 <- predict(bag_2,newdata=Test_Inner)
#rmse(log(Test_Inner$SalePrice),log(yhat.2)) # = .1394
#---test bag w/ 17----------------------------------------------------------------
bag_2<-randomForest(as.factor(Outcome) ~.,train, mtry=8, importance= TRUE)
bag_2
yhat.2 <- predict(bag_2,newdata=test)

t<-table(true=test$Outcome,Predicted= yhat.2)
t
accuracy_test<-(t[1,1]+t[2,2])/sum(t)
accuracy_test
True_positive<-t[2,2]/(t[2,1]+t[2,2])
false_positive<-t[1,2]/(t[1,1]+t[1,2])
True_positive
false_positive
Kappa<-ScoreQuadraticWeightedKappa(test$Outcome, yhat.2,min.rating =0, max.rating =1)
Kappa

#---Train Boosting------------------------------------------------------------
#boost_1<-gbm(SalePrice~.,data=Training_Inner,n.trees=1000)
#boost_1
#pred.boost<-predict.gbm(boost_1,newdata=Test_Inner,n.trees=1000,type="response")
#rmse(log(Test_Inner$SalePrice),log(pred.boost)) # = .1474
#----Test Boosting ------------------------------------------------------------
boost_1<-gbm(Outcome ~.,data=train,n.trees=1000)
boost_1
pred.boost<-predict.gbm(boost_1,newdata=test,n.trees=1000, type = "response")
#rmse(log(Test_Inner$SalePrice),log(pred.boost)) # 

pred.boost<- ifelse(pred.boost<=0.5,0,1)
accuracy<-mean(pred.boost==test$Outcome)

accuracy


t<-table(true=test$Outcome,Predicted= pred.boost)
t
accuracy_test<-(t[1,1]+t[2,2])/sum(t)
accuracy_test
True_positive<-t[2,2]/(t[2,1]+t[2,2])
false_positive<-t[1,2]/(t[1,1]+t[1,2])
True_positive
false_positive
Kappa<-ScoreQuadraticWeightedKappa(test$Outcome, yhat.2,min.rating =0, max.rating =1)
Kappa

#---Plain lm using non factor actual test set-----------------------------------------
reg1<- lm(train$Outcome~., data = train)
summary(reg1)
Prediction_1<- predict(reg1, newdata= test)

Prediction_1<- ifelse(pred.boost<=0.5,0,1)
t<-table(true=test$Outcome,Predicted= Prediction_1)
t
accuracy_test<-(t[1,1]+t[2,2])/sum(t)
accuracy_test
True_positive<-t[2,2]/(t[2,1]+t[2,2])
false_positive<-t[1,2]/(t[1,1]+t[1,2])
True_positive
false_positive
Kappa<-ScoreQuadraticWeightedKappa(test$Outcome, Prediction_1 ,min.rating =0, max.rating =1)
Kappa


# Real Elastic Net---------------------------------------------------------------------
y_1 <- as.matrix(train$Outcome)
x_1 <- as.matrix(train[,-27])
xtest_1 <-as.matrix(test[,-27])
ytest_1 <-as.matrix(test$Outcome)
lambda_1 <-cv.glmnet(x_1,y_1)
best.lambda_1 <- lambda_1$lambda.min


model_en_1 <- glmnet(x_1,y_1, alpha = 0, standardize = TRUE)
y_hat_en_1 <- predict(model_en_1, newx = xtest_1, s = best.lambda_1)

y_hat_en_1<- ifelse(pred.boost<=0.5,0,1)
t<-table(true=test$Outcome,Predicted= y_hat_en_1)
t
accuracy_test<-(t[1,1]+t[2,2])/sum(t)
accuracy_test
True_positive<-t[2,2]/(t[2,1]+t[2,2])
false_positive<-t[1,2]/(t[1,1]+t[1,2])
True_positive
false_positive
Kappa<-ScoreQuadraticWeightedKappa(test$Outcome, y_hat_en_1 ,min.rating =0, max.rating =1)
Kappa

#---Kaggle hybrid-----------------------------------------------------------------------
set.seed(123)
model_2_1 <- lm(formula = Outcome ~ .,data=train)
# Perform the feature selection
step <- stepAIC(model_2_1, direction="both") #"forward" | "backward" |"both"
step$anova # display results
reg1<- lm(train$Outcome ~ Hielera_Days + COB + immune + mistr_hygiene +
            Detention_Count + CBP_Days + Detention_City + cross_city +
            behavioral + gastrointestinal + POE.denial + POE.denied + 
            ICE_Days + food_water + up_respiratory + med_deten + pregnant + mistr_verb, data = train)
summary(reg1)
Prediction_1 <- predict(reg1, newdata= test)

Prediction_1<- ifelse(pred.boost<=0.5,0,1)
t<-table(true=test$Outcome,Predicted= Prediction_1)
t
accuracy_test<-(t[1,1]+t[2,2])/sum(t)
accuracy_test
True_positive<-t[2,2]/(t[2,1]+t[2,2])
false_positive<-t[1,2]/(t[1,1]+t[1,2])
True_positive
false_positive
Kappa<-ScoreQuadraticWeightedKappa(test$Outcome,Prediction_1 ,min.rating =0, max.rating =1)
Kappa





