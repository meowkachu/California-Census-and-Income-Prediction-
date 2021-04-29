#packages
library(dplyr)
library(finalfit)
library(psych)
library(Hmisc)# Description of the data
library(car)#rename the levels in a factor
library(xlsx)
library(ggplot2)
library(tidyr)
library(purrr)
library(caret)#dummy
library(GGally)#corr matrix
library(ROSE)
library(ggthemes)
library(e1071)
library(kernlab)
library(pROC)
library(ROCR)
library(doParallel)
library(rpart)
library(rpart.plot)
library(Boruta)
library(mlbench)
library(randomForest)


#load population dataset
firstdata <- read.csv("ss13pusa.csv")
head(firstdata)

######################################Data Preprocessing##################################

#filter out data that are outside of California
calidata <- filter(firstdata, ST == '6')
#filter out data that are "unemployed"
edata_ca <- filter(calidata, !COW == '9')

head(edata_ca)
str(edata_ca)
#filter out work hours less than 20
edata_ca <- filter(edata_ca, WKHP >=20)
#filter out age older than 18
edata_ca <- filter(edata_ca, AGEP > 18 & AGEP < 60)
#filter out self care diffucult
edata_ca <- filter(edata_ca, DDRS >1)
#filter out less than high school degree
edata_ca <- filter(edata_ca, SCHL >15)
#filter out Hearing diffuculty
edata_ca <- filter(edata_ca, DEAR >1)
#filter out vision difficulty
edata_ca <- filter(edata_ca, DEYE >1)
#filter out independent living difficulty
edata_ca <- filter(edata_ca, DOUT >1)
#filter out ambulatory difficulty
edata_ca <- filter(edata_ca, DPHY >1)


#variales that will be be used
#ST - State
#AGEP(Continuous) - Age
#CIT(Categorical) _ Citizenship Status
#COW(Categirical) - Class of Worker
#DDRS(Catedorical) - Self-care difficulty
#LANX(Categorical) - Language other than English spoken at home 
#MAR(Categorical) - Martial status
#SCHL(Categorical) - Education attainment
#SEX(Categorical) - Gender
#Nativity(Categorical) - (Native/Foreign born)
#PINCP(Continous) -Total Person's Income
#QTRBIR(Categorical) - Quater of Birth
#WKHP(Continious) - Work hours per week
#RAC1P(Categorcal)
#SCIENGP(Categorical) - Field of Degree Science and Engineering Related Flag – NSF Definition
#OCCP(Categorixal) - industry


#selected dataset
sedata_ca <- select(edata_ca, AGEP,CIT,COW,LANX,MAR,SCHL,SEX,
                    NATIVITY,PINCP,QTRBIR,WKHP,RAC1P,SCIENGP,OCCP)
str(sedata_ca)
head(sedata_ca)

#adjust the plot size
par(mfrow = c(1,1))
par(mar=c(3,2,3,2))
#check missing data
sedata_ca %>%
  ff_glimpse()
sedata_ca %>%
  missing_plot()
sedata_ca %>%
  missing_pattern()

sedata_ca %>%
  or_plot('stem','eduLevel', matrix = TRUE,table_text_size=4, title_text_size=14,
plot_opts=list(xlab("OR, 95% CI"), theme(axis.title = element_text(size=12))))
#Data Preparation
#Missing data are those who don't have bachelor's degree.
#There is 65.9% missing data in SCIENGP column, 
#but they can be replacedby 0, which means 'no'. 

sedata_ca$SCIENGP[is.na(sedata_ca$SCIENGP)] <- 0
sedata_ca$SCIENGP[sedata_ca$SCIENGP == 2] <- 0
#convert no stem degree records to 0, now 0 represent people who don't have 
#stem degrees, and 1 represent people who have stem degrees

#check missing data and pattern
sedata_ca %>%
  missing_plot()

#examine the dataset
options(scipen=999)#not to use sicence notation
ggplot(sedata_ca,aes(x=PINCP))+geom_histogram(binwidth =3000, fill="lightblue") +
  labs(title = "Income Distribution",
       subtitle = "PINCP",
       caption = "Data from the 2013 American Community Survey.",
       x = "Income",
       y = "Count") +
  theme_solarized_2()
describe(sedata_ca)

#histgrams of each variable
par(mfrow = c(1,1))
multi.hist(sedata_ca, freq = TRUE, bcol = "lightblue")

#Data Transformation
#check high income
filter(sedata_ca, PINCP > 250000)
#remove high income
sedata_ca <- sedata_ca[!(sedata_ca$PINCP > 250000),]
#break income data into two groups
sedata_ca["incomeClass"] <-  cut(sedata_ca$PINCP, br =c(-1,50000,250000), 
                                 labels = c("below50K", "above50K"))
sedata_ca %>%
  missing_plot()
sedata_ca <- na.omit(sedata_ca)#some values are negative 
table(sedata_ca$incomeClass)
#narrow down the levels of 'CIT', only show if the person is a U.S. citizen or not
sedata_ca["cStatus"] <-  cut(sedata_ca$CIT, br =c(-1,4, 5), 
                             labels = c("Yes", "No"))

#create a factor of working class, and rename the levels
sedata_ca$wClass <- as.factor(as.character(sedata_ca$COW))
sedata_ca$wClass <- recode(sedata_ca$wClass," '1' = 'em_p_profit'; '2' = 'em_p_nprofit';
                           '3' = 'loc_gov'; '4' = 'sta_gov'; '5' = 'fed_gov'; '6' = 'self_not_incor';
                           '7' = 'self_incor'; '8' = 'without_pay'")

#create a factor of language spoken at home, and rename the levels
sedata_ca$oEnglish <- as.factor(as.character(sedata_ca$LANX))
sedata_ca$oEnglish <- recode(sedata_ca$oEnglish, "'2' = 'Yes'; '1' = 'No'")
#narrow down the levels of 'AR', only show married or not married
sedata_ca["sMarried"] <-  cut(sedata_ca$MAR, br =c(-1,1, 5),labels = c("Yes","No"))

#reduce education level from 24 levels to 6 levels
sedata_ca["eduLevel"] <-  cut(sedata_ca$SCHL, br =c(-1,19,20,21,22,23,24), 
                              labels = c("no_degree", "associate","bachelor","master","professional","doc"))
#Create a factor of Sex, and rename the levels.
sedata_ca$gender <- as.factor(as.character(sedata_ca$SEX))
sedata_ca$gender <- recode(sedata_ca$gender, "'1' = 'Male';
                           '2' = 'Female'")
#Create a factor of Nativity, indicating if the peraion was born in the U.S. or foreign countries. 
sedata_ca$nBorn <- as.factor(as.character(sedata_ca$NATIVITY))
sedata_ca$nBorn <- recode(sedata_ca$nBorn, "1 = 'Yes'; 2 = 'No'")

#Create a factor of birth month, and rename the levels.
sedata_ca$bMonth <- as.factor(as.character(sedata_ca$QTRBIR))
sedata_ca$bMonth <- recode(sedata_ca$bMonth, " 1 = 'Jan_Mar'; 2 = 'Apr_June';
                           3 = 'Jul_Sep'; 4 = 'Oct_Dec'")
#Create a factor of stem degree, and rename the levels.
sedata_ca$stem <- as.factor(as.character(sedata_ca$SCIENGP))
sedata_ca$stem <- recode(sedata_ca$stem,"0 = 'No'; '1' = 'Yes'")
#Create a factor of race, and rename the levels.
#Reduce the levels from 9 to 6, group American Indian and Alaska
#and Pacific Islands togther
#shift the order of orginal data set
sedata_ca$RAC1P[sedata_ca$RAC1P == 6] <- 10
sedata_ca$RAC1P[sedata_ca$RAC1P == 7] <- 6
sedata_ca$RAC1P[sedata_ca$RAC1P == 10] <- 7
sedata_ca$race <- cut(sedata_ca$RAC1P, br = c(-1,1,2,6,7,8,9),
                      labels = c("White", "Black","Ind_Alas_Pac","Asian", "Other", "Two_or_More"))
#Create a factor of the industries, group data based on industry names.
sedata_ca["industry"] <- cut(sedata_ca$OCCP, br = c(-1,450,760,960,1260,1580,1980,2080,2180,
                                                    2280,2950,3580,3680,3980,4180,4280,4680,
                                                    4980,5980,6180,6780,6980,7680,8980,9780,
                                                    9850),
                             labels = c("MGR","BUS","FIN","CMM","ENG","SCI","CMS","LGL",
                                        "EDU","ENT","MED","HLS","PRT","EAT","CLN","PRS",
                                        "SAL","OFF","FFF","CON","EXT","RPR","PRD","TRN","MIL"))


cdata <- select(sedata_ca, AGEP, WKHP, PINCP, incomeClass, cStatus,
                wClass, oEnglish, sMarried, eduLevel,
                gender, nBorn, bMonth, stem, race, industry)

cdata %>%
  ff_glimpse()
cdata %>%
  missing_plot()'
'
ggplot(cdata, aes(x=incomeClass)) +
         geom_bar( width=0.7, fill="lightblue") +
  theme_bw()
       

#Explore data
par(mfrow = c(1,1))
expl <- names(cdata[,  !names(cdata) %in% PINCP])
dfun = function(x){
  ggplot(cdata, aes(x = .data[[x]])) +
    geom_bar(aes(fill = incomeClass)) +
    theme_bw() +
    labs(x = x) +
    theme(axis.text.x = element_text(angle = 45)) 
}
dfun("AGEP")
expl_plts = map(expl, ~ dfun(.x))
expl_plts[8]#change the index to see bigger plot of an individual variable
#combining plots
cowplot::plot_grid(plotlist = expl_plts)

#explore the correlations
#convert factors to dummy variables
dmy <- dummyVars("~.", data = cdata)
ddata <- data.frame(predict(dmy, newdata = cdata))
ggcorr(ddata,name = "corr", label = TRUE, hjust =1, label_size = 2, angle = 0, size = 2,
       low = "darkblue", mid = "white", high = "darkblue")
#remove nBorn because of its high correlations with other variables
#remove rMonth because it doesn't have any impact
cdata <- select(cdata, AGEP, WKHP, PINCP, incomeClass, cStatus,
                wClass, oEnglish, sMarried, eduLevel,
                gender, stem, race, industry)

#export the new data file
write.csv(cdata, "cdata.csv")
table(cdata$incomeClass)

set.seed(555)
#seperate the dataset to test and train
#80%--train, 20%--test
trainIndex <- createDataPartition(cdata$incomeClass, p = .8, 
                                  list = FALSE, 
                                  times = 1)

#create training set subset (80%)
incomeTrain <- cdata[ trainIndex,]

#create training set subset (20%)
incomeTest  <- cdata[-trainIndex,]

table(incomeTrain$incomeClass)
table(incomeTest$incomeClass)

#datasets for classsification models
classTrain <- select(incomeTrain, -PINCP)
classTest <- select(incomeTest, -PINCP)

#datasets for regression models
regreTrain <- select(incomeTrain, -incomeClass)
regreTest <- select(incomeTest, -incomeClass)

#original dataset is imbalanced, we use oversampling here
library(ROSE)

table(classTrain$incomeClass)

set.seed(555)
underTr <- ovun.sample(incomeClass~., data = classTrain, method = "under", N = 73708)$data
table(underTr$incomeClass)
#underTr will be used for classification models, predict on incomeClass

set.seed(555)
overTr <- ovun.sample(incomeClass~., data = classTrain, method = "over", N = 114346)$data
table(overTr$incomeClass)
#overTr will be used for classification models, predict on incomeClass
library(doParallel)
cl <- makePSOCKcluster(5)
clusterSetRNGStream(cl, 555) #set seed for everymember of cluster
registerDoParallel(cl)

library(DMwR)

#hybrid both up and down
set.seed(555)
smoteTr <- SMOTE(incomeClass ~ ., data  = classTrain)                         
table(smoteTr$incomeClass) 
#smoteTr will be used for classification models, predict on incomeClass

#dummy variables for regression models
dmyTr <- dummyVars("~.", data = regreTrain)
dTrain <- data.frame(predict(dmyTr, newdata = regreTrain))

dmyTe <- dummyVars("~.", data = regreTest)
dTest <- data.frame(predict(dmyTe, newdata = regreTest))

#dTrain and dTest will be used for regression models, predict on PINCP

#cross validation
#cv for classification models
set.seed(555)
ctrl <- trainControl(method="cv", number=5,
                     classProbs=TRUE,
                     summaryFunction = twoClassSummary, 
                     allowParallel =  TRUE,
                     savePredictions=TRUE)
#cv for regression models
set.seed(555)
ctrl.r <- trainControl(method="cv", number=5,
                       summaryFunction = defaultSummary, 
                       allowParallel =  TRUE,
                       savePredictions=TRUE)

#####################################Model Building####################################

################################Regression Models###################################

ggcorr(ddata,name = "corr", label = TRUE, hjust =1, label_size = 2, angle = 0, size = 2,
       low = "darkblue", mid = "white", high = "darkblue")
#highest correlastion is with sMarried

#build linear regression model, X=age, Y=logPINCP
linear.fit1<- lm(PINCP ~ AGEP, data=dTrain)  
print(linear.fit1)
summary(linear.fit1)
par(mfrow=c(2,2))
plot(linear.fit1)
#Adjusted R^2 is 0.114, model is bad
#From the residual plot, we can see some points are further away from y axis and may be potential outliers
#The points in the QQ plot almost lie on the straight line, except the front and tail.
#Overall, simple linerar regression is not considered as a good model for this analysis


#Multiple Linear Regression
linear.fit2<- lm(PINCP ~., data=dTrain)  
print(linear.fit2)
summary(linear.fit2)
par(mfrow=c(2,2))
plot(linear.fit2)
#R^2 is 0.4508


#RMSE
#result for the train set
rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse.fit2.train <- rmse(fitted(linear.fit2), dTrain$PINCP)
rmse.fit2.train#32232
#test for the test set
rmse.fit2.test <- rmse(predict(linear.fit2, dTest), dTest$PINCP)
rmse.fit2.test#32207
#Performance is not much worse on the test set

#It is quite likely that not all 57 variables are necessary to make a good prediction
#In fact, some of them might add white nosie, 
#we can improve that by using the default stepwise model selection
step.fit2 <- step(linear.fit2)
summary(step.fit2)#0.451
cross.step.fit2 <- cv.lm(dTrain, step.fit2, m=5) 
#result for the train set
rmse.stepfit2.train <- rmse(cross.step.fit2$cvpred, dTrain$PINCP)
rmse.stepfit2.train#32251
#test for the test set
rmse.stepfit2.test <- rmse(predict(step.fit2,dTest), dTest$PINCP)
rmse.stepfit2.test#32206

###########################Polynomial Regression##########################

plot(dTrain$AGEP, dTrain$PINCP, pch=20, col="gray")
polymodel1 <- lm(PINCP~AGEP+I(AGEP^3)+I(AGEP^2), data = dTrain)
summary(polymodel1)
#R squared = 0.148
#result for the train set
rmse.poly1.train <- rmse(fitted(polymodel1), dTrain$PINCP)
rmse.poly1.train#40153
#test for the test set
rmse.poly1.test <- rmse(predict(polymodel1, dTest), dTest$PINCP)
rmse.poly1.test#40201

plot(dTrain$WKHP, dTrain$PINCP, pch=20, col="gray")
polymodel2 <- lm(PINCP~WKHP+I(WKHP^3)+I(WKHP^2), data = dTrain)
summary(polymodel2)
#R squared = 0.171
#result for the train set
rmse.poly2.train <- rmse(fitted(polymodel2), dTrain$PINCP)
rmse.poly2.train#39614
#test for the test set
rmse.poly2.test <- rmse(predict(polymodel2, dTest), dTest$PINCP)
rmse.poly2.test#39584

plot(dTrain$sMarried.No, dTrain$PINCP, pch=20, col="gray")
polymodel3 <- lm(PINCP~sMarried.No+I(sMarried.No^3)+I(sMarried.No^2), data = dTrain)
summary(polymodel3)
#R squared = 0.0668
#result for the train set
rmse.poly3.train <- rmse(fitted(polymodel3 ), dTrain$PINCP)
rmse.poly3.train#42028
#test for the test set
rmse.poly3.test <- rmse(predict(polymodel3, dTest), dTest$PINCP)
rmse.poly3.test#41967


#####################################Naive Bayes#######################################

nb.tune1 <-  train(incomeClass ~ ., data=underTr, method="nb", trControl = ctrl,
                   metric="ROC") 
plot(nb.tune1)
nb.tune1
pred.nb1 <- predict(nb.tune1, classTest)
result.pred.nb1 <- confusionMatrix(pred.nb1, classTest$incomeClass, positive = 'below50K')
result.pred.nb1$byClass#
roc.curve(classTest$incomeClass, pred.nb1, add.roc = TRUE, col =7, lwd = 1, lty = 1)
#auc-0.766
#Plot Variable performance
var1 <- varImp(nb.tune1)
plot(var1)


nb.tune2 <-  train(incomeClass ~ ., data=overTr, method="nb", trControl = ctrl,
                   metric="ROC") 
plot(nb.tune2)
pred.nb2 <- predict(nb.tune2, classTest)
result.pred.nb2 <- confusionMatrix(pred.nb2, classTest$incomeClass, positive = 'below50K')
result.pred.nb2$byClass
roc.curve(classTest$incomeClass, pred.nb2, add.roc = TRUE, col =7, lwd = 1, lty = 1)
#auc-0.764
#Plot Variable performance
var2 <- varImp(nb.tune2)
plot(var1)


nb.tune3 <-  train(incomeClass ~ ., data=smoteTr, method="nb", trControl = ctrl,
                   metric="ROC") 
plot(nb.tune3)
pred.nb3 <- predict(nb.tune3, classTest)
result.pred.nb3 <- confusionMatrix(pred.nb3, classTest$incomeClass, positive = 'below50K')
result.pred.nb3$byClass#
roc.curve(classTest$incomeClass, pred.nb3, add.roc = TRUE, col =7, lwd = 1, lty = 1)
#auc-0.600
#Plot Variable performance
var3 <- varImp(nb.tune3)
plot(var3)



##############################Decision Trees for a quick look#################################
#Using underTr 
tree.fit1 <- rpart(incomeClass ~., data = underTr, control=rpart.control(minsplit=1), method="class", 
                   parms=list(split='information'))
pred.tree1 <- predict(tree.fit1,classTest,type=c("class"))
#confusion matrix for evaluating model
result.pred.tree1 <- confusionMatrix(pred.tree1, classTest$incomeClass, positive = 'below50K')
roc.curve(classTest$incomeClass, pred.tree1, add.roc = TRUE, col =9, lwd = 1, lty = 1)
#auc0.764

#visualize decision tree
#basic tree
prp(tree.fit1)
#with color
rpart.plot(tree.fit1)
plotcp(tree.fit1)

#pruning the tree
opt.cp <- tree.fit1$cptable[which.min(tree.fit1$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree.fit1, cp=opt.cp)
pred.tree.p <- predict(tree.pruned,classTest,type=c("class"))
result.pred.treep <- confusionMatrix(pred.tree.p, classTest$incomeClass, positive = 'below50K')
roc.curve(classTest$incomeClass, pred.tree.p, add.roc = TRUE, col =9, lwd = 1, lty = 1)
#visualize decision tree
rpart.plot(tree.pruned)
#auc-0.764, it's better


#############################Logistic Regression Models#############################

set.seed(555)
glm.fit1 <- train(incomeClass ~ .,data = underTr,trControl = ctrl,
                  method = "glm",metric="ROC",family=binomial())
pred.glm1 <- predict(glm.fit1, classTest)
result.pred.glm1 <- confusionMatrix(pred.glm1, classTest$incomeClass, positive = 'below50K')
result.pred.glm1$byClass
roc.curve(classTest$incomeClass, pred.glm1, add.roc = FALSE, col =6, lwd = 1, lty = 1)
#auc-0.780

set.seed(555)
glm.fit2 <- train(incomeClass ~ .,data = overTr,trControl = ctrl,
                  method = "glm",family=binomial())
pred.glm2 <- predict(glm.fit2, classTest)
result.pred.glm2 <- confusionMatrix(pred.glm2, classTest$incomeClass, positive = 'below50K')
result.pred.glm2$byClass
roc.curve(classTest$incomeClass, pred.glm2, add.roc = TRUE, col =7, lwd = 1, lty = 1)
#auc-0.781

set.seed(555)
glm.fit3 <- train(incomeClass ~ .,data = smoteTr,trControl = ctrl,
                  method = "glm",family=binomial())
pred.glm3 <- predict(glm.fit3, classTest)
result.pred.glm3 <- confusionMatrix(pred.glm3, classTest$incomeClass, positive = 'below50K')
result.pred.glm3$byClass#
roc.curve(classTest$incomeClass, pred.glm3, add.roc = TRUE, col =8, lwd = 1, lty = 1)
#auc-0.738


#######################################Support Vector Machines####################################
#make 5 clusters
cl <- makeCluster(5)
registerDoParallel(cl)

#reduce train size for tunning,svms take too long to run
set.seed(555)
underSample <- classTrain[1:10000,]
table(underSample$incomeClass)#samples are balanced
radial.tune <- tune.svm(incomeClass~., data = underSample,
                     gamma = 10^(-6:-3), cost = 10^(1:2), kernal = 'radial', pred.glm5,preProc = c("center", "scale"))
saveRDS(radial.tune, "radial_tune.rds")
summary(radial.tune)#best-gamma= 0.001, cost=100
radial.best <- radial.tune$best.model
#evaluate results
pred.radial.best <- predict(radial.best, classTest)
result.radial.best <- confusionMatrix(pred.radial.best, classTest$incomeClass, positive = 'below50K')
result.radial.best$byClass
roc.curve(classTest$incomeClass, pred.radial.best, add.roc = FALSE, col =1, lwd = 1, lty = 1)
#auc 0.782


set.seed(555)
poly.tune <- tune.svm(incomeClass~., data = underSample,
                     gamma = 10^(-6:-3), cost = 10^(1:2), kernel = 'polynomial')
saveRDS(poly.tune, "poly_tune.rds")
summary(poly.tune)
poly.best <- poly.tune$best.model
pred.poly.best <- predict(poly.best, classTest)
result.poly.best <- confusionMatrix(pred.poly.best, classTest$incomeClass, positive = 'below50K')
result.poly.best$byClass
roc.curve(classTest$incomeClass, pred.poly.best, add.roc = TRUE, col =2, lwd = 1, lty = 1)
#model is bad, auc-0.5


set.seed(555)
sigmoid.tune <- tune.svm(incomeClass~., data = underSample,
                      gamma = 10^(-6:-3), cost = 10^(1:2), kernel = 'sigmoid')
saveRDS(sigmoid.tune, "sigmoid_tune.rds")
summary(sigmoid.tune)
sigmoid.best <- sigmoid.tune$best.model
pred.sigmoid.best <- predict(sigmoid.best, classTest)
result.sigmoid.best <- confusionMatrix(pred.sigmoid.best, classTest$incomeClass, positive = 'below50K')
result.sigmoid.best$byClass
roc.curve(classTest$incomeClass, pred.sigmoid.best, add.roc = TRUE, col =3, lwd = 3, lty = 1)
#model makes random choie, auc-0.773


set.seed(555)
linear.tune <- tune.svm(incomeClass~., data = underSample,
                         gamma = 10^(-6:-3), cost = 10^(-1:2), kernel = 'linear')
saveRDS(linear.tune, "linear_tune.rds")
summary(linear.tune)
linear.best <- linear.tune$best.model
pred.linear.best <- predict(linear.best, classTest)
result.linear.best <- confusionMatrix(pred.linear.best, classTest$incomeClass, positive = 'below50K')
result.linear.best$byClass
roc.curve(classTest$incomeClass, pred.linear.best, add.roc = TRUE, col =5, lwd = 1, lty = 1)
#auc-0.774
saveRDS(radial.tune2, "radial_tune2.rds")


###############################################KNN############################################

knn.tune <-  train(incomeClass ~ ., data=underTr, method="knn", 
                   metric="ROC", trControl=ctrl, tuneLength=10, preProc = c("center", "scale") ) 
plot(knn.tune)
knn.tune#23
pred.knn <- predict(knn.tune, classTest)
result.pred.knn <- confusionMatrix(pred.knn, classTest$incomeClass, positive = 'below50K')
result.pred.knn$byClass
roc.curve(classTest$incomeClass, pred.knn, add.roc = TRUE, col =7, lwd = 1, lty = 1)
#auc-0.778
#other two training sets are bigger and took too long too run, so we won't include them in the analysis

##############################Classification Models Evaluation####################################

#SVMs
roc.curve(classTest$incomeClass, pred.radial.best, add.roc = FALSE, col =2, lwd =2, lty = 1)#0.782
roc.curve(classTest$incomeClass, pred.poly.best, add.roc = TRUE, col =3, lwd = 2, lty = 1)#0.500
roc.curve(classTest$incomeClass, pred.sigmoid.best, add.roc = TRUE, col =4, lwd = 2, lty = 1)#0.773
roc.curve(classTest$incomeClass, pred.linear.best, add.roc = TRUE, col =5, lwd = 2, lty = 1)#0.774
legend("right", c("radial.best(best)", "poly.best", "sigmoid.best", "linear.best"), lty=1, 
       col = c(2,3,4,5), bty="n", inset=c(0,-0.15))

#Logistic Regressions
roc.curve(classTest$incomeClass, pred.glm1, add.roc = FALSE, col =2, lwd = 1, lty = 1)#0.780
roc.curve(classTest$incomeClass, pred.glm2, add.roc = TRUE, col =3, lwd = 1, lty = 1)#0.781
roc.curve(classTest$incomeClass, pred.glm3, add.roc = TRUE, col =4, lwd = 1, lty = 1)#0738.
legend("right", c("pred.glm1", "pred.glm2(best)", "pred.glm3"), lty=1, 
       col = c(2,3,4), bty="n", inset=c(0,-0.15))

#KNN
roc.curve(classTest$incomeClass, pred.knn, add.roc = FALSE, col =5, lwd = 2, lty = 1)#0.778
legend("right", "KNN", lty=1, 
       col = 5, bty="n", inset=c(0,-0.15))

#Naive Bayes
roc.curve(classTest$incomeClass, pred.nb1, add.roc = FALSE, col =2, lwd = 1, lty = 1)#0.766
roc.curve(classTest$incomeClass, pred.nb2, add.roc = TRUE, col =3, lwd = 1, lty = 1)#0.764
roc.curve(classTest$incomeClass, pred.nb3, add.roc = TRUE, col =4, lwd = 1, lty = 1)#0.600
legend("right", c("pred.nb1(best)", "pred.nb2", "pred.nb3"), lty=1, 
       col = c(2,3,4), bty="n", inset=c(0,-0.15))

#Decision Trees
roc.curve(classTest$incomeClass, pred.tree1, add.roc = FALSE, col =1, lwd = 1, lty = 1)#0.764
roc.curve(classTest$incomeClass, pred.tree.p, add.roc = TRUE, col =2, lwd = 1, lty = 1)#0.764
legend("right", c("tree1", "tree2"), lty=1, 
       col = c(1,2), bty="n", inset=c(0,-0.15))


#Best of the Best
#svm
roc.curve(classTest$incomeClass, pred.radial.best, add.roc = FALSE, col =2, lwd = 1, lty = 1)#0.782
#glm
roc.curve(classTest$incomeClass, pred.glm2, add.roc = TRUE, col =3, lwd = 1, lty = 1)#0.781
#knn
roc.curve(classTest$incomeClass, pred.knn, add.roc = TRUE, col =4, lwd = 1, lty = 1)#0.778
#tree
roc.curve(classTest$incomeClass, pred.tree.p, add.roc = TRUE, col =5, lwd = 1, lty = 1)#0.764
#nb
roc.curve(classTest$incomeClass, pred.nb1, add.roc = TRUE, col =6, lwd = 1, lty = 1)#0.766
legend("right", c("SVM.radial(best)", "Logistic regression","Decision Tree", "KNN", "Naive Bayes"), lty=1, 
       col = c(2,3,4,5,6), bty="n", inset=c(0,-0.15))



