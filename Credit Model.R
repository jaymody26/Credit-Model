install.packages("MASS")
library(MASS)

setwd("C:/Baruch College/Baruch Fall 2020/FIN 9854 Risk/Project")
credit <- read.csv("Training50.csv")
str(credit)

# Model with all independent variables

LogisticModel.1 <- glm(Creditability ~., data=credit, family=binomial)
summary(lm(LogisticModel.1))

# Model with backward stepwise regression

AIC1 = stepAIC(LogisticModel.1,credit, direction = "backward")
summary(lm(AIC1))

library(ROCR)

# AUC with all variables

fitLog1 <- predict(LogisticModel.1, data=credit, type="response")
pred1 <- prediction(fitLog1, credit$Creditability)
perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")

AUCLog1 <- performance(pred1, measure = "auc")@y.values[[1]]
AUCLog1
plot(perf1)

# AUC with less variables

fitLog2 <- predict(AIC1, data=credit, type="response")
pred2 <- prediction(fitLog2, credit$Creditability)
perf2 <- performance(pred2, measure = "tpr", x.measure = "fpr")

AUCLog2 <- performance(pred2, measure = "auc")@y.values[[1]]
AUCLog2
plot(perf2)

####Linear Discrimant Analysis

test <- read.csv("Test50.csv")

ldafit <- lda(Creditability ~., data =test)
ldafit
lda.pred <- predict(ldafit, data=test)
ldaclass <- lda.pred$class
table(ldaclass,test$Creditability)
