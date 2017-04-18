library(MASS)
data(biopsy)
str(biopsy)
biopsy$ID = NULL
names(biopsy) = c("thick", "u.size", "u.shape", "adhsn", 
                  "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
names(biopsy)
biopsy.v2 <- na.omit(biopsy)
y <- ifelse(biopsy.v2$class == "malignant", 1, 0)
library(reshape2)
library(ggplot2)
biop.m <- melt(biopsy.v2, id.var = "class")
ggplot(data = biop.m, aes(x = class, y = value)) + 
  geom_boxplot() +
  facet_wrap(~ variable, ncol = 3)
library(corrplot)
bc <- cor(biopsy.v2[ ,1:9]) #create an object of the features
corrplot.mixed(bc)

set.seed(123) #random number generator
ind <- sample(2, nrow(biopsy.v2), replace = TRUE, prob = c(0.7, 0.3))
train <- biopsy.v2[ind==1, ] #the training data set
test <- biopsy.v2[ind==2, ] #the test data set
str(test) #confirm it worked
table(train$class)
table(test$class)

full.fit <- glm(class ~ ., family = binomial, data = train)
summary(full.fit)
confint(full.fit)
exp(coef(full.fit))
library(car)
vif(full.fit)

train.probs <- predict(full.fit, type = "response")
train.probs[1:5] #inspect the first 5 predicted probabilities
contrasts(train$class)

library(InformationValue)
trainY <- y[ind==1]
testY <- y[ind==2]
confusionMatrix(trainY, train.probs)
# optimalCutoff(trainY, train.probs)
misClassError(trainY, train.probs)
confusionMatrix(trainY, train.probs)

test.probs <- predict(full.fit, newdata = test, type = "response")
misClassError(testY, test.probs)
confusionMatrix(testY, test.probs)
####

####

library(bestglm)
X <- train[, 1:9]
Xy <- data.frame(cbind(X, trainY))
bestglm(Xy = Xy, IC = "CV", CVArgs = list(Method = "HTF", K = 10, REP = 1), 
        family=binomial)
reduce.fit <- glm(class ~ thick + u.size + nucl, family = binomial, data = train)

test.cv.probs = predict(reduce.fit, newdata = test, type = "response")
misClassError(testY, test.cv.probs)
confusionMatrix(testY, test.cv.probs)

bestglm(Xy = Xy, IC = "BIC", family = binomial)
bic.fit <- glm(class ~ thick + adhsn + nucl + n.nuc, 
               family = binomial, data = train)
test.bic.probs = predict(bic.fit, newdata = test, type = "response")
misClassError(testY, test.bic.probs)
confusionMatrix(testY, test.bic.probs)
####

lda.fit <- lda(class ~ ., data = train)
lda.fit
plot(lda.fit, type="both")
train.lda.probs <- predict(lda.fit)$posterior[, 2]
misClassError(trainY, train.lda.probs)
confusionMatrix(trainY, train.lda.probs)

test.lda.probs <- predict(lda.fit, newdata = test)$posterior[, 2]
misClassError(testY, test.lda.probs)
confusionMatrix(testY, test.lda.probs)

qda.fit <- qda(class ~ ., data =train)
qda.fit
train.qda.probs <- predict(qda.fit)$posterior[, 2]
misClassError(trainY, train.qda.probs)
confusionMatrix(trainY, train.qda.probs)

test.qda.probs <- predict(qda.fit, newdata = test)$posterior[, 2]
misClassError(testY, test.qda.probs)
confusionMatrix(testY, test.qda.probs)
## MARS###########################################
library(earth)
set.seed(1)
earth.fit <- earth(class ~ ., data = train,
                   pmethod = "cv",
                   nfold = 5,
                   ncross = 3,
                   degree = 1,
                   minspan = -1,
                   glm=list(family=binomial)
                   )
summary(earth.fit)
plotmo(earth.fit)
plotd(earth.fit)
evimp(earth.fit)

test.earth.probs <- predict(earth.fit, newdata = test, type = "response")
misClassError(testY, test.earth.probs)
confusionMatrix(testY, test.earth.probs)
##################################################
library(ROCR)
bad.fit <- glm(class ~ thick, family = binomial, data = train)
test.bad.probs <- predict(bad.fit, newdata = test, type = "response") #save probabilities
pred.full <- prediction(test.probs, test$class)
perf.full <- performance(pred.full, "tpr", "fpr")
plot(perf.full, main = "ROC", col = 1)
pred.bic <- prediction(test.bic.probs, test$class)
perf.bic <- performance(pred.bic, "tpr", "fpr")
plot(perf.bic, col = 2, add = TRUE)
pred.bad <- prediction(test.bad.probs, test$class)
perf.bad <- performance(pred.bad, "tpr", "fpr")
plot(perf.bad, col = 3, add = TRUE)
pred.earth <- prediction(test.earth.probs, test$class)
perf.earth <- performance(pred.earth, "tpr", "fpr")
plot(perf.earth, col = 4, add = TRUE)
legend(0.6, 0.6, c("FULL", "BIC", "BAD", "EARTH"), 1:4)
performance(pred.full, "auc")@y.values
performance(pred.bic, "auc")@y.values
performance(pred.bad, "auc")@y.values
performance(pred.earth, "auc")@y.values

