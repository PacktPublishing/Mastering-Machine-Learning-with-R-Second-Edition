library(MASS)
library(caret)
library(caretEnsemble)
library(caTools)

pima <- rbind(Pima.tr, Pima.te)
set.seed(502)
split <- createDataPartition(y = pima$type, p = 0.75, list = F)
train <- pima[split, ]
test <- pima[-split, ]
table(train$type)
#svm 79.6 with sigmoid kernel

control <- trainControl(method = "cv",
                        number = 5,
                        savePredictions = "final",
                        classProbs = T,
                        index=createResample(train$type, 5),
                        sampling = "up",
                        summaryFunction = twoClassSummary)

set.seed(2)
models <- caretList(
  type ~ ., data = train,
  trControl = control,
  metric = "ROC",
  methodList = c("rpart", "earth", "knn")
)

models

# xyplot(resamples(models))
modelCor(resamples(models))

#greedy_ensemble <- caretEnsemble(
#  models, 
#  metric = "ROC",
#  trControl=trainControl(
 #   number=2,
 #   summaryFunction=twoClassSummary,
 #   classProbs=TRUE
 # ))

#summary(greedy_ensemble)

model_preds <- lapply(models, predict, newdata=test, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"Yes"])
model_preds <- data.frame(model_preds)
# head(model_preds)
# ens_preds <- predict(greedy_ensemble, newdata=test, type="prob")
# model_preds$ensemble <- ens_preds
# colAUC(model_preds, test$type)

stack <- caretStack(models, method = "glm",
                    metric = "ROC",
                    trControl = trainControl(
                      method = "boot",
                      number = 5,
                      savePredictions = "final",
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary
                    ))

summary(stack)
# rawPreds <- predict(stack, newdata = test, type = "raw")
prob <- 1-predict(stack, newdata = test, type = "prob")

model_preds$ensemble <- prob
colAUC(model_preds, test$type)

# mlR

library(mlr)
library(ggplot2)
library(HDclassif)
library(DMwR)
library(reshape2)
library(corrplot)

data(wine)
table(wine$class)
wine$class <- as.factor(wine$class)
set.seed(11)
df <- SMOTE(class ~ ., wine, perc.over = 300, perc.under = 300)
table(df$class)

wine.scale <- data.frame(scale(wine[, 2:5]))
wine.scale$class <- wine$class
wine.melt = melt(wine.scale, id.var="class") 
ggplot(data = wine.melt, aes( x = class, y = value)) + 
  geom_boxplot() + 
  facet_wrap( ~ variable, ncol = 2)

#outliers
outHigh <- function(x) {
  x[x > quantile(x, 0.99)] <- quantile(x, 0.9)
  x
}

outLow <- function(x) {
  x[x < quantile(x, 0.01)] <- quantile(x, 0.1)
  x
}

wine.trunc <- data.frame(lapply(wine[, -1], outHigh))
wine.trunc <- data.frame(lapply(wine.trunc, outLow))
wine.trunc$class <- wine$class
boxplot(wine.trunc$V3 ~ wine.trunc$class)

c <- cor(wine.trunc[, -14])
corrplot.mixed(c, upper = "ellipse")

library(caret) #if not already loaded
set.seed(502)
split <- createDataPartition(y = df$class, p = 0.7, list = F)
train <- df[split, ]
test <- df[-split, ]
wine.task <- makeClassifTask(id = "wine", data = train,
                             target = "class")
# wine.task

# wine.task <- normalizeFeatures(wine.task, method = "standardize")
str(getTaskData(wine.task))

rdesc <- makeResampleDesc("Subsample", iters = 3)

param <- makeParamSet(
  makeDiscreteParam("ntree", values = c(750, 1000, 1250, 1500, 1750, 2000))
)

ctrl <- makeTuneControlGrid()

# list of models
# https://mlr-org.github.io/mlr-tutorial/release/html/integrated_learners/index.html
tuning <- tuneParams("classif.randomForest", task = wine.task,
                     resampling = rdesc, par.set = param, 
                     control = ctrl)
tuning$x
tuning$y

rf <- setHyperPars(makeLearner("classif.randomForest",
                               predict.type = "prob"), par.vals = tuning$x)
# getHyperPars(rf)
fitRF <- train(rf, wine.task)
fitRF$learner.model
predRF <- predict(fitRF, newdata = test)
# head(data.frame(predRF))
getConfMatrix(predRF)
performance(predRF, measures = list(mmce, acc))

# classif.penalized.ridge 
# one vs rest
ovr <- makeMulticlassWrapper("classif.penalized.ridge", mcw.method = "onevsrest")


#################################3
bag.ovr <- makeBaggingWrapper(ovr, bw.iters = 10, #default of 10 
                             bw.replace = TRUE, #default
                             bw.size = 0.7, 
                             bw.feats = 1)

#bag.ovr <- setPredictType(bag.ovr, predict.type = "prob")
#################################################################
set.seed(317)
fitOVR <- mlr::train(bag.ovr, wine.task)
predOVR <- predict(fitOVR, newdata = test)
head(data.frame(predOVR))
getConfMatrix(predOVR)
# performance(predOVR, measures = list(mmce, acc))

# wrapper
#models <- list(makeLearner("classif.rpart", predict.type = "prob"),
#               makeLearner("classif.glmnet", predict.type = "prob"))
pima.task <- makeClassifTask(id = "pima", data = train, target = "type")
pima.smote <- smote(pima.task, rate = 2, nn = 3)
str(getTaskData(pima.smote))

base <- c("classif.randomForest", "classif.qda", "classif.glmnet")
learns <- lapply(base, makeLearner)
learns <- lapply(learns, setPredictType, "prob")
sl <- makeStackedLearner(base.learners = learns, 
                         super.learner = "classif.logreg",
                         predict.type = "prob",
                         method = "stack.cv")
slFit <- mlr::train(sl, pima.smote)
predFit <- predict(slFit, newdata = test)
getConfMatrix(predFit)
performance(predFit, measures = list(mmce, acc, auc))


