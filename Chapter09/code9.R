
library(ggplot2) #support scatterplot
# library(GPArotation) #support rotation
library(psych) #PCA package

train <- read.csv("NHLtrain.csv")
str(train)
names(train)

train.scale <- scale(train[, -1:-2])
nhl.cor = cor(train.scale)
cor.plot(nhl.cor)

pca <- principal(train.scale, rotate="none")
plot(pca$values, type="b", ylab="Eigenvalues", xlab="Component")
pca.rotate <- principal(train.scale, nfactors = 5, rotate = "varimax")
pca.rotate
pca.scores <- data.frame(pca.rotate$scores)
head(pca.scores)
pca.scores$ppg <- train$ppg

nhl.lm <- lm(ppg ~ ., data = pca.scores)
summary(nhl.lm)
nhl.lm2 <- lm(ppg ~ RC1 + RC2, data = pca.scores)
summary(nhl.lm2)

plot(nhl.lm2$fitted.values, train$ppg, 
     main="Predicted versus Actual",
     xlab="Predicted",ylab="Actual")

train$pred <- round(nhl.lm2$fitted.values, digits = 2)

p <- ggplot(train, aes(x = pred,
                           y = ppg,
                           label = Team)) 
p + geom_point() + 
  geom_text(size=3.5, hjust=0.1, vjust=-0.5, angle=0) + 
  xlim(0.8, 1.4) + ylim(0.8, 1.5) +
  stat_smooth(method="lm", se=FALSE)

pca.scores$Team <- train$Team
p2 <- ggplot(pca.scores, aes(x = RC1, y = RC2, label = Team))
p2 + geom_point() +
  geom_text(size = 2.75, hjust = .2, vjust = -0.75, angle = 0) +
  xlim(-2.5, 2.5) + ylim(-3.0, 2.5)

sqrt(mean(nhl.lm2$residuals^2))

test <- read.csv("NHLtest.csv")
test.scores <- data.frame(predict(pca.rotate, test[, c(-1:-2)]))
test.scores$pred <- predict(nhl.lm2, test.scores)

test.scores$ppg <- test$ppg
test.scores$Team <- test$Team

p <- ggplot(test.scores, aes(x = pred,
                       y = ppg,
                       label = Team)) 
p + geom_point() + 
  geom_text(size=3.5, hjust=0.4, vjust = -0.9, angle = 35) + 
  xlim(0.75, 1.5) + ylim(0.5, 1.6) +
  stat_smooth(method="lm", se=FALSE)

resid <- test.scores$ppg - test.scores$pred
sqrt(mean(resid^2))
