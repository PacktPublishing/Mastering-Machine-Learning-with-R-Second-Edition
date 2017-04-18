install.packages("abc")

#....
"Let's Go Sioux!"
15
((22+5)/9)*2
c(0,1,1,2,3,5,8,13,21,34) #fibonnaci sequence 
x <- c(0,1,1,2,3,5,8,13,21,34) 
x
x[1:3]
x[-5:-6]
plot(x)
plot(x, main = "Fibonacci Sequence", xlab = "Order", ylab = "Value")
y <- sqrt(x)
y
?sqrt
plot(x, y)
z <- 3
x2 <- x * z
x2
#Boolean
5 < 6 
6 < 5
x == 0

rep("North Dakota Hockey, 2016 NCAA Division 1 Champions", times=3)
seq(0, 10, by = 2)
p <- seq(1:3)
p
q = seq(1, 2, by = 0.5)
q
r <- rbind(p,q)
r
str(r)
s <- cbind(p, q)
s
s <- data.frame(s)
str(s)
names(s) <- c("column 1", "column 2")
s
t <- as.matrix(s)
t
t[1, 1]
t[, 2]
t[1:2, ]
new <- old[1:70, c(1, 3, 7:10)]
new = old[, -1]

#summary stats
a <- c(1, 2, 3, NA)
sum(a)
sum(a, na.rm = TRUE)
data <- c(4, 3, 2, 5.5, 7.8, 9, 14, 20)
mean(data)
median(data)
sd(data)
max(data)
min(data)
range(data)
quantile(data)
barplot(data)
abline(h = mean(data))
abline(h = median(data), lty = 2)

set.seed(1)
norm = rnorm(100) 
plot(norm)
hist(norm)

# tidyverse
library(dplyr)
summarize(group_by(iris, Species), average = mean(Sepal.Length))

iris %>% group_by(Species) %>% summarize(average = mean(Sepal.Length))

distinct(iris, Species)

count(iris, Species)

df <- filter(iris, Sepal.Width > 3.5)
df <- arrange(iris, desc(Petal.Length))
head(df)

iris2 <- select(iris, starts_with("Se"))
iris3 <- select(iris, -starts_with("Se"))

theIris <- bind_cols(iris2, iris3)
head(theIris)
head(iris)

summarize(iris, n_distinct(Sepal.Width))

dedupe <- iris %>% distinct(Sepal.Width, .keep_all = T)
str(dedupe)










          