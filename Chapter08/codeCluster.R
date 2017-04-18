library(cluster) #conduct cluster analysis
library(compareGroups) #build descriptive statistic tables
library(HDclassif) #contains the dataset
library(NbClust) #cluster validity measures
library(sparcl) #colored dendrogram
data(wine)
str(wine)
names(wine) <- c("Class", "Alcohol", "MalicAcid", "Ash", "Alk_ash",
                 "magnesium", "T_phenols", "Flavanoids", "Non_flav",
                 "Proantho", "C_Intensity", "Hue", "OD280_315", "Proline")
names(wine)
df <- as.data.frame(scale(wine[, -1]))
str(df)
table(wine$Class)

numComplete <- NbClust(df, distance = "euclidean", 
                       min.nc = 2, max.nc = 6, 
                       method = "complete", index = "all")
numComplete$Best.nc

dis <- dist(df, method = "euclidean")
hc <- hclust(dis, method = "complete")
plot(hc, hang = -1,labels = FALSE, main = "Complete-Linkage")
comp3 <- cutree(hc, 3)
ColorDendrogram(hc, y = comp3, main = "Complete", branchlength = 50)
table(comp3)
table(comp3, wine$Class)
(51+50+48)/178

numWard <- NbClust(df, diss = NULL, distance = "euclidean", 
        min.nc = 2, 
        max.nc = 6, 
        method= "ward.D2", 
        index = "all")

hcWard <- hclust(dis, method = "ward.D2")
plot(hcWard, hang = -1, labels = FALSE, main = "Ward's-Linkage")
ward3 <- cutree(hcWard, 3)
table(ward3, wine$Class)              
table(comp3, ward3)
aggregate(wine[, -1], list(comp3), mean)
aggregate(wine[, -1], list(ward3), mean)
par(mfrow = c(1, 2))

boxplot(wine$Proline ~ comp3, 
        main = "Proline by Complete Linkage")
boxplot(wine$Proline ~ ward3, 
        main = "Proline by Ward's Linkage")

numKMeans <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")

set.seed(1234)
km <- kmeans(df, 3, nstart = 25)
table(km$cluster)

km$centers

# wine$km_cluster <- km$cluster
boxplot(wine$Alcohol ~ km$cluster, 
        main = "Alcohol Content, K-Means")
boxplot(wine$Alcohol ~ ward3, 
        main = "Alcohol Content, Ward's")
table(km$cluster, wine$Class)

wine$Alcohol <- as.factor(ifelse(df$Alcohol > 0, "High", "Low"))

disMatrix <- daisy(wine[, -1], metric = "gower")  
set.seed(123)
pamFit <- pam(disMatrix, k = 3)
table(pamFit$clustering)
table(pamFit$clustering, wine$Class)

wine$cluster <- pamFit$clustering

group <- compareGroups(cluster ~ ., data = wine) 
clustab <- createTable(group) 
clustab
export2csv(clustab, file = "wine_clusters.csv")

library(randomForest)
set.seed(1)
rf <- randomForest(x = wine[, -1], ntree = 2000, proximity = T)
rf

dim(rf$proximity)
rf$proximity[1:5, 1:5]
importance(rf)

dissMat <- sqrt(1 - rf$proximity)
dissMat[1:2, 1:2]

set.seed(123)
pamRF <- pam(dissMat, k = 3)
table(pamRF$clustering)
table(pamRF$clustering, wine$Class)
