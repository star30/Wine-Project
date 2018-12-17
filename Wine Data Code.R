Wine_Data <- read_excel("~/Documents/Data Practicum/Data Practicum II/Wine Data.xlsx")
View(Wine_Data)
summary(Wine_Data)
str(Wine_Data)
graph <- ggplot(Wine_Data, aes(x=Cultivar, fill=Cultivar))+
geom_bar()+theme_minimal()
graph
graph + labs(title="Wine Cultivars")
install.packages("ggcorrplot")
qplot(Wine_Data$Alcohol, geom = "histogram", main = "Alcohol Distribution", xlab = "Alcohol", ylab = "Count", fill=I("blue"), ylim = c(0,20))
qplot(Wine_Data$`Total phenols`, geom = "histogram", main = "Phenol Distribution", xlab = "Phenol", ylab = "Count", fill=I("pink"), ylim = c(0,20))
qplot(Wine_Data$`Color intensity `, geom = "histogram", main = "Color Intensity Distribution", xlab = "Color Intensity", ylab = "Count", fill=I("purple"), ylim = c(0,20))
install.packages("corrplot")
library(corrplot)
M <-cor(Wine_Data)
corrplot(M, method = "number")
#SVM Classification
Wine_Data$'Cultivar' <- as.factor(Wine_Data$'Cultivar')
i<-subset(Wine_Data, select = -Cultivar)
j<-factor(Wine_Data$Cultivar)
j
model<-svm(i,j, probability = T)
summary(model)
pred<-predict(model,i)
table(pred,j)
#Kmeans Clusters
wine_data_cluster<- scale(Wine_Data[, -1])
set.seed(123)
k.max <- 15
wss <- sapply(1:k.max, function(k){kmeans(wine_data_cluster, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
winedata.km <- kmeans(wine_data_cluster, 3, nstart = 25)
wine_data_cluster$cluster
winedata.km$cluster
install.packages("factoextra")
install.packages("NbClust")
#plots the clusters
fviz_cluster(winedata.km, data = wine_data_cluster, geom = "point", stand = FALSE, frame.type = "norm")
set.seed(123)
#Decision Tree
winedata_train <- Wine_Data[1:134, ]
winedata_test <- Wine_Data[135:178, ]
library(rpart)
wine_model <- rpart(Cultivar ~. , data = winedata_train)
wine_model
library(rpart.plot)
rpart.plot(wine_model)
library(rpart)
winemodel_predict <- predict(wine_model,winedata_test)
summary(winemodel_predict)
summary(winedata_test$Cultivar)





