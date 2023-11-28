library(dplyr)
library(imputeTS)
library(tidyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(rpart)
library(rpart.plot)
library(randomForest)

setwd("C:/Users/AlexiaLeong.ALEXIARPI/OneDrive/Documents/Data Analytics/DataAnalyticsProject2023")

literacy <- read.csv("YouthLiteracyRate.csv", check.names=FALSE)
wdi <- read.csv("WorldDevelopmentIndicators.csv", check.names=FALSE)
wdi2 <- read.csv("WorldDevelopmentIndicators2.csv", check.names=FALSE)

#colnames(literacy)
#colnames(wdi)

#Merge Datasets
allWDI <- merge(wdi, wdi2, by=c("Time", "Country Name", "Time Code", "Country Code"))
combinedData <- merge(literacy, allWDI, by.x=c("Time Period", "Reference Area"), by.y=c("Time", "Country Name"))

#Change all '..' to NA values to easily use is.na() later
combinedData[combinedData == '..'] <- NA

#Change datatypes of character columns to numerical
combinedData <- combinedData %>%
  mutate(across(-c(`Reference Area`, `Sex`, `Age group`, `Units of measurement`
                   ,`Time Code`, `Country Code`), as.numeric))

sapply(combinedData, class)
View(combinedData)

#---------------------------Exploratory Data Analysis----------------------------
attach(combinedData)
boxplot(`Observation Value`)

summary(`Observation Value`)
hist(`Observation Value`)

data1980 <- combinedData[combinedData$`Time Period` == 1980,]
hist(data1980$`Observation Value`,  ylim=c(0,100), main = "Histogram of 1980s Literacy Rates")

data1990 <- combinedData[combinedData$`Time Period` == 1990,]
hist(data1990$`Observation Value`, ylim=c(0,100),  main = "Histogram of 1990s Literacy Rates")

data2000 <- combinedData[combinedData$`Time Period` == 2000,]
hist(data2000$`Observation Value`, ylim=c(0,100),  main = "Histogram of 2000s Literacy Rates")

data2010 <- combinedData[combinedData$`Time Period` == 2010,]
hist(data2010$`Observation Value`, ylim=c(0,100),  main = "Histogram of 2010s Literacy Rates")

View(data2010)

data1990Male <- data1990[data1990$Sex == "Male", ]
data1990Female <- data1990[data1990$Sex == "Female", ]

plot(data1990Male$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, data1990Male$`Observation Value`, main="1990s Male",
     xlab="GDP Per Capita ", ylab="Literacy Rate ", pch=19)

plot(data1990Female$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, data1990Female$`Observation Value`, main="1990s Female",
     xlab="GDP Per Capita ", ylab="Literacy Rate ", pch=19)

data2010AllGenders <- data2010[data2010$Sex == "All genders", ]
data2010Male <- data2010[data2010$Sex == "Male", ]
data2010Female <- data2010[data2010$Sex == "Female", ]

plot(data2010AllGenders$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, data2010AllGenders$`Observation Value`, main="2010s",
     xlab="GDP Per Capita ", ylab="Literacy Rate ", pch=19)

plot(data2010Male$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, data2010Male$`Observation Value`, main="2010s Male",
     xlab="GDP Per Capita ", ylab="Literacy Rate ", pch=19)

plot(data2010Female$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, data2010Female$`Observation Value`, main="2010s Female",
     xlab="GDP Per Capita ", ylab="Literacy Rate ", pch=19)

plot(combinedData$`Observation Value` ~ combinedData$`GDP (current US$) [NY.GDP.MKTP.CD]`, data = combinedData,
     xlab = "GDP (US$)", ylab = "Literacy Rate",
     main = "GDP Scatter Plot")

plot(combinedData$`Observation Value` ~ combinedData$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, data = combinedData,
     xlab = "GDP per Capita (US$)", ylab = "Literacy Rate",
     main = "GDP per Capita Scatter Plot")

plot(combinedData$`Observation Value` ~ combinedData$`School enrollment, secondary (% gross) [SE.SEC.ENRR]`, data = combinedData,
     xlab = "School Enrollment Rate", ylab = "Literacy Rate",
     main = "School Enrollment Scatter Plot")

plot(combinedData$`Observation Value` ~ combinedData$`Government expenditure on education, total (% of government expenditure) [SE.XPD.TOTL.GB.ZS]`, data = combinedData,
     xlab = "Government Expenditure %", ylab = "Literacy Rate",
     main = "Government Expenditure Scatter Plot")

#------Year aggregation: Keep years from 2000-2015 with countries with at least 5 entries---------
data2000Beyond <- combinedData[combinedData$`Time Period` >= 2000,]
View(data2000Beyond)

data2000BeyondAG <- data2000Beyond[data2000Beyond$Sex == "All genders",]
data2000BeyondMale <- data2000Beyond[data2000Beyond$Sex == "Male",]
data2000BeyondFemale <- data2000Beyond[data2000Beyond$Sex == "Female",]
View(data2000BeyondAG)

countries <- data2000BeyondAG %>% 
  group_by(data2000BeyondAG$`Reference Area`)%>%
  summarise(num_datapoints = n()) %>%
  filter(num_datapoints >= 5)

colnames(countries)[1] <- "Countries"

print(countries)

data2000BeyondAG <- data2000BeyondAG[data2000BeyondAG$`Reference Area`%in% countries$Countries,]
View(data2000BeyondAG)

#--------------------Filling in NaN values-------------------------------
#Fill in NAs with mean of that group for that column 
data2000BeyondAG <- data2000BeyondAG %>%
  group_by(data2000BeyondAG$`Reference Area`) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

#there are still NaN values for columns where that country never measured for that metric
# -- get rid of those rows completely when doing analysis later?
  
View(data2000BeyondAG)

#-------------------------KMeans Clustering--------------------------------------

literacyRatesandGDP <- data2000BeyondAG %>% select(`Observation Value`
                                                   , `GDP (current US$) [NY.GDP.MKTP.CD]`)

#Finding the number of clusters:
sumofsquares <- numeric(10)  # Vector to store within-cluster sum of squares

for (i in 1:10) {
  kmeans_model <- kmeans(literacyRatesandGDP, centers = i)
  sumofsquares[i] <- kmeans_model$tot.withinss
}

# Plot the within-cluster sum of squares
plot(1:10, sumofsquares, type = "b", xlab = "Number of Clusters (K)", ylab = "Within-cluster Sum of Squares")

#Elbow point at k = 2 clusters
k <- 2

# KMeans clustering
kmeans_model <- kmeans(literacyRatesandGDP, centers = k)

# Get cluster assignments
cluster_assignments <- kmeans_model$cluster

# Add cluster assignments to the original data
clusterData <- cbind(data2000BeyondAG, Cluster = cluster_assignments)
View(clusterData)

#Scatterplot with Cluster centers:
cluster_centers <- as.data.frame(kmeans_model$centers)

ggplot(clusterData, aes(x = `GDP (current US$) [NY.GDP.MKTP.CD]`, 
                        y = `Observation Value`, color = factor(Cluster))) +
  geom_point() +
  geom_point(data = cluster_centers, aes(x = `GDP (current US$) [NY.GDP.MKTP.CD]`, 
                                         y = `Observation Value`), color = "black", size = 5, shape = 1) +
  labs(title = "KMeans Clustering with Cluster Centers",
       x = "GDP (US$)",
       y = "Literacy Rate",
       color = "Cluster") +
  theme_minimal()

#Cluster stats:

#Silhouette score:
silhouette_scores <- silhouette(kmeans_model$cluster, dist(data2000BeyondAG))

#Plot silhouette scores:
fviz_silhouette(silhouette_scores)

#Cluster sizes:
table(kmeans_model$cluster)
#-------------------------Linear Regression on Each Cluster-------------------------
# Select data for upcoming models
clusterDataRegression <- clusterData %>% select(`Reference Area`, `Sex`, `Age group`,
                                      `Observation Value`
                          , `GDP (current US$) [NY.GDP.MKTP.CD]`
                          , `GDP per capita (current US$) [NY.GDP.PCAP.CD]`
                          , `School enrollment, secondary (% gross) [SE.SEC.ENRR]`
                          , `Government expenditure on education, total (% of government expenditure) [SE.XPD.TOTL.GB.ZS]`
                          , `Cluster`)

colnames(clusterDataRegression) <- c("ReferenceArea", "Sex", "AgeGroup", "ObservationValue",
                                     "GDP", "GDPperCapita", "SchoolEnrollment",
                                     "GovernmentExpenditure", "Cluster")

View(clusterDataRegression)
clusterDataRegression <- na.omit(clusterDataRegression)
View(clusterDataRegression)

# Split by cluster:
cluster1 <- clusterDataRegression[clusterDataRegression$Cluster == 1,]
View(cluster1)

cluster2 <- clusterDataRegression[clusterDataRegression$Cluster == 2,]
View(cluster2)

# Linear Regression:
cluster1_regression <- lm(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, data = cluster1)
summary(cluster1_regression)

#Plotting each predictor variable:
plot(ObservationValue ~ GDP, data = cluster1,
     xlab = "GDP (US$)", ylab = "Literacy Rate",
     main = "C1: GDP Scatter Plot with Regression Line")

#Adding regression line:
abline(lm(ObservationValue ~ GDP, data = cluster1), col = "red")

plot(ObservationValue ~ GDPperCapita, data = cluster1,
     xlab = "GDP per Capita (US$)", ylab = "Literacy Rate",
     main = "C1: GDP per Capita Scatter Plot with Regression Line")

abline(lm(ObservationValue ~ GDPperCapita, data = cluster1), col = "red")

plot(ObservationValue ~ SchoolEnrollment, data = cluster1,
     xlab = "School Enrollment", ylab = "Literacy Rate",
     main = "C1: School Enrollment Scatter Plot with Regression Line")

abline(lm(ObservationValue ~ SchoolEnrollment, data = cluster1), col = "red")

plot(ObservationValue ~ GovernmentExpenditure, data = cluster1,
     xlab = "Government Expenditure (US$)", ylab = "Literacy Rate",
     main = "C1: Government Expenditure Scatter Plot with Regression Line")

abline(lm(ObservationValue ~ GovernmentExpenditure, data = cluster1), col = "red")

#Training and predicting
set.seed(100)
trainingRowIndex <- sample(1:nrow(cluster1), 0.8*nrow(cluster1))
trainingData <- cluster1[trainingRowIndex, ]
testData  <- cluster1[-trainingRowIndex,]

predlm1 <- lm(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, data = trainingData)
predictions <- predict(predlm1, testData)  # predict
summary(predlm1)

#Cluster 2:
cluster2_regression <- lm(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, data = cluster2)
summary(cluster2_regression)

#Plotting each predictor variable:
plot(ObservationValue ~ GDP, data = cluster2,
     xlab = "GDP (US$)", ylab = "Literacy Rate",
     main = "C2: GDP Scatter Plot with Regression Line")

#Adding regression line:
abline(lm(ObservationValue ~ GDP, data = cluster2), col = "red")

plot(ObservationValue ~ GDPperCapita, data = cluster2,
     xlab = "GDP per Capita (US$)", ylab = "Literacy Rate",
     main = "C2: GDP per Capita Scatter Plot with Regression Line")

abline(lm(ObservationValue ~ GDPperCapita, data = cluster2), col = "red")

plot(ObservationValue ~ SchoolEnrollment, data = cluster2,
     xlab = "School Enrollment", ylab = "Literacy Rate",
     main = "C2: School Enrollment Scatter Plot with Regression Line")

abline(lm(ObservationValue ~ SchoolEnrollment, data = cluster2), col = "red")

plot(ObservationValue ~ GovernmentExpenditure, data = cluster2,
     xlab = "Government Expenditure (US$)", ylab = "Literacy Rate",
     main = "C2: Government Expenditure Scatter Plot with Regression Line")

abline(lm(ObservationValue ~ GovernmentExpenditure, data = cluster2), col = "red")

#Training and predicting
set.seed(100)
trainingRowIndex <- sample(1:nrow(cluster2), 0.8*nrow(cluster2))
trainingData <- cluster2[trainingRowIndex, ]
testData  <- cluster2[-trainingRowIndex,]

predlm2 <- lm(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, data = trainingData)
predictions <- predict(predlm2, testData)  # predict
summary(predlm2)

#------------------------------Decision Tree--------------------------------
#Cluster 1

dtree1 <- rpart(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, 
                    data = cluster1)

summary(dtree1)
rpart.plot(dtree1, main = "Cluster 1 Decision Tree")

set.seed(100)

splitIndex <- sample(1:nrow(cluster1), 0.8 * nrow(cluster1))

trainData <- cluster1[splitIndex, ]
dtree1 <- rpart(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, 
data = trainData)

summary(dtree1)
rpart.plot(dtree1, main = "Cluster 1 Decision Tree")

testData <- cluster1[-splitIndex, ]
predictions <- predict(dtree1, cluster1)
print(predictions)

table <- table(cluster1$ObservationValue, predictions)
table

#Cluster 2

dtree2 <- rpart(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, 
                data = cluster2)

summary(dtree2)
rpart.plot(dtree1, main = "Cluster 2 Decision Tree")

#------------------------------Random Forest--------------------------------
#Cluster 1
set.seed(100)
rf1 <- randomForest(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure
                    , data=cluster1, ntree=500,importance=TRUE)
rf1
varImpPlot(rf1)
varImportance1 <- importance(rf1)
print(varImportance1)

splitIndex <- sample(1:nrow(cluster1), 0.8 * nrow(cluster1))
trainData <- cluster1[splitIndex, ]
testData  <- cluster1[-splitIndex,]
rtree1 <- randomForest(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, 
                data = trainData)

predictions <- predict(rtree1, newdata = testData)

plotData <- data.frame(Actual = testData$ObservationValue, Predicted = predictions)

plot(plotData$Actual, plotData$Predicted, 
     xlab = "Actual Values", ylab = "Predicted Values",
     main = "C1: Predicted vs Actual Values")

abline(0, 1, col = "red")

#Cluster 2
set.seed(100)
rf2 <- randomForest(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure
                    , data=cluster2, ntree=500,importance=TRUE)
rf2
varImpPlot(rf2)
varImportance2 <- importance(rf2)
print(varImportance2)

splitIndex <- sample(1:nrow(cluster2), 0.8 * nrow(cluster2))
trainData <- cluster2[splitIndex, ]
testData  <- cluster2[-splitIndex,]
rtree2 <- randomForest(ObservationValue ~ GDP + GDPperCapita + SchoolEnrollment + GovernmentExpenditure, 
                       data = trainData)

predictions <- predict(rtree2, newdata = testData)

plotData <- data.frame(Actual = testData$ObservationValue, Predicted = predictions)

plot(plotData$Actual, plotData$Predicted, 
     xlab = "Actual Values", ylab = "Predicted Values",
     main = "C2: Predicted vs Actual Values")

abline(0, 1, col = "red")
