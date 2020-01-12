require(glmnet)
install.packages("randomForest")
library(randomForest)
install.packages("e1071")
install.packages("caret")
library("InformationValue")


data <- read.csv("C:\\Users\\Vishal Garg\\Documents\\london_merged.csv", sep= ",", header = T)
str(data)

###################################################

#We assume from looking at the data, below (see the "EXPLORE RAW DATA" section) (and are told, for some variables,)...
# cnt = number of bikes rented
# t1 = temperature in celsius
# t2 = temperature feel in celsius
# wind_speed =  wind speed (km/hr)
# weather_code  Is a scale from 0-25 (We can't really assume anything else about it)  
# is_holiday  1=is holiday  0=is not holiday
# is_weekend  1=is weekend  0=is not weekend
# Season  0=Spring 1=Summer 2=Fall 3=Winter

###################################################

data$season <- as.factor(data$season)
data$season
str(data)

set.seed(1234)

input<- sample(2, nrow(data),replace = T, prob = c(0.8,0.2))

tdata<- data[input == 1,]
test <- data[input==2,]

boxplot(data$cnt, main= "cnt", col = "skyblue", pch= 16)
boxplot(data$t1,main= "t1", col = "skyblue", pch= 16)
boxplot(data$t2, main="t2", col="skyblue", pch= 16)
boxplot(data$hum, main="hum", col="skyblue", pch= 16)
boxplot(data$wind_speed, main="windspeed", col="skyblue", pch= 16)
boxplot(data$weather_code, main="weather_code", col="skyblue", pch= 16)
table(data$is_holiday)
table(data$is_weekend)
table(data$season)

modal <- glm(is_weekend ~ t1+t2+hum+wind_speed+ weather_code+ cnt+ season, data = tdata, family = "binomial")
summary(modal)

modal <- glm(is_weekend ~ t1+t2+ weather_code+ cnt, data = tdata, family = "binomial")
summary(modal)

f1 <- predict(modal, test, type = 'response')

mat1 <- ifelse(f1>0.5, 1, 0)

tab1 <- table(predicted = mat1, actual = test$is_weekend)

tab1

o1 <- optimalCutoff(test$is_weekend, f1)
misClassError(test$is_weekend, f1, threshold = o1)



################### Random Forest ##############################

set.seed(333)
str(tdata)

tdata$is_weekend <- as.factor(tdata$is_weekend)

rf <- randomForest(is_weekend ~ cnt+t1+t2+hum+wind_speed+weather_code+is_holiday+season, data = tdata )

print(rf)

attributes(rf)
rf$confusion



str(tdata)


library(caret)

p1 <- predict(rf, test )

head(p1)


library(e1071)

plot(rf)

hist(treesize(rf), main = "NO. of nodes for the trees", col = "blue")

varImpPlot(rf, pch = 16, main = "Important variable role", col = "blue", sort = T)

importance(rf)
varUsed(rf)

partialPlot(rf, tdata, cnt, "1")

getTree(rf,1, labelVar = T)

############################# THE  END  #####################

