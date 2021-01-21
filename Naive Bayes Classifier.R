install.packages("naivebayes")
install.packages("dplyr")
install.packages("ggplot2")
library(naivebayes)
library(dplyr)
library(ggplot2)
getwd()
data <- read.csv("C:/Users/shane/OneDrive/Documents/R Projects/binary.csv", header = T)
str(data)
attach(data)
xtabs(~admit+rank, data = data)
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)
str(data)

set.seed(1234)
index <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2) )
train <- data[index == 1,]
test <- data[index ==2,]

model <- naive_bayes(admit ~ ., data = train)
model
plot(model)

pred <- predict(model, train, type = 'prob')
head(cbind(pred, train))

p1 <- predict(model, train)
tab1 <- (table(PredictedValue=p1, ActualValue=train$admit))
tab1

p2 <- predict(model, test)
tab2 <- (table(PredictedValue=p2, ActualValue = test$admit))
tab2
c1 <- confusionMatrix(p1, train$admit)
c1

pairs.panels(data)
data %>%
ggplot(aes(x=admit, y=gre, fill = admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")

data %>%
  ggplot(aes(x=gre, fill = admit)) +
  geom_density(alpha = 0.8, color = 'black') +
  ggtitle("Density Plot")
