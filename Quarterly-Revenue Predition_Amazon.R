#load libraries
library(tidyverse)

#set working directory (adjust this for your own computer)
setwd("C:/Users/Lucy Wu/Documents")


#read dataset into R
amazondf <- read.csv("amazon_web_services.csv")
View(amazondf)

#create a time series plot showing number of LinkedIn members by quarter, 
#in millions
ggplot(data = amazondf, mapping = aes(x = Quarter, y = Revenue)) +
  geom_line (group=1) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Amazon Web Services by Quarter (millions), 2014 to 2021", 
       x = "Quarter", y = "Revenue")

#Add a column of consecutive numbers corresponding with each quarter
amazondf$Time <- 1:nrow(amazondf) 

#Use simple linear regression analysis to create a regression equation for 
#forecasting
amazonreg<-lm(Revenue ~ Time, data = amazondf)
summary(amazonreg)

#Create a vector of predicted values generated from the 
#regression above
amazon_pred = predict(amazonreg)

#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae (amazondf$Revenue, amazon_pred)
mse (amazondf$Revenue, amazon_pred)
rmse (amazondf$Revenue, amazon_pred)
mape (amazondf$Revenue, amazon_pred)

#Create a new variable that squares the Time variable
amazondf$Time2 <- amazondf$Time^2

#Use a quadratic regression model to create a regression equation for 
#forecasting
amazonregquad<-lm(Revenue ~ Time + Time2, data = amazondf)
summary(amazonregquad)

#Create a vector of predicted values generated from the 
#regression above
amazon_pred2 = predict(amazonregquad)

#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae (amazondf$Revenue, amazon_pred2)
mse (amazondf$Revenue, amazon_pred2)
rmse (amazondf$Revenue, amazon_pred2)
mape (amazondf$Revenue, amazon_pred2)


#Predict LinkedIn membership for Quarter 3 and Quarter 4 of 2014

#Create an object with the time periods to use for the prediction
new <- data.frame(Time = c(33, 34, 35, 36), Time2 = c(1089, 1156, 1225, 1296))
predict(amazonregquad, newdata = new)






#Create functions for the accuracy measures (we've done this before)
mae<-function(actual,pred){
  mae <- mean(abs(actual-pred), na.rm=TRUE)
  return (mae)
}

mse<-function(actual,pred){
  mse <- mean((actual-pred)^2, na.rm=TRUE)
  return (mse)
}

rmse<-function(actual,pred){
  rmse <- sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return (rmse)
}  

mape<-function(actual,pred){
  mape <- mean(abs((actual - pred)/actual), na.rm=TRUE)*100
  return (mape)
}

