library(tidyverse)
library(ggplot2)


games_sales <- read.csv(file.choose (), header = T)

summary(games_sales)
head(games_sales)


# Prior exploring of the variables relation
# For North America
ggplot(games_sales, aes(x=NA_Sales,
                        y=Global_Sales,
                        col=Genre))+
                  geom_point()+
                  geom_smooth(method="lm",se=FALSE)+
                  labs(title = "Sales projection in North America",
                  subtitle = "By game type")

#For Europe
ggplot(games_sales, aes(x=EU_Sales,
                        y=Global_Sales,
                        col=Genre))+
                        geom_point()+
                        geom_smooth(method="lm",se=FALSE)+
                        labs(title = "Sales projection",
                        subtitle = "By game type")

####################################################

#Multiple linear regression model:

model <-lm(Global_Sales ~ NA_Sales * EU_Sales, data=games_sales)
summary(model)


########################################################################################

# Create a new object and specify the lm function and the variables:
# Fit the simple linear regression model for North America Sales assuming 80 Global sales

model1 <- lm(NA_Sales ~ Global_Sales, data = games_sales)
predict(model1)
predict(model1, data.frame(Global_Sales=5.51))


# Fit the simple linear regression model for Europe Sales, assuming 500 Global sales
model2 <- lm(EU_Sales ~ Global_Sales, data = games_sales)
predict(model2)
predict(model2, data.frame(Global_Sales=5.51))


#Predict Global Sales for from the estimated assumed variables
predict(model)
predict(model, data.frame(NA_Sales=2.72, EU_Sales=1.60))


##############################################################################################

# Another way of predicting Global Sales for upcoming financial year
# Dropping unnecessary columns give me a final dataset

games_final <-dplyr::select(games_sales, -c('Rank','Name','Platform','Publisher','Year','Genre'))

head(games_final)

cor(games_final)

# Plot the relationship between the variables

qplot(Global_Sales, NA_Sales, data=games_final, geom="point")
qplot(Global_Sales, NA_Sales, data=games_final, geom="point")+ geom_smooth()


# Create a new object and specify the lm function and the variables:
# Fit the simple linear regression model for North America Sales

model1 <- lm(Global_Sales ~ NA_Sales, data = games_final)
model1
summary(model1)

games_forecast1 <- data.frame(NA_Sales = 100)
games_forecast1

## Predict from 
predictTest = predict(model1, newdata = games_forecast1, interval = 'confidence')
predictTest           


#Fit the simple linear regression model2 for European Sales
model2 <- lm(Global_Sales ~ EU_Sales , data = games_final)
model2
summary(model2)

games_forecast2 <- data.frame(EU_Sales = 80)
games_forecast2

## Predict from 
predictTest = predict(model2, newdata = games_forecast2, interval = 'confidence')
predictTest


model3 <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = games_final)
model3
summary(model3)

## Create a new data frame for the forecast values
games_forecast3 <- data.frame(NA_Sales= 179.2459, EU_Sales = 222.381)
games_forecast3

## Predict from 2021 to 2023
predictTest = predict(model3, newdata = games_forecast3, interval = 'confidence')
predictTest

SSE = sum(model3$residuals^2)
SSE


####################################################

## Another approach with linear regression
# join the two values for North America and for Europe

games_sales$NA_EU <-paste(games_sales$NA_Sales + games_sales$EU_Sales)
head(games_sales)

##Fit the linear regression model
#Fit the simple linear regression model4 for NA and Europe Sales

model4 <- lm(NA_EU ~ Global_Sales, data = games_sales)
model4
summary(model4)

## Create a new data frame for the forecast values

games_forecast4 <- data.frame(Global_Sales= 2021:2023)
games_forecast4

## Predict from 2021 to 2023
predict(model4, newdata = games_forecast4)
predictTest = predict(model4, newdata = games_forecast4, interval = 'confidence')
predictTest

predict(model4, newdata = games_forecast4)


###########################################################

# Determine global sales (in millions) for each of the video games

# Multiple linear regression visual aid
# First graph with most sold and less sold Genres

ggplot(games_sales, aes(x=EU_Sales,
                        y=Global_Sales,
                        col=Genre))+
  geom_point()+
  geom_abline(aes(intercept=0.029183,
                  slope=1.159430,
                  col="Action"))+
  geom_abline(aes(intercept=1.159430-1.381044,
                  slope=1.159430-0.003201,
                  col="Strategy"))+
  labs(title = "Sales projection in Eurpe",
       subtitle = "By game type")

#Visual Aid

ggplot(games_sales, aes(x=EU_Sales,
                        y=Global_Sales,
                        col=Genre))+
                     geom_point()+
                     geom_abline(aes(intercept=0.029183,
                     slope=1.159430))+
                     geom_abline(aes(intercept=1.159430-1.381044,
                     slope=1.159430-0.003201))

library(dplyr)

games_new<-games_sales %>%
  group_by(Genre) %>%
  summarise(Global Sales = sum(Global_Sales))

games_new



