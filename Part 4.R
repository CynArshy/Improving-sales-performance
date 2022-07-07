# Import the necessary libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library('stringr')

# Import the insurance data set
games <- read.csv (file.choose (), header = T)

# Sense check the data set
head(games)
str(games)
summary(games)

ggplot (games, aes (x = Global_Sales)) +
  geom_histogram (stat = "count")+ 
  labs(title = "Turtle Games Global sales",
       subtitle = "In millions")

qplot(Global_Sales, bins = 10, data = games)+
  labs(title = "Turtle Games Count of Global sales",
       subtitle = "In millions")

#missing values for the whole data set 
sum(is.na(games))

games$Genre <- tolower(games$Genre)
head(games)

games2 <- subset (games, select = -c(Rank, Name, Platform, Year, Publisher))
head(games2)


#How will you evaluate the skewness of the data

summary(games2)

install.packages("moments")
library(moments)

ggplot (games2, aes (x = Genre)) +  
  geom_histogram (stat = "count")+
  labs(title = "Games sold by type")


ggplot (games2, 
        aes (x = Genre, 
             y = ..count.. / sum(..count..))) +
             geom_histogram(fill = "blue",
                 color = "black",
                 stat = "count") +
            labs (x = "Game Genre",
            y = "Percent",
            title = "Games sold by type") +
            scale_y_continuous (label = scales::percent) +
            coord_flip() 


ggplot (games2, aes(x = Genre)) +
  geom_density() + 
  labs (title = "Games sold by type")  
  ggplot (games2, aes (x = Global_Sales)) +
  geom_density (fill = "red",
                bw = 5) +  
  labs (title = "Global Sales")  

  
# Now we can check for skewness
skewness(games2$Global_Sales)
#Check for kurtosis.
kurtosis(games2$Global_Sales)

shapiro.test(games2$Global_Sales[0:5000])


skewness(games2$NA_Sales)
#Check for kurtosis.
kurtosis(games2$NA_Sales)

shapiro.test(games2$NA_Sales[0:5000])


skewness(games2$EU_Sales)
#Check for kurtosis.
kurtosis(games2$EU_Sales)

shapiro.test(games2$EU_Sales[0:5000])


skewness(games$Rank)
#Check for kurtosis.
kurtosis(games$Rank)

shapiro.test(games$Rank[0:5000])


#As the 3 variables above are correlated, I will study another variable, Sales online

games2$Online_Sales <- (games2$Global_Sales - games2$ EU_Sales - games2$NA_Sales)
head(games2)

skewness(games2$Online_Sales)
#Check for kurtosis.
kurtosis(games2$Online_Sales)

shapiro.test(games2$Online_Sales[0:5000])

# What is the correlation between the two variables that will help you predict global sales
# Specify the qqnorm function; draw a qqplot using the games_sale data:

qqnorm(games2$NA_Sales, col="blue", 
       xlab="NA_Sales Value", 
       ylab="In Millions S")

qqline(games2$NA_Sales,, col="red", lwd=2)

qqnorm(games2$EU_Sales, col="blue", 
       xlab="EU_Sales Value", 
       ylab="In Millions S")
qqline(games2$EU_Sales, col="red", lwd=2)

qqnorm(games2$Global_Sales, col="blue", 
       xlab="Global_Sales Value", 
       ylab="In Millions S")

qqline(games2$Global_Sales, col="red", lwd=2)

qqnorm(games$Rank, col="blue", 
       xlab="Top ranked product sales", 
       ylab="In Millions S")

qqline(games$Rank, col="red", lwd=2)

qqnorm(games2$Online_Sales, col="blue", 
       xlab="Global_Sales Value", 
       ylab="In Millions S")

qqline(games2$Online_Sales, col="red", lwd=2)


# [1] Specify the t.test function; set 
#[1a] the data source, 
#[1b] the confidence interval (95%), and 
#[1c] the theoretical mean:  

t.test (games2$Global_Sales, conf.level = 0.95, mu = 120) 

# Specify the cor function; set the [1a] first and [1b] second variables:

cor (games2$NA_Sales, games2$Global_Sales)


# Our correlation coefficient is -0.42 suggests a weak negative correlation.
# A negative correlation coefficient suggests that the two variables vary in 
# opposite directions: If one increases, the other decreases
# The value is also closer to 0 than it is to -1, suggesting a weak relationship
# between the variables

cor (games2$NA_Sales , games2$EU_Sales)
# Our correlation coefficient of 0.76 suggests a strong positive correlation.
# A positive correlation coefficient suggests that the two variables vary in 
# same direction. That means as the one increases, so does the other; and if 
# one decreases, the other does too. Again the coefficient is closer to 1 than 0, 
# meaning there is a strong positive correlation, Meaning that the sales in 
# NA influence sales in EU
# The value is also closer to 1 than it is to -1, suggesting a strong relationship
# between the variables

cor (games2$EU_Sales , games2$Global_Sales)

cor (games2$EU_Sales , games2$Online_Sales)     



