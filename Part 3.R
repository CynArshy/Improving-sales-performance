install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)

lego <- read.csv(file.choose(), header = TRUE)
summary(lego)

#Histogram with reviews by age group
qplot(ages, bins = 10, data = lego, label= num_reviews)+
      labs(title = "Most reviewers of lego by age group")

#Simple scatterplot
qplot(ages, num_reviews, colour= piece_count, data = lego)+
      labs(title = "Most reviewed lego by age group")
      
#Barchart shows ages better distribution
ggplot(lego,aes(x = ages,
                col=list_price)) +
  geom_bar(position = "dodge") +
  labs(title = "Most reviewers of lego by age group")


#Scatterplot showing the smooth line
ggplot(lego, aes(x=ages,
                 y=num_reviews))+
                 geom_point()+
                 geom_smooth()+
                 labs(title = "Most reviewed lego by age group")

#Scatter plot with labels in reviews number                
qplot(ages, num_reviews, color= ages,label= num_reviews,
      data = lego, geom = c("point", "text"))+
      labs(title = "Most reviewers of lego by age group")

###############################################################################

# Purchases by customers who are at least 25 years old (>25 years)

lego2 <-filter(lego,ages>=25)
view(lego2)

arrange (lego2, desc(list_price)) 

qplot(ages,list_price, data = lego2, 
      colour = I("red"), geom = "boxplot")+
      labs(title = "Sets purchased by age with price")

qplot(ages,list_price, data = lego2, 
      colour = I("red"), geom = "bar")+
  labs(title = "Sets purchased by age with price")

qplot(ages, data = lego2, geom = "bar")+
  labs(title = "Sets purchased by age with price")

#scatter plot with price, age and kind of Lego set purchased 
qplot(ages, list_price, colour = piece_count, 
      data = lego2, geom = c("point", "smooth"))+
      labs(title = "Sets purchased by age with price")

#Histogram with reviews by age group
qplot(ages,list_price,
      col=piece_count, 
      data = lego2, label= num_reviews)+
      labs(title = "Sets purchased by age with price")

#Bar chart shows a better ages distribution
  
ggplot(lego2,aes(x = ages, 
                 col= piece_count)) +
  geom_bar(position = "dodge") +
  labs(title = "Sets purchased by over 25's",
       subtitle = "Amount of pieces preferred")

#Scatter plot showing 3 variables

qplot(ages, list_price, colour = piece_count,
      data = lego2, geom = c("point", "jitter"))+
  labs(title = "Lego sets purchases by over 25's",
       subtitle = "Pieces numbers preferred in colour")


