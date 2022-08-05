# Regression-Analysis-in-Rprogramming-
#REGRESSION ANALYSIS ###
### loading the library
pacman::p_load(pacman, rio, tidyverse)

### Loading the dataset 

data <- import("StateData.xlsx") %>% as_tibble() %>%
        select(instagram:modernDance) %>% print()
view(data)
attach(data)
### Scatteplot for all the data 
data %>% select(instagram:modernDance) %>% plot() 

### Graphical check on some variables 
data %>% select(museum, volunteering) %>% plot()

### Adding a regresssion line to the plot 
lm(data$volunteering~data$museum) %>% abline()
title("Scatter plot of Volunteering againt Museum")

#Checking the correlation between the variables 
cor(museum, volunteering)
cor.test(museum, volunteering)
### This is enough to show that there is correlation between the two variables

## Bivariate Regression 
### Computing and saving the model
model <- lm(data$volunteering~data$museum)
### showing the model 
model #this show the intercept and the slope 

###Summarizing the regresssion model
summary(model)
### This tell you that the intercept is not significantly different from zero which is fine. 
### But it also tell you that the slope is significantly different from zero
### The adjusted R squared is 0.3497 and this means that the about 35% of the varaint in
### volunterring can be predicted by the scores in museum. 

### Finding the confidence intervals for the coefficience 
confint(model)

### Finding the predicted values of all the cases  of volunteering 
predict(model)

###Finding the prediction interval for values of volunteering for new 
### data for museum 

### Creating a new dataframe with 4 new values for museum 
newdata <- data.frame(museum = c(5.124, 3.456, 4.565, 28.34))

### using the fitted model to predict the values of volunteering based on the
### three new values 

predict(model, newdata = newdata)
### I will explains only one line, you can do the rest
### For a new student at the musuem with grade 5.124, we predict that the 
### student will volunteer with -0.69808195

### Creating prediction interval around the predicted values 
predict(model, newdata = newdata, interval = "predict")

### I will explains only one line, you can do the rest 
### The 95% prediction interval of the volunteering students at the musuem with
### grade 5.124 is between -2.3793 and 0.9831 

### creating a 99% prediction intervals around the predicted values
predict(model, newdata = newdata, interval = "predict", level = 0.99)

##  Regression diagnostics 
##
lm.influence(model) # Measure of influence 

influence.measures(model) # influence of individual observation 








