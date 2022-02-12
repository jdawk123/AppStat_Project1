
#load libraries that will be in use for this project
library(tidyverse)
library(readr)
library(fastDummies)
library(car)
library(olsrr)
library(MASS)
library(qpcR)
library(dplyr)
library(ggplot2)
library(GGally)
library(leaps)




#insert the data here
cars = read.csv(file.choose(),header = TRUE)

#check out summary statistics and look for missing values
head(cars,5)
tail(cars,5)
summary(cars)

#checks for how many missing values
colSums(is.na(cars))

#Will change the name of the columns to make it easier to use
colnames(cars) = c('Make','Model','Year','Engine_Fuel_Type','Engine_HP','Engine_Cylinders','Transmission_Type','Driven_Wheels','Number_of_Doors','Market_Category','Vehicle_Size','Vehicle_style','Highway_MPG','City_MPG','Popularity','MSRP')

#check to make sure the column names are changed
colnames(cars)

##Filling in Informatiaon based on cars
#Suzuki Verona and all 2005 Suzuki are 'regular unleaded' 
cars <- cars %>% mutate(Engine_Fuel_Type = ifelse(is.na(Engine_Fuel_Type), 'regular unleaded',Engine_Fuel_Type))

#Engine HP Missing Value - ~.57% of data
cars %>% filter(is.na(Engine_HP))

#We have 69 missing vlaues spread across ~13 different models
#some of these models have data from other years, ,while some will require more research

#Chevy Impala - Flex-Fuel
# All other flex-fuel models have 305 HP
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Impala', 305, Engine_HP))

# Ford Escapes
#cars %>% filter(Model == 'Escape') %>% dplyr::select(Year, Engine_Fuel_Type, Engine_HP)
# All other regular unleaded models have 168 horsepower
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Escape', 168, Engine_HP))

#Freestars
#cars %>% filter(Model == 'Freestar')
# 2006 models are split 193/201 and 2007 models are all 2001. According to autoblog.com, the 2005 Freestar has 193 hp.
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Freestar', 193, Engine_HP))

# i-MiEV
#cars %>% filter(Model == 'i-MiEV')
# Both other i-MiEVs have 66 hp. This matches with research from edmunds.com
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'i-MiEV', 66, Engine_HP))

# M-Class
#cars %>% filter(Model == 'M-Class')
# All other diesel models have 240 horsepower.
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'M-Class', 240, Engine_HP))

# MKZ
#cars %>% filter(Model == 'MKZ')
# All other MKZ with Luxury,Hybrid Market Categories have 188 HP.
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'MKZ', 188, Engine_HP))

#RAV4 EV
#cars %>% filter(Model == 'RAV4 EV')
# All other RAV4 EV models have 154 horsepower. This matches with research from edmunds.com
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'RAV4 EV', 154, Engine_HP))

#Fiat 500e - 111 horsepower (kbb.com)
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == '500e', 111, Engine_HP))

#Focus - 143 horsepower (MotorTrend.com)
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Focus', 143, Engine_HP))

# Fit EV - 123 horsepower (car & driver)
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Fit EV', 123, Engine_HP))

# Soul EV - 109 horsepower (car & driver)
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Soul EV', 109, Engine_HP))

# Continental - 335 horsepower (carconnection.com)
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Continental', 335, Engine_HP))

#Leaf - 107 horsepower (USNews)
cars <- cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Leaf', 107, Engine_HP))

#Tesla Model S - Horsepower of 315 (Google)
cars = cars %>% mutate(Engine_HP = ifelse(is.na(Engine_HP) & Model == 'Model S', 315, Engine_HP))



# Engine Cylinders Missing Values
#cars %>% filter(is.na(Engine_Cylinders))
# All electric cars have 0 cylinders
cars <- cars %>% mutate(Engine_Cylinders = ifelse(Engine_Cylinders == 'electric', 0, Engine_Cylinders))

# Google says that all the remaining cars (RX-7 '93-'95, RX-8 '09-'11) have 2 cylinders
cars <- cars %>% mutate(Engine_Cylinders = ifelse(is.na(Engine_Cylinders), 2, Engine_Cylinders))

# Number of Doors Missing Values
#cars %>% cars(is.na(Number_of_Doors))
# Only two models here - '16 Tesla Model S (4 doors) & '13 Ferrari FF (2 doors)
# This was checked with other FF's and S's within the dataset.
cars <- cars %>% mutate(Number_of_Doors = ifelse(is.na(Number_of_Doors),ifelse(Model == 'FF', 2 ,4), Number_of_Doors))

#UNKNOWN Transmission Types
#cars %>% filter(Transmission_Type == 'UNKNOWN')
# 5 models...
#cars %>% filter(Make == 'Oldsmobile' & Model == 'Achieva') # MANUAL ---- look at Engine Cylinders
cars <- cars %>% mutate(Transmission_Type = ifelse(Transmission_Type =='UNKNOWN' & Model == 'Achieva', 'MANUAL', Transmission_Type))

#cars %>% filter(Make == 'Pontiac' & Model == 'Firebird') # MANUAL ---- look at Engine Fuel Type
cars <- cars %>% mutate(Transmission_Type = ifelse(Transmission_Type =='UNKNOWN' & Model == 'Firebird', 'MANUAL', Transmission_Type))

#cars %>% filter(Make == 'GMC' & Model == 'Jimmy') # MANUAL ---- all manuals are compact, autos are midsize
cars <- cars %>% mutate(Transmission_Type = ifelse(Transmission_Type =='UNKNOWN' & Model == 'Jimmy', 'MANUAL', Transmission_Type))

#cars %>% filter(Make == 'Chrysler' & Model == 'Le Baron') # AUTOMATIC
cars <- cars %>% mutate(Transmission_Type = ifelse(Transmission_Type=='UNKNOWN' & Model == 'Le Baron', 'AUTOMATIC', Transmission_Type))

#cars %>% filter(Make == 'Dodge' & Model == 'RAM 150') # MANUAL
cars <- cars %>% mutate(Transmission_Type = ifelse(Transmission_Type=='UNKNOWN' & Model == 'RAM 150', 'MANUAL', Transmission_Type))

#lastly we want to find all the different distinct market categories
#We see they are 72 different categories. This could potentially be left out due to variables are mentioned in other variables
cars %>% distinct(Market_Category)

#Let's classify the N/A as 'Non-Exotic' here
cars = cars %>% mutate(Market_Category = ifelse(Market_Category == 'N/A', 'Non-Exotic',Market_Category))
#The market category is difficult to define here, we know each other specific items are in other variables which represents the car as well 
#Such as The vehicle style and the engine fuel type

#Convert some column into factors and or numerical values
cars$Make = factor(cars$Make)

#Convert Model into a facotr
cars$Model = factor(cars$Model)

#Convert Engine Fuel Type to a factor
cars$Engine_Fuel_Type = factor(cars$Engine_Fuel_Type)

#Convert Transmission type to a factor
cars$Transmission_Type = factor(cars$Transmission_Type)

#convert market category to a factor
cars$Market_Category = factor(cars$Market_Category)

#convert vehicle size into a factor
cars$Vehicle_Size = factor(cars$Vehicle_Size)

#convert vehicle style into a factor
cars$Vehicle_style = factor(cars$Vehicle_style)

#Convert driven wheels to a numeric value and numeric data type
cars = cars %>% mutate(Driven_Wheels = ifelse(Driven_Wheels == 'rear wheel drive', 1, Driven_Wheels))
cars = cars %>% mutate(Driven_Wheels = ifelse(Driven_Wheels == 'all wheel drive', 2, Driven_Wheels))
cars = cars %>% mutate(Driven_Wheels = ifelse(Driven_Wheels == 'front wheel drive',3,Driven_Wheels))
cars = cars %>% mutate(Driven_Wheels = ifelse(Driven_Wheels == 'four wheel drive',4,Driven_Wheels))

cars$Driven_Wheels = as.numeric(cars$Driven_Wheels)

#We will now conduct some explanatory data analysis to see what we can find out about the data here
#Our question of interest is the Popularity variable here and how it is impacting the MSRP
p = cars %>% ggplot(aes(x = Popularity, y = MSRP, colour = Make)) + geom_point()
#We also see some outliers here with BMW/ Bugatti being so high in MSRP could potentially alter the regression model

#Let's look at the MSRP and other factors
MSRP_Style = cars %>% ggplot(aes(x = Vehicle_style, y = MSRP, colour = Vehicle_style)) + geom_boxplot() +  ylim(2000,2070000)
#We see that the convertible and coupe have the highest averages per group in MSRP here

#Let's look at the MSRP and Transmission Type
MSRP_Transmission = cars %>% ggplot(aes(x=Transmission_Type, y = MSRP, colour = Transmission_Type)) + geom_boxplot() + ylim(2000,2070000)

#Let's look at the MSRP and Vehicle Size
MSRP_Size = cars %>% ggplot(aes(x=Vehicle_Size, y = MSRP, colour = Vehicle_Size)) + geom_boxplot() + ylim(2000,2080000)
#We see, which makes sense that most compact cars are cheaper in MSRP compared to large and midsize, with a few outliers of the compact cars having high MSRP most likely due to high performance sports cars

#Let's look at the MSRP and Engine Fuel Type
MSRP_Engine = cars %>% ggplot(aes(x = Engine_Fuel_Type, y = MSRP, colour = Engine_Fuel_Type)) + geom_boxplot() + ylim(2000,2080000)

#Let's look at the MSRP and Car Model / Car Make
MSRP_Model = cars %>% ggplot(aes(x = Model, y = MSRP, colour = Model)) + geom_boxplot() + ylim(2000,2080000) # Won't load due to the amount of car models

#Let's examine closely at these potential interaction pairings
Style_Door = cars %>% ggplot(aes(x = Vehicle_style, y = Number_of_Doors, colour = Vehicle_style)) + geom_boxplot()

#Examining specifically MPG and car size as a potential interaction pairing
Size_MPG = cars %>% ggplot(aes(x = Vehicle_Size, y = Highway_MPG, colour = Vehicle_Size)) + geom_boxplot()

#Examining the number of wheels on the car and the transmission type
Wheels_Trans = cars %>% ggplot(aes(x = Transmission_Type, y = Driven_Wheels, colour = Transmission_Type)) + geom_boxplot()

#Examining the Engine Fuel Type and the Cylinders
Fuel_Cylinders = cars %>% ggplot(aes(x = Engine_Fuel_Type, y = Engine_Cylinders, colour = Engine_Fuel_Type)) + geom_boxplot()

#Let's look at the scatterplots of all the numerical values
pairs(cars[,c(3,5:6,8:9,13:16)], col = cars$Engine_Fuel_Type)

pairs(cars[,c(3,5:6,8:9,13:16)], col = cars$Transmission_Type) #Seems to be something here 

pairs(cars[,c(3,5:6,8:9,13:16)], col = cars$Vehicle_Size)

pairs(cars[,c(3,5:6,8:9,13:16)], col = cars$Vehicle_style)

#Let's examine multicollinearity in variables

#Look at pairwise correlation matrix
x = cars[,c(3,5:6,8:9,13:15)]
ggpairs(x)
cor(x)

#from looking at this matrix we can examine that Engine Cylinder and Engine HP are highly correlated 
#we can also see that city mpg and highway mpg is also highly correlated
#We determine to drop Engine HP and city mpg from our explanatory variables and examine the scatterplot again
x = cars[,c(3,6,7:8,13,15)]
ggpairs(x)

#We will break the data up into 3 parts, train, test and validation that will be used
set.seed(1234)
index = sample(seq(1,3),size = nrow(cars),replace = TRUE,prob = c(0.8,0.1,0.1))
train = cars[index==1,]
test = cars[index==2,]
validate = cars[index==3,]


#After examine these, we will now conduct a full model with these new variables and see if any transformations are to be made
full.model = lm(MSRP~., data=train)
summary(full.model) #Model gave us many coefficients
plot(full.model) #Didn't give us an accurate diagnostics
#right away we see some adjustments that need to be made.. potentially removing some variables like Make, Model, Market_Category and the multicollinearity vairables of City_MPG and Engine_HP
#after examining potential interaction we will insert them here as well

detect_lin_dep(model.matrix(full.model4,data = train, model = 'pooling'))


#Let's do a transformation on the MSRP variable then maybe the explanatory variables
full.model4 = lm(log(MSRP)~Year+Engine_Fuel_Type*Engine_Cylinders + Transmission_Type*Driven_Wheels + Number_of_Doors*Vehicle_style + Vehicle_Size*Highway_MPG + Popularity, data=train)
summary(full.model4)
plot(full.model4)

vcov(full.model4)
#Feature selection process
reg.fwd = regsubsets(log(MSRP)~Year+Engine_Fuel_Type*Engine_Cylinders+Transmission_Type*Driven_Wheels + Number_of_Doors*Vehicle_style + Vehicle_Size*Highway_MPG + Popularity, method = 'forward', nvmax =14 ,data=train)
reg.bwd = regsubsets(log(MSRP)~Year+Engine_Fuel_Type*Engine_Cylinders+Transmission_Type*Driven_Wheels + Number_of_Doors*Vehicle_style + Vehicle_Size*Highway_MPG + Popularity, method = 'backward', nvmax =14,data=train)
coef(reg.fwd,9)
coef(reg.bwd,9)


pairs(train[,c(3,6,8,9,13,15)])
y = train[,c(3,6,8,9,13,15)]
ggpairs(y)








#We will break the data up into 3 parts, train, test and validation that will be used
set.seed(1234)
index = sample(seq(1,3),size = nrow(cars),replace = TRUE,prob = c(0.8,0.1,0.1))
train = cars[index==1,]
test = cars[index==2,]
validate = cars[index==3,]

#Taking the log of MSRP
train = train %>% mutate(log_MSRP = log(MSRP))
test = test %>% mutate(log_MSRP = log(MSRP))

#Using lasso, will not take into consideration of interaction with variables here
library(glmnet)
a = train[,17] #Define the response variable
b = data.matrix(train[,c(3,4,6,7,8,9,11,12,13,15)]) #Put all the explanatory variables in a data matrix (took out highly correlated ones before hand)
c_test = data.matrix(test[,c(3,4,6,7,8,9,11,12,13,15)]) #explanatory variables of the test set
d_test = test[,17] #response variable of the test set
#Specify the range for lambda
lambdas = 10^seq(3,-2,by=-.1)

#Lasso regression using the response/explanatory variables
lasso_model = glmnet(b,a,alpha = 1, lambda = lambdas)

#Will use cross validation to work out and print out a graph displaying the various lambdas
cv_fit = cv.glmnet(b,a,alpha=1,lambda = lambdas)
plot(cv_fit) #lowest point in the curve indicates the optimal lambda

#displaying the optimal lambda the lowest lambda here
optimal_lambda = cv_fit$lambda.min
optimal_lambda

#Predicting value and computing the R2 value for the data we trained on
b_predicted = predict(lasso_model, s = optimal_lambda, newx = c_test)

#test MSE for lasso
testMSE = mean((d_test - b_predicted)^2)
testMSE

#Define coefficents of using for the lasso regression
coef(lasso_model,s=optimal_lambda)


#Using to predict on train set
predicted = predict(lasso_model, s=optimal_lambda, newx = b)

#Sum of square total and error
sst = sum((a - mean(a))^2)
sse = sum((predicted - a)^2)

#R-squared
rsq = 1 - sse/sst
rsq

#Summary stats of regression model
lasso_print = lm(log(MSRP)~Year+Engine_Fuel_Type+Engine_Cylinders+Transmission_Type+Driven_Wheels+Number_of_Doors+Vehicle_Size+Highway_MPG+Popularity,data=train)
summary(lasso_print)
confint(lasso_print)


#Conduct Knn Regression / Regression Tree




