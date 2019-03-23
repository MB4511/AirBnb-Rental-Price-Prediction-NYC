## Kaggle-Project--Airbnb-

Predicting Airbnb rental prices in New York CIty

## Introduction

The aim of the project was to predict the rental prices of over 25,000 Airbnb listings in NYC. The predictions had to be made using 95 variables which made 
feature selection extremely important. To check for the accuracy of predictions, RMSE(root mean squared error) was computed.

## Exploratory Data Analysis

To get familiar with this fairly complicated dataset, I began inspecting various variables and visualizing the relationship between various variables.
Some of the steps I took for EDA were:

a) Removing unncessary variables from the dataset(used feature selection techniques like Lasso Regression, Forward selection).

b) ggplots and data visualization tools like Tableau to get a sense of how variables interact to make a rental costlier or not. 

c) correlations between variables before performing machine learning techniques.

## Data Cleaning

Now came the most challenging part of the project. To get the most out of the statistical modeling techniques, the dataset had to be 
cleaned which took weeks of hard work and logical thinking.

Some steps taken were:

a) Imputing Missing data: I used MICE imputation, kNN method and even median imputation for certain variables. However, I had to go back and forth 
to check for the best method to handle missing values. 

b) One hot encoding: Many variables were character variables which were one hot encoded for faster computation and storage purposes.

c) Dealing with relevant variables: Some variables like Property Type and neighbourhood cleansed were extremely important but had too much information which
had to be reduced to make certain algorithms such as Random Forest work. To reduce the levels of properties, I came up with a threshold value of listings for each property 
below which all the levels were removed. I only included the top 5 properties to have a more insightful prediction. 

## Feature Engineering

In order to make more accurate predictions, I decided to use information about amenities as separate variables to give more weight to
my models. for exaqmple, business amenities such as laptop friendly, doorman services, iron and dryer etc were all factors which have a say
in the pricing of a rental.

Also, information about number of amenities listed was taken into account as more the amenities, higher the chance for a increase in price of rental

## Supervised Machine Learning

Techniques applied: 

## 1) Linear Regression

After finalizing the few variables which seemed the most relevant using feature selection, I tested few models by fitting them with
linear regression. The method was decent enough to give some really insightful and interpretable analysis but the accuracy was not good enough to 
make an impact in the leaderboard.

Lowest RMSE achieved: 66

## 2) Random Forest

Random forest showed a drastic improvement in accuracy but the computation speed was too high and hence inefficient for 95 variables 
with some variables having a lot of levels.

Lowest RMSE achieved: 55

## 3) Gradient Boosting

Gradient Boosting performed slightly better than Random forest on the basis of accuracy but this particular algorithm required a 
lot of effort to hypertune the parameter. But once I figured the paramaters using cross validation and grid search, it perfomed well as 
it took lesser time to compute the model. 

Lowest RMSE achieved: 52

## 4) XGBoost

XGboost required a lot more effort to tune the parameters, even more effort than gbm demanded. However, the algorithm gave 
even more accurate predictions with less computation time.

Lowest RMSE achieved: 50







