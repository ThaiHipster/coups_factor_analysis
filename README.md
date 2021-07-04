# Coups Factor Analysis

**Overview**

The goal of this project was to build a model that could predict the total number of coups in a given year based on the fewest number of global socio-economic variables. This project was inspired by my own personal experience living through three peaceful coups growing up in Thailand and wondering if there were any global factors that could indicate the likelihood of coups. I used a data set of the total number of coups, the number of successful coups, and the number of failed coups that was curated by researchers at the University of Kentucky. I used a general linear model in R and the poisson distribution to create my model and estimates. In the end I found that the two variables with the highest predictive power were the Human Development Index for a given year and the Global GDP which the model indicated holding everything else constant predicted an increase in 1.8 coup attempts for every 10 trillion dollar increase in global GDP.  


**Analysis Process**

The goal of this analysis was to see what values were most influential in predicting the total number of coups, not necessarily to predict the number of coups the next year. For this reason I used a more traditional statistical analysis approach not a machine learning approach. To start I aggregated potential global and socio-economic features into an aggregated time series dataset overtime. The features included were:  
Successful coups
Failed coups
Number of People in Poverty
Population
Human_Development_Index
National Employment 
World_GDP 
Number of autocracies and democracies
World population by political regime 
I then ran multiple iterations of a poisson model removing variables that didn’t pass a chi-square test for significance. After pruning the features of the model I arrived at a final model that predicted the total number of coups in a given year using Global GDP and the Human Development Index. To arrive at this final model I went through multiple iterations of data cleaning eventually using smaller datasets that had the highest number of features to use for building the model. Additionally, in my original model the variables weren’t normalized so I normalized those variables with the most differences in order to make the model results more easily interpretable. 

**Results and Further Questions**

In comparing the models, the best model seems to be a model predicting the total coups by World GDP and HDI. In re-adjusting the World GDP data to be smaller there seems to be an effect of around 1.8 more coups for every $10 trillion dollar increase in global GDP.  These results could be primarily due to the increase in the number of coups over the time period, or the fact that as there is more money throughout the world there are also more democracies leading to more value gained from a successful coups . For future investigation it would be interesting to investigate the effect of adding in time as a predictive variable as well as a deeper investigation of the specific predictors of failed vs. successful coups

