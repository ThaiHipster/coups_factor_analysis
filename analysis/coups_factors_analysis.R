###########################################
# Title: An Analysis of Global Factors Leading to Coups
# Author: Robert Alward
# Date: 1/14/2021
###########################################

# Installing Libraries
library("readxl")
library(dplyr)
library(tidyr)
library(naniar)
library(sandwich) 
library(msm)
library(ggplot2)
library(tidyverse)

############################################################################
#### 0. Reading in Data                                                #####
############################################################################


#X-Variables: Coups Datasets
coups_x1.dat <- read.table ("1.powell_thyne_coups_final.txt",header=TRUE)
count <- c(rep(1,478))
coups_x1.dat <- cbind(coups_x1.dat, count)
coups_tot_sum <- aggregate(coups_x1.dat$count, by=list(year=coups_x1.dat$year),FUN=sum)
names(coups_tot_sum)[1] <- "Year"
sapply(coups_tot_sum,mean)
sapply(coups_tot_sum,var)


coups_win.dat <- subset(coups_x1.dat, coup=="2")
coups_win_sum <- aggregate(coups_win.dat$count, by=list(year=coups_win.dat$year),FUN=sum)
names(coups_win_sum)[2] <- "coups_win_sum"
names(coups_win_sum)[1] <- "Year"


coups_fail.dat <- subset(coups_x1.dat, coup=="1")
coups_fail_sum <- aggregate(coups_fail.dat$count, by=list(year=coups_fail.dat$year),FUN=sum)
names(coups_fail_sum)[2] <- "coups_fail_sum"
names(coups_fail_sum)[1] <- "Year"


#Y-Variables: Raw Datasets
poverty_y2.dat <- read_excel ("2.Poverty.xlsx") 
pop_y3.dat <- read.csv ("3.Population.csv")
HDI_y4.dat <- read.csv("4.Human_Development_Index(HDI).csv") 
ILO_y5.dat <- read_excel("5.ILO_Employment.xlsx") 
GDP_y6.dat <- read_excel("6.World_GDP.xlsx") 
democracy_y7.dat <- read.csv("7.autocracies-and-democracies.csv")
democratic_pop_y9.dat <- read.csv("9.world-pop-by-political-regime.csv") 

############################################################################
####                    I. Data Wrangling                              #####
############################################################################

### 1. Individual Datasets Data Wrangling

#Poverty Data
poverty_y2.dat
poverty_clean <- na.omit(poverty_y2.dat)
GDP_per_capita <- poverty_clean[1,]

#GDP Headers
GDP_headers <- names(GDP_per_capita)
names(GDP_headers)<- GDP_headers

#Population: 
population_1 <- pop_y3.dat[1,]
population_1 <- rbind(population_1,GDP_headers)
population_2 <- as.data.frame(t(as.matrix(population_1)))
population_3 <- population_2[-1:-4,]
names(population_3) <- c("Population","Year")
population_3$Year = substr(population_3$Year,1,nchar(population_3$Year)-9)


#Poverty Percentage $1.90: 
poverty_clean
low_poverty_clean <- poverty_clean[5,]
low_poverty_clean_1 <- rbind(low_poverty_clean,GDP_headers)
low_poverty_2 <- as.data.frame(t(as.matrix(low_poverty_clean_1)))
low_poverty_3 <- low_poverty_2[-1:-4,]
names(low_poverty_3) <- c("Poverty Rate $1.90","Year")
low_poverty_3$Year = substr(low_poverty_3$Year,1,nchar(low_poverty_3$Year)-9)

#Poverty Percentage $3.20: 
mid_poverty_clean <- poverty_clean[4,]
mid_poverty_clean_1 <- rbind(mid_poverty_clean,GDP_headers)
mid_poverty_2 <- as.data.frame(t(as.matrix(mid_poverty_clean_1)))
mid_poverty_3 <- mid_poverty_2[-1:-4,]
names(mid_poverty_3) <- c("Poverty Rate $3.20","Year")
mid_poverty_3$Year = substr(mid_poverty_3$Year,1,nchar(mid_poverty_3$Year)-9)

#Poverty Percentage $5.50: 
high_poverty_clean <- poverty_clean[3,]
high_poverty_clean_1 <- rbind(high_poverty_clean,GDP_headers)
high_poverty_2 <- as.data.frame(t(as.matrix(high_poverty_clean_1)))
high_poverty_3 <- high_poverty_2[-1:-4,]
names(high_poverty_3) <- c("Poverty Rate $5.50","Year")
high_poverty_3$Year = substr(high_poverty_3$Year,1,nchar(high_poverty_3$Year)-9)

#GDP per Capita: 
GDP_per_capita_1 <- GDP_per_capita[1,]
GDP_per_capita_1 <- rbind(GDP_per_capita_1,GDP_headers)
GDP_per_capita_2 <- as.data.frame(t(as.matrix(GDP_per_capita_1)))
GDP_per_capita_3 <- GDP_per_capita_2[-1:-4,]
names(GDP_per_capita_3) <- c("GDP per Capita","Year")
GDP_per_capita_3$Year = substr(GDP_per_capita_3$Year,1,nchar(GDP_per_capita_3$Year)-9)

#HDI: 
HDI_clean_1 <- HDI_y4.dat[-1:-4,]
HDI_clean_2 <- HDI_clean_1[colSums(!is.na(HDI_clean_1)) > 0]
HDI_headers <- HDI_clean_2[1,]

HDI_clean_3 <- HDI_clean_2
names(HDI_clean_3) <- as.matrix(HDI_headers[1,])

HDI_world <- HDI_clean_3[c(1,207),]
HDI_world_1 <- as.data.frame(t(as.matrix(HDI_world)))
HDI_world_2 <- HDI_world_1[-1:-2,]
names(HDI_world_2) <- c("Year","HDI")

#ILO Employment: 
employ <- ILO_y5.dat[1,]
employ_1 <- rbind(employ,GDP_headers)
employ_2 <- as.data.frame(t(as.matrix(employ_1)))
employ_3 <- employ_2[-1:-4,]
names(employ_3) <- c("Employment Rate","Year")
employ_3$Year = substr(employ_3$Year,1,nchar(employ_3$Year)-9)

#Total GDP: 
GDP_total <- GDP_y6.dat[1,]
GDP_total_1 <- rbind(GDP_total,GDP_headers)
GDP_total_2 <- as.data.frame(t(as.matrix(GDP_total_1)))
GDP_total_3 <- GDP_total_2[-1:-4,]
names(GDP_total_3) <- c("World GDP","Year")
GDP_total_3$Year = substr(GDP_total_3$Year,1,nchar(GDP_total_3$Year)-9)

#Democracy Number: 
dem_num <- democracy_y7.dat[,c(1,3)]

auto_num <- democracy_y7.dat[,c(1,2)]

#Democracy Population: 
dem_pop <- democratic_pop_y9.dat
names(dem_pop) <- c("Year","Population Transition","Population Colony","Population Autocracy", 
                    "Closed Anocracy","Open Anocracy", "Population Democracy")


##### 2. Dataset Combinations

# Y Vaiables: dem_pop, dem_num, auto_num, GDP_total_3, employ_3
# HDI_world_2, GDP_per_capita_3, high_poverty_3, mid_poverty_3
# low_poverty_3, 

# X Variables: coups_tot_sum, coups_win_sum, coups_fail_sum 

#### 2.1 Total datasets

tot_merge_1 <- merge(x = coups_tot_sum, y = population_3, by = "Year", all.x = TRUE) 
tot_merge_2 <- merge(x = tot_merge_1, y = dem_pop, by = "Year", all.x = TRUE) 
tot_merge_3 <- merge(x = tot_merge_2, y = dem_num, by = "Year", all.x = TRUE) 
tot_merge_4 <- merge(x = tot_merge_3, y = auto_num, by = "Year", all.x = TRUE) 
tot_merge_5 <- merge(x = tot_merge_4, y = GDP_total_3, by = "Year", all.x = TRUE) 
tot_merge_6 <- merge(x = tot_merge_5, y = employ_3, by = "Year", all.x = TRUE) 
tot_merge_7 <- merge(x = tot_merge_6, y = HDI_world_2, by = "Year", all.x = TRUE) 
tot_merge_8 <- merge(x = tot_merge_7, y = GDP_per_capita_3, by = "Year", all.x = TRUE) 
tot_merge_9 <- merge(x = tot_merge_8, y = high_poverty_3, by = "Year", all.x = TRUE) 
tot_merge_10 <- merge(x = tot_merge_9, y = mid_poverty_3, by = "Year", all.x = TRUE) 
tot_merge_11 <- merge(x = tot_merge_10, y = low_poverty_3, by = "Year", all.x = TRUE)
names(tot_merge_11)[2] <- "coups_total" 

na_added <- tot_merge_11 %>% replace_with_na(replace = list(Population= ".."))
tot_merge_12 <- tot_merge_11 %>% replace_with_na_all (condition = ~.x == "..")

col.name.tot <- names(tot_merge_12)
tot_merge_12[col.name.tot] <- sapply(tot_merge_12[col.name.tot],as.numeric)

# 2.1.a: Checking dataset
str(tot_merge_12)

#### 2.2 Wins and Fails datasets

win_merge_1 <- merge(x = tot_merge_11, y = coups_win_sum, by = "Year", all.x = TRUE)
names(win_merge_1)[2] <- "coups_total"
win_merge_1 %>% replace_with_na_all (condition = ~.x == "..")

col.name.win <- names(win_merge_1)
win_merge_1[col.name.tot] <- sapply(win_merge_1[col.name.win],as.numeric)

fail_merge_1 <- merge(x = tot_merge_11, y = coups_fail_sum, by = "Year", all.x = TRUE)
names(fail_merge_1)[2] <- "coups_total"
fail_merge_1 %>% replace_with_na_all (condition = ~.x == "..")

col.name.fail <- names(fail_merge_1)
fail_merge_1[col.name.tot] <- sapply(fail_merge_1[col.name.fail],as.numeric)
str(fail_merge_1)

#### 2.3 Limited Data

lim_merge_1 <- merge (x = coups_tot_sum, y = population_3, by = "Year", all.x = FALSE) 
lim_merge_2 <- merge (x = lim_merge_1, y = dem_pop, by = "Year", all.x = FALSE) 
lim_merge_3 <- merge (x = lim_merge_2, y = dem_num, by = "Year", all.x = FALSE) 
lim_merge_4 <- merge (x = lim_merge_3, y = auto_num, by = "Year", all.x = FALSE) 
lim_merge_5 <- merge (x = lim_merge_4, y = GDP_total_3, by = "Year", all.x = FALSE) 
lim_merge_6 <- merge (x = lim_merge_5, y = employ_3, by = "Year", all.x = FALSE) #1960 - 2015: CHECK THIS
lim_merge_7 <- merge (x = lim_merge_6, y = HDI_world_2, by = "Year", all.x = FALSE) 
lim_merge_8 <- merge (x = lim_merge_7, y = GDP_per_capita_3, by = "Year", all.x = FALSE) 
lim_merge_9 <- merge (x = lim_merge_8, y = high_poverty_3, by = "Year", all.x = FALSE) 
lim_merge_10 <- merge(x = lim_merge_9, y = mid_poverty_3, by = "Year", all.x = FALSE) 
lim_merge_11 <- merge(x = lim_merge_10, y = low_poverty_3, by = "Year", all.x = FALSE)
lim_merge_11 <- lim_merge_11[-1,]

col.name <- names(lim_merge_11)
lim_merge_11[col.name] <- sapply(lim_merge_11[col.name],as.numeric)

lim_merge_11 <- lim_merge_11 %>% 
  mutate(`Small_GDP` = `World GDP`/10000000000000)

# 2.3.a: Checking Dataset
col.name
head(lim_merge_11) 
str(lim_merge_11)

############################################################################
#### II. Poisson Regression with limited Data for Total Coup Attempts   ####
############################################################################

#### Note: The Poisson Regressions with the desired 

##### 1. Full Poisson Model: Limited Data

lim.fit.full <- glm(x ~ Year + Population+`Population Transition`+`Population Colony`+
                      `Population Autocracy`+`Closed Anocracy`+`Open Anocracy`+
                      `Population Democracy`+ Democracies+Autocracies+`World GDP`+ 
                      `Employment Rate`+HDI +`GDP per Capita`+ `Poverty Rate $5.50`+
                      `Poverty Rate $3.20`+`Poverty Rate $1.90`,family = poisson,
                       data = lim_merge_11)

summary(lim.fit.full)

linear_pred_full.lim <- predict.glm(lim.fit.full, lim_merge_11)

full_log_lik <- sum(lim_merge_11$x * linear_pred_full.lim - 
                      exp(linear_pred_full.lim) - log(factorial(lim_merge_11$x)))

# full log likelihood = -42.66466

# Notes:
# Using Sandwhich package for robust standard errors
# Using Chi Squared test to check the model
# Testing different models against eachother


##### 2. 1st Reduced Poisson Model: Limited Data
# Notes: for this I am removing all indicators with a P-value over .5
# Repeat Reduction to continue to remove and reduce the model
# Larger likelihood ratio means that the original model was better
# H0 = reduced model is sufficient, if likeliehood ratio is smaller than chi squared

# Reduced Variables: World GDP, HDI, GDP per Capita


lim.fit.1 <- glm(x ~ `World GDP`+ HDI +`GDP per Capita`,family = poisson, data = lim_merge_11)

summary(lim.fit.1)

(linear_pred_1.lim <- predict.glm(lim.fit.1, lim_merge_11))

(first_log_lik <- sum(lim_merge_11$x * linear_pred_1.lim - exp(linear_pred_1.lim) - 
                      log(factorial(lim_merge_11$x))))

#Log likelihood of reduced model 1: [1] -48.06531

#Chi-squared test for reduced model 1
with(lim.fit.1, cbind(res.deviance = deviance, df = df.residual,
                        p = pchisq(deviance, df.residual, lower.tail=FALSE))) 
                        #large p-value indicates decent model fit

## test model differences with chi square test
anova(lim.fit.1, lim.fit.full, test="Chisq") 
# because p value is high full model does not hold statistically significant predictors

# test model difference with other chi square test
likelihood_ratio_1 <- -2*(first_log_lik- full_log_lik)#the larger the value the better the full model

crit_val_1 <- qchisq(.95,17-4) 

likelihood_ratio_1
crit_val_1
p_value_1 <- pchisq(likelihood_ratio_1, 17-4, lower.tail = F)
p_value_1

##### 3. 2nd Reduced Poisson Model:Limited Data
# Reduced Variables: World GDP, HDI

lim.fit.2 <- glm(x ~ `World GDP`+ HDI ,family = poisson, data = lim_merge_11)

summary(lim.fit.2)

(linear_pred_2.lim <- predict.glm(lim.fit.2, lim_merge_11))

(Second_log_lik <- sum(lim_merge_11$x * linear_pred_2.lim - exp(linear_pred_2.lim) - 
                         log(factorial(lim_merge_11$x))))
#Log likelihood of 2nd reduced model: [1] -48.28197

#Chi-squared test for 2nd reduced model
with(lim.fit.2, cbind(res.deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE))) #large p-value:.46247 indicating decent model fit


## Testing 2nd reduced model differences with chi square test
anova(lim.fit.2, lim.fit.1, test="Chisq") #because p value is high full model does not hold statistically significant predictors

# Testing model differences with other chi square test

likelihood_ratio_2 <- -2*(Second_log_lik - first_log_lik) #the larger the value the better the full model
likelihood_ratio_2

crit_val_2 <- qchisq(.95,4-3) 

likelihood_ratio_2
crit_val_2
p_value_2 <- pchisq(likelihood_ratio_2, 4-3, lower.tail = F) 
p_value_2 #likelihood is less than critical value therefor GDP per Capita is not a useful predictor of Coups

#Repeat this to continue to remove and reduce the model
#larger likelihood ratio means that the original model was better
#H0 = reduced model is sufficient, if likelihood ratio is smaller than chi squared


##### 4. 3rd reduced Poisson Model:Limited Data
# Reduced Variables: HDI 

lim.fit.3 <- glm(x ~ HDI ,family = poisson, data = lim_merge_11)

summary(lim.fit.3)

(linear_pred_3.lim <- predict.glm(lim.fit.3, lim_merge_11))

(Third_log_lik <- sum(lim_merge_11$x * linear_pred_3.lim - exp(linear_pred_3.lim) - 
                        log(factorial(lim_merge_11$x))))
#Log likelihood of 3rd reduced model: [1] -50.47732

#Chi-squared test for 3nd reduced model
with(lim.fit.3, cbind(res.deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE))) 
                      #large p-value:0.2814215 indicating decent model fit


## test Reduced model differences with chi square test
anova(lim.fit.3, lim.fit.2, test="Chisq") 
#because p value is low 2nd reduced model does hold statistically significant predictors

# Testing model difference with other chi square test
likelihood_ratio_3 <- -2*(Third_log_lik - Second_log_lik)#the larger the value the better the full model
likelihood_ratio_3

crit_val_3 <- qchisq(.95,4-3) 

likelihood_ratio_3
crit_val_3
p_value_3 <- pchisq(likelihood_ratio_3, 4-3, lower.tail = F) 
p_value_3 #likelihood is greater than critical value therefor Global GDP is a useful predictor of Coups


##### 5. 4th reduced Poisson Model:Limited Data
# Reduced Variables: HDI, `World GDP`, `Population Autocracy` 

lim.fit.4 <- glm(x ~ HDI + `World GDP` + `Population Autocracy` ,family = poisson, data = lim_merge_11)

summary(lim.fit.4)

(linear_pred_4.lim <- predict.glm(lim.fit.4, lim_merge_11))

(Fourth_log_lik <- sum(lim_merge_11$x * linear_pred_4.lim - exp(linear_pred_4.lim) - 
                         log(factorial(lim_merge_11$x))))
#Log likelihood of 4rd reduced model: [1] [1] -48.28105


#Chi-squared test for 4th reduced model
with(lim.fit.4, cbind(res.deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE))) #large p-value:0.625533 indicating decent model fit


## Testing 4th Reduced model differences with chi square test
anova(lim.fit.2,lim.fit.4 , test="Chisq") #because p value is high 4th reduced model does hold statistically significant predictors

# II. Conclusion: 
# Best Model is: lim.fit.2 <- glm(x ~ `World GDP`+ HDI ,family = poisson, data = lim_merge_11)


############################################################################
###                 III. Regression Estimate and Checks                  ###
############################################################################

summary(lim.fit.2)
# Notes: 
# The new variable Small_GDP is reducing the variable 
# Global GDP and dividing it by 10,000,000,000,000 in
# order to have more interesting analysis results.
# This new regression is based on the new variable Small_GDP

lim.fit.exp <- glm(x ~ HDI + Small_GDP ,family = poisson, data = lim_merge_11)

summary(lim.fit.exp)

exp(lim.fit.exp$coefficients)

### 1. Getting the Predicted Values for lim.fit.2 model

with(lim.fit.2, cbind(res.deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE))) 
                      #large p-value:.46247 indicating decent model fit

## 2. Calculating and storing predicted values
lim_merge_11$phat <- predict(lim.fit.2, type="response")
lim_merge_new <- lim_merge_11

## 3. Re-order data by program and then by math
lim_merge_new <- lim_merge_new[with(lim_merge_new, order(`World GDP`, HDI)), ]

## 4. Create the plot
ggplot(lim_merge_new, aes(x = `World GDP`, y = phat, colour = HDI)) +
  geom_point(aes(y = x), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "World GDP", y = "Expected number of Coups")

### 5. Estimates using lim.fit.s

(cov.lim <- vcovHC(lim.fit.2, type="HC0"))
(std.err <- sqrt(diag(cov.lim)))
r.est <- cbind(Estimate= coef(lim.fit.2), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(lim.fit.2)/std.err), lower.tail=FALSE),
               LL = coef(lim.fit.2) - 1.96 * std.err,
               UL = coef(lim.fit.2) + 1.96 * std.err)

r.est 
(rexp.est <- exp(r.est[,-3]))
exp(lim.fit.2$coefficients)

############################################################################
###                 IV. Model Outputs and Conclusions                    ###
############################################################################

### 1. Outputs

summary(lim.fit.full)
summary(lim.fit.1)
summary(lim.fit.2)
rexp.est <- exp(r.est[,-3])
print(rexp.est)

summary(lim.fit.3)
summary(lim.fit.exp)
exp(lim.fit.exp$coefficients)

### 2. Conclusion
# In looking at the models the best model seems to be a model predicting the total
# coups by World GDP and HDI. In re-adjusting the World GDP data to be smaller
# there seems to be an effect of around 1.8 more coups for every $10,000,000,000,000 
# increase in world GDP


