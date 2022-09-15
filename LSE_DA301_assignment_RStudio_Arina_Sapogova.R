## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyr)

# Install and import necessary packages.
install.packages("ggpubr")
install.packages("car")
library(carData)
library(ggpubr)

# Determine and set the current working directory.
getwd() 
setwd(dir='/users/anastasiamakareva/LSE_DA301_ASSIGNMENT_files')

# Import the data set.
read.table(file='/users/anastasiamakareva/LSE_DA301_ASSIGNMENT_files/turtle_sales.csv', sep='\t',
           header=TRUE, stringsAsFactors=FALSE)
read.csv(file.choose(), header=T)
readfile <- read.csv('turtle_sales.csv')


# Print the data frame.
sales_data <- read.csv('turtle_sales.csv')
View(sales_data)

# Create a new data frame from a subset of the sales data frame.
# Install and import necessary packages.
install.packages("dplyr")
library(dplyr)

# Remove unnecessary columns. 
sales_data_new <- subset(sales_data, select = c(Product, Platform, NA_Sales, EU_Sales, Global_Sales))

# View the data frame.
View(sales_data_new)


# View the descriptive statistics.
str(sales_data_new)
summary(sales_data_new)

################################################################################

# 2. Review plots to determine insights into the data set.

# Install and import necessary packages.
install.packages("ggplot2")
library(ggplot2)

## 2a) Scatterplots
# Create scatterplots.
qplot(Product, NA_Sales, data=sales_data_new)
qplot(Product, EU_Sales, data=sales_data_new)
qplot(Product, Global_Sales, data=sales_data_new)
#--> The resulting scatterplots shows several outliers in the data.  

## 2b) Histograms
# Create histograms.
qplot(NA_Sales, data=sales_data_new)
qplot(EU_Sales, data=sales_data_new)
qplot(Global_Sales, data=sales_data_new)
#--> The resulting histograms show the NA_Sales,EU_Sales,Global_Sales variables distributed across 10 bins in a histogram. The distribution is skewed to the right (right-skewed data: mean > mode)


# Adjust histogram bins.
ggplot(sales_data_new, aes(x=NA_Sales)) + geom_histogram(bins=10)
ggplot(sales_data_new, aes(x=EU_Sales)) + geom_histogram(bins=10)
ggplot(sales_data_new, aes(x=Global_Sales)) + geom_histogram(bins=10)
#--> The resulting boxplots shows several outliers in the data.  



## 2c) Boxplots
# Create boxplots.
qplot(Product, NA_Sales, data=sales_data_new, geom='boxplot')
qplot(Product, EU_Sales, data=sales_data_new, geom='boxplot')
qplot(Product, Global_Sales, data=sales_data_new, geom='boxplot')
## --> Data above the median is more dispersed. There are several outliers at the higher extreme.

###############################################################################

# 3. Observations and insights

# Observations and insights are noted under the code lines and in the analytical report.




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales_data_new)

# Check output: Determine the min, max, and mean values.

## Min values
aggregate(NA_Sales~Platform, sales_data_new, min)
aggregate(EU_Sales~Platform, sales_data_new, min)
aggregate(Global_Sales~Platform, sales_data_new, min)

min_sales <- sales_data_new %>% group_by(Platform) %>%
  summarise(min_NA_Sales=min(NA_Sales),
            min_EU_Sales=min(EU_Sales),
            min_Global_Sales=min(Global_Sales),
            .groups='drop')
View(min_sales)


## Max values
aggregate(NA_Sales~Platform, sales_data_new, max)
aggregate(EU_Sales~Platform, sales_data_new, max)
aggregate(Global_Sales~Platform, sales_data_new, max)

max_sales <- sales_data_new %>% group_by(Platform) %>%
  summarise(max_NA_Sales=max(NA_Sales),
            max_EU_Sales=max(EU_Sales),
            max_Global_Sales=max(Global_Sales),
            .groups='drop')
View(max_sales)

## Mean values
aggregate(NA_Sales~Platform, sales_data_new, mean)
aggregate(EU_Sales~Platform, sales_data_new, mean)
aggregate(Global_Sales~Platform, sales_data_new, mean)

mean_sales <- sales_data_new %>% group_by(Platform) %>%
  summarise(mean_NA_Sales=mean(NA_Sales),
            mean_EU_Sales=mean(EU_Sales),
            mean_Global_Sales=mean(Global_Sales),
            .groups='drop')
View(mean_sales)

# View the descriptive statistics.
summary(min_sales)
summary(max_sales)
summary(mean_sales)




###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_per_product = sales_data_new %>% group_by(Product) %>%
  summarise(Total_NA_Sales = sum(NA_Sales), 
            Total_EU_Sales = sum(EU_Sales), 
            Total_Global_Sales = sum(Global_Sales),
            .groups = 'drop')

# View the data frame.
View(sales_per_product)
# Explore the data frame.
summary(sales_per_product)

### Create a column combining total NA_Sales, EU_Sales and Global_Sales per product
combined_sales_per_product = sales_data_new %>% group_by(Product) %>%
  summarise(Total_Sales = sum(NA_Sales, EU_Sales, Global_Sales),
            .groups = 'drop')

# View the data frame.
View(combined_sales_per_product)

# Explore the data frame.
summary(combined_sales_per_product)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product, Total_NA_Sales, data=sales_per_product)
qplot(Product, Total_EU_Sales, data=sales_per_product)
qplot(Product, Total_Global_Sales, data=sales_per_product)
#--> The resulting plots shows several outliers in the data. The distribution is less dense due to the aggregaated values.


# Create histograms.
qplot(Total_NA_Sales, data=sales_per_product, bins = 15)
qplot(Total_EU_Sales, data=sales_per_product, bins = 15)
qplot(Total_Global_Sales, data=sales_per_product, bins = 15)
# --> The distribution is skewed to the right.

# Create boxplots.
qplot(Product, Total_NA_Sales, data=sales_per_product, geom='boxplot')
qplot(Product, Total_EU_Sales, data=sales_per_product, geom='boxplot')
qplot(Product, Total_Global_Sales, data=sales_per_product, geom='boxplot')
### --> Data above the median is more dispersed. There are several outliers at the higher extreme.

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_per_product$total_sales)

qqline(sales_per_product$total_sales)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)


# Perform Shapiro-Wilk test.
shapiro.test(sales_per_product$total_sales)


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_per_product$total_sales) 
kurtosis(sales_per_product$total_sales)


## 3d) Determine correlation
# Determine correlation.

cor(sales_per_product$Product, sales_per_product$total_sales)



# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(combined_sales_per_product$Total_Sales)
qqline(combined_sales_per_product$Total_Sales)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(combined_sales_per_product$Total_Sales)


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(combined_sales_per_product$Total_Sales) 
kurtosis(combined_sales_per_product$Total_Sales)


## 3d) Determine correlation
# Determine correlation.
cor(combined_sales_per_product$Product, combined_sales_per_product$Total_Sales)



###############################################################################


# 3. Determine the normality of the data set.

View(sales_per_product)

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_per_product$total_sales)
qqline(sales_per_product$total_sales)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)


# Perform Shapiro-Wilk test.
shapiro.test(sales_per_product$total_sales)


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_per_product$total_sales) 
kurtosis(sales_per_product$total_sales)


## 3d) Determine correlation
# Determine correlation.
cor(sales_per_product$Product, sales_per_product$total_sales)


###############################################################################
# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

install.packages("ggpubr")
library(ggplot2)

### Relationship between Product ID and Total Sales
ggplot(data = combined_sales_per_product, 
       mapping = aes(x = Product, y = Total_Sales)) + 
  geom_point(color = 'black',
             alpha = .5,
             size = 3) +
  geom_smooth(method = lm, size = 1.5) +
  scale_x_discrete("Product ID") +
  scale_y_continuous("Total Sales (Million Pounds)") +
  labs(title = "Relationship between product ID and total sales")
#--> Negative linear relationship between Product ID and Total Sales. 

### Relationship between Global Sales, Sales in North America and Sales in the European Union
ggplot(data = sales_per_product, 
       mapping = aes(x = Total_NA_Sales, y = Total_Global_Sales)) + 
  geom_point(color = 'black',
             alpha = .5,
             size = 3) +
  geom_smooth(method = lm, size = 1.5) +
  scale_x_discrete("Total sales in North America (Million Pounds)") +
  scale_y_continuous("Total sales in the world (Million Pounds)") +
  labs(title = "Relationship between total sales in the world and North America")
#--> Positive linear relationship between Sales in North America and Global Sales



ggplot(data = sales_per_product, 
       mapping = aes(x = Total_EU_Sales, y = Total_Global_Sales)) + 
  geom_point(color = 'black',
             alpha = .5,
             size = 3) +
  geom_smooth(method = lm, size = 1.5) +
  scale_x_discrete("Total sales in the European Union (Million Pounds)") +
  scale_y_continuous("Total sales in the world (Million Pounds)") +
  labs(title = "Relationship between total sales in the world and the European Union")
#--> Positive linear relationship between Sales in North America and Global Sales


ggplot(data = sales_per_product, 
       mapping = aes(x = Total_NA_Sales, y = Total_EU_Sales)) + 
  geom_point(color = 'black',
             alpha = .5,
             size = 3) +
  geom_smooth(method = lm, size = 1.5) +
  scale_x_discrete("Sales in North America (Million Pounds)") +
  scale_y_continuous("Total sales in the European Union (Million Pounds)") +
  labs(title = "Relationship between total sales in the European Union and North America")
#--> Positive linear relationship between Sales in the European Union and North America



### Check the significance of the models.
ggscatter(sales_per_product, x = "Total_NA_Sales", y = "Total_Global_Sales", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total sales in North America (Million Pounds)", ylab = "Total sales in the world (Million Pounds)")
# R=0.92, p < 2.2e-16

ggscatter(sales_per_product, x = "Total_EU_Sales", y = "Total_Global_Sales", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total sales in the European Union (Million Pounds)", ylab = "Total sales in the world (Million Pounds)")
# R=0.85, p < 2.2e-16

ggscatter(sales_per_product, x = "Total_NA_Sales", y = "Total_EU_Sales", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Sales in North America (million pounds)", ylab = "Sales in the European Union (million pounds)")
# R=0.62, p < 2.2e-16

#--> The strongest relationship is between sales in the world (dependent variable y) and sales in NA (independent variable x) 


###############################################################################

# 5. Observations and insights
# Observations and insights are noted under the code lines and in the analytical report.



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(sales_per_product)
View(sales_data_new)

# Determine a summary of the data frame.
summary(sales_per_product)

###############################################################################

# 2. Create a simple linear regression model

# Install and import necessary packages.
install.packages("forecast")
library(forecast)

# Install and import necessary packages.
install.packages("tseries")
library(tseries)

## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
cor(sales_per_product)
View(cor(sales_per_product))

### (1) Test the relationship between Global Sales and NA Sales
plot(sales_per_product$Total_NA_Sales, sales_per_product$Total_Global_Sales)

model1 <- lm(Total_Global_Sales ~ Total_NA_Sales, data = sales_per_product)
summary(model1)

### (2) Test the relationship between Global Sales and EU Sales
plot(sales_per_product$Total_EU_Sales, sales_per_product$Total_Global_Sales)

model2 <- lm(Total_Global_Sales ~ Total_EU_Sales, data = sales_per_product)
summary(model2)

### (3) Test the relationship between NA Sales and EU Sales
plot(sales_per_product$Total_NA_Sales, sales_per_product$Total_EU_Sales)

model3 <- lm(Total_EU_Sales ~ Total_NA_Sales, data = sales_per_product)
summary(model3)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.

### MODEL 1
# Plot the residuals.
plot(model1$residuals)
# Specify the coefficients and add a line of best fit.
abline(coefficients(model1))

# Calculate the sum of squares error (SSE) to determine the accuracy of the forecasting model.
SSE1 = sum(model1$residuals^2)
SSE1

### MODEL 2
# Plot the residuals.
plot(model2$residuals)
# Specify the coefficients and add a line of best fit.
abline(coefficients(model2))

# Calculate the sum of squares error (SSE) to determine the accuracy of the forecasting model.
SSE2 = sum(model2$residuals^2)
SSE2

### MODEL 3
# Plot the residuals.
plot(model3$residuals)
# Specify the coefficients and add a line of best fit.
abline(coefficients(model3))

# Calculate the sum of squares error (SSE) to determine the accuracy of the forecasting model.
SSE3 = sum(model3$residuals^2)
SSE3

#--> Very high SEE means that the model is a poor fit. Thus, the simple linear regression models are not accurate in terms of forecasting.
###############################################################################

# 3. Create a multiple linear regression model

# Select only numeric columns from the original data frame.
sales_data_numeric <- subset(sales_data, select = c(NA_Sales, EU_Sales, Global_Sales))
View(sales_data_numeric)

# Multiple linear regression model.
cor(sales_data_numeric)
View(cor(sales_data_numeric))
model4 <- lm(Global_Sales ~ NA_Sales+EU_Sales, data = sales_data_numeric)

# Descriptve statistics
summary(model4)
## --> The adjusted R-squared is 0.9685 which means that the multiple linear regression model (Model #4) is statistically significant.
## --> It can be inferred that there is a positive correlation between Global Sales (dependent varibale) and NA/EU Sales (independent variable). The more NA/EU Sales the company has, the larger the volume of Global Sales.

# Plot the model
plot(model4$residuals)
###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

## (1) NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
given_values1 <- data.frame(NA_Sales=c(34.02), EU_Sales=c(23.80))
predict(model4, newdata=given_values1)
### --> Predicted global sales based on the given values are equal to 71.469 M pounds.

# Predict the values with confidence interval. 
predict(model4, newdata=given_values1, interval = 'confidence')
### --> The output predicted with confidence interval shows that of sales in North America and the European Union are equal to the given values, Global Sales are between the 70.16242 and 72.77472 range.

## (2) NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
given_values2 <- data.frame(NA_Sales=c(3.93), EU_Sales=c(1.56))
predict(model4, newdata=given_values2)
### --> Predicted global sales based on the given values are equal to 6.856 M pounds.

# Predict the values with confidence interval. 
predict(model4, newdata=given_values2, interval = 'confidence')
### --> The output predicted with confidence interval shows that of sales in North America and the European Union are equal to the given values, Global Sales are between the 6.71842 and 6.993745 range.


## (3) NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
given_values3 <- data.frame(NA_Sales=c(2.73), EU_Sales=c(0.65))
predict(model4, newdata=given_values3)
### --> Predicted global sales based on the given values are equal to 4.248 M pounds.

# Predict the values with confidence interval. 
predict(model4, newdata=given_values3, interval = 'confidence')
### --> The output predicted with confidence interval shows that of sales in North America and the European Union are equal to the given values, Global Sales are between the 4.102094 and 4.394639 range.


## (4) NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
given_values4 <- data.frame(NA_Sales=c(2.26), EU_Sales=c(0.97))
predict(model4, newdata=given_values4)
### --> Predicted global sales based on the given values are equal to 4.135 M pounds.

# Predict the values with confidence interval. 
predict(model4, newdata=given_values4, interval = 'confidence')
### --> The output predicted with confidence interval shows that of sales in North America and the European Union are equal to the given values, Global Sales are between the 4.009122 and 4.260365 range.

## (5) NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
given_values5 <- data.frame(NA_Sales=c(22.08), EU_Sales=c(0.52))
predict(model4, newdata=given_values5)
### --> Predicted global sales based on the given values are equal to 26.432 M pounds.

# Predict the values with confidence interval. 
predict(model4, newdata=given_values5, interval = 'confidence')
### --> The output predicted with confidence interval shows that of sales in North America and the European Union are equal to the given values, Global Sales are between the 25.41334 and 27.44979 range.

###############################################################################

# 5. Observations and insights
# Observations and insights are noted under the code lines and in the analytical report.


###############################################################################
###############################################################################




