# LSE Data Analytics Online Career Accelerator
# DA301: Advanced Analytics for Organisational Impact

# Course 3 Final Assignment - Week 6 
# Title : Determine the optimal global sales for all the video games
# Author - Saurabh Dixit
# Date 30-June-2022

###############################################################################
# 

##### ****** START of META DATA FOR GAMES SALES DATASET ****** ########
#COLNAME  DESCRIPTION
--------- ----------------------------------------------------------
  #Rank :   World ranking of the game
  #Name : Name of the video game
  #Platform : Video game console on which the game was launched
  #Year : Year of launch 
  #Genre :  Genre of the video game
  #Publisher : Company that published the game
  #NA_Sales : Number of games sold in North America (in millions of units)
  #EU_Sales :  Number of games sold Europe (in millions of units)
  # Global_Sales : Total sales in the world (which is a sum of EU_Sales, NA_Sales and online sales) (in millions of units)
  
####### *** END OF META DATA *** ####


###### *** GOAL OF THIS PROGRAM *** ####
# Apply regression techniques to determine the optimal global sales for all the video games..
# based on the sales in North American and European stores for the upcoming financial year.
################################################################################################


##### *** PLAN OF ACTION *** ####
# 1. LOAD THE DATA AND CLEAN THE DATA
# 2. FIND THE CORRELATION BETWEEN VARIABLES

###### *** PREPARE WORKSTATION BY IMPORTING LIBRARIES *** ####

# import libraries - this might take a minute or so
# whole tidyverse package
library(tidyverse)
# Useful for importing data
library(readr) 
#Useful for data wrangling
library(dplyr) 
#Useful for data wrangling
library(tidyr) 
# Useful for creating tidy tables
library(knitr) 
# useful for working with vectors and functions
library(purrr)
# useful to create insightful summaries of data set
library(skimr)
# useful to create insightful reports on data set
library(DataExplorer)
# useful for visualisation
library(ggplot2)

###### *** LOAD AND CLEAN THE DATA SET *** ####

# choose games_sales.csv file  
games_sales <- read.csv (file.choose (), header = T)


# sense-check data set
as_tibble(games_sales)
#Dimensions of the dataset
dim(games_sales)

#View the dataset in a tabular format
View(games_sales)

#Look up the overall Structure of the dataset
str(games_sales)

##############################################################################

# CLEAN THE DATA

# sum of missing values 
sum(is.na(games_sales))

# delete all the records with missing values
games_sales_new <-na.omit(games_sales)
head(games_sales_new)

dim(games_sales_new)
sum(is.na (games_sales_new))

# Still some N/A values in Year, filter those out and store only the records that Do NOT HAVE "N/A" in Year column
games_sales_new <- games_sales_new[games_sales_new["Year"]!="N/A",]
dim(games_sales_new)

#Change data type of Year col from Char to Numeric by initializing it with the newly created object
year_num <- as.numeric(games_sales_new$Year)
class(year_num)
games_sales_new$Year = year_num
as_tibble(games_sales_new)

###### *** FIND CORRELATION *** ####
# Check correlation coefficient between Global Sales and North America Sales
cor(games_sales_new$NA_Sales, games_sales_new$Global_Sales)

# Check correlation coefficient between Global Sales and European Sales
cor(games_sales_new$EU_Sales, games_sales_new$Global_Sales)

# Check correlation coefficient between Global Sales and European Sales + N-America Sales
cor(games_sales_new$NA_Sales + games_sales_new$EU_Sales, games_sales_new$Global_Sales)


# Scatter plot showing North America Sales by Global Sales
ggplot(games_sales_new, aes(x = NA_Sales, y = Global_Sales)) +  
  geom_point() +  
  labs(x = "North America Sales in Million Units",
       y = "Global Sales in Million Units",
       title = " North America Sales ~ Global Sales in Million Units") +  
  theme(axis.text.x =element_text(size = 10)) +
  geom_smooth()

# Scatter plot showing EU Sales by Global Sales
ggplot(games_sales_new, aes(x = EU_Sales, y = Global_Sales)) +  
  geom_point() +  
  labs(x = "EU Sales in Million Units",
       y = "Global Sales in Million Units",
       title = " EU Sales ~ Global Sales in Million Units") +  
  theme(axis.text.x =element_text(size = 10)) +
  geom_smooth()


# Scatter plot showing North America + EU Sales by Global Sales
ggplot(games_sales_new, aes(x = NA_Sales + EU_Sales, y = Global_Sales)) +  
  geom_point() +  
  labs(x = "North America Sales + EU Sales",
       y = "Global Sales in Million Units",
       title = " NA Sales + EU Sales ~ Global Sales in Million Units") +  
  theme(axis.text.x =element_text(size = 10)) +
  geom_smooth()


###### *** INSIGHTS SO FAR BASED ON CORRELATION COEFF and PLOTS *** ####
# Strong POSITIVE CORRELATION BETWEEN 
#  - North America Sales and Global Sales, with CORR COEFF: 0.9412
# -  Europe Sales and Global Sales, with CORR COEFF: 0.9032
# -------------------------------------------------------------
# VERY STRONG + POSITIVE CORRELATION with CORR COEFF: 0.9818
# Between North America Sales + EU Sales AND Global Sales
#----------------------------------------------------------------
# VERY LOW NEGATIVE OR NO CORRELATION BETWEEN YEAR AND Global Sales, with CORR COEFF: - 0.0747

###### *** CREATE NEW MULTIPLE LINEAR REGRESSION MODEL(S) *** ####
### Model-1 Build starts ####
#Create a new model1 object and specify the lm function and the variables
#model1 y: Global Sales, x: NA_Sales + EU_Sales
model1 = lm(Global_Sales ~ NA_Sales + EU_Sales, data=games_sales_new)

summary(model1) # Print the summary statistics.
print(coefficients(model1)) # Look up coefficients from model 1
## Plot RESIDUALS for model1
hist(residuals(model1), col = 'steel blue')

### STATISTICAL OBSERVATIONS FROM MODEL-1 SUMMARY
## Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.035512   0.002420   14.67   <2e-16 ***
#  NA_Sales    1.150282   0.004377  262.83   <2e-16 ***
#  EU_Sales    1.351483   0.007068  191.22   <2e-16 ***
# Multiple R-squared:  0.9648,	Adjusted R-squared:  0.9648 
# F-statistic: 2.238e+05 on 2 and 16324 DF,  p-value: < 2.2e-16

# OBSERVATIONS
# Multiple R-square: 0.9648 *** indicates a STRONG LINEAR RELATIONSHIP between NA_Sales, EU_Sales AND Global_Sales
# 96.48% of the variations in y (Global_Sales) can be explained by the predictor / independent variables NA_Sales and EU_Sales
# p-value < 0.05, which means the model is statistically significant


#plot model-2 residuals
################# Model 1 Summary ends #########


### Model 2 Build starts ####

#Create a new model2 object and specify the lm function and the variables
#model2 y: Global Sales, x: NA_Sales + EU_Sales + Year
model2 = lm(Global_Sales ~ NA_Sales + EU_Sales + Year, data=games_sales_new)
summary(model2) # Print the summary statistics.
print(coefficients(model2)) # Look up coefficients from model 2
#plot residuals to see how good the model1 fit is

hist(residuals(model2), col = 'steel blue')

################# Model 2 Summary ends #########


############ Make Prediction using the model1 coefficients #########

#Define the coefficients from the model1 output
intercept <- coef(summary(model1))["(Intercept)", "Estimate"]
NA_sales_coef <- coef(summary(model1))["NA_Sales", "Estimate"]
EU_sales_coef <- coef(summary(model1))["EU_Sales", "Estimate"]


print(intercept)
print(NA_sales_coef)
print(EU_sales_coef)

#Rename the column Name to Game_Title to make more sense of that column 
names(games_sales_new)[names(games_sales_new)=="Name"] <- "Game_Title"

tail(games_sales_new)

#Use the model coefficients to predict the value for Global Sales for all Games
predicted_global_sales <- intercept + NA_sales_coef * games_sales_new$NA_Sales + EU_sales_coef * games_sales_new$EU_Sales

#Round off predicted global sales to 4 decimal places
predicted_global_sales = round(predicted_global_sales,4)
head(predicted_global_sales)
tail(predicted_global_sales)

### Add PREDICTED GLOBAL SALES as a NEW COLUMN in the data set to see PREDICTED GLOBAL SALES FOR EACH GAME
games_sales_new["Predicted_Global_Sales"] <- predicted_global_sales

dim(games_sales_new)
head(games_sales_new)
tail(games_sales_new)


#### PREDICTED GLOBAL SALES for all GAMES based on Model 1 are as below ########

##### VIEW PREDICTED GLOBAL SALES FOR ALL THE GAMES
view(games_sales_new)

str(games_sales_new)
####### END OF PREDICTION OF GLOBAL SALES ########

#### End of PROGRAM ***
      

