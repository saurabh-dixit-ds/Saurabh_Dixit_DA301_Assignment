# LSE Data Analytics Online Career Accelerator
# DA301: Advanced Analytics for Organisational Impact

# Course 3 Final Assignment - Week 4 - Lego DataSet Analysis R script
# Title : Explore the data to answer below Business Questions
#                    1. Determine the customer group that will most likely leave a review on the products 
#                    2. Determine the most popular, expensive product purchased by a particular group of customers.
# Author : Saurabh Dixit
# Date : 30-June-2022

###############################################################################################


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

# choose lego.csv file  
df_lego <- read.csv (file.choose (), header = T)


# sense-check data set
as_tibble(df_lego)
#Dimensions of the dataset
dim(df_lego)

#View the dataset in a tabular format
View(df_lego)

#Look up the overall Structure of the dataset
str(df_lego)

##############################################################################

# CLEAN THE DATA

# sum of missing values 
sum(is.na (df_lego))

# delete all the records with missing values
df_lego <-na.omit(df_lego)
head(df_lego)

#########################

########################## Business Question 1 : Determine the customer group that will most likely leave a review on the products 


#Lego sets purchased in the age group upto 25 years old
df_lego_age_lt_25 = df_lego[df_lego['ages'] <= 25,]
head(df_lego_age_25)
#Count distinct age groups
count(distinct(df_lego_age_lt_25["ages"])) 


df_rev_age_group <- df_lego_age_lt_25 %>% group_by(ages) %>% summarise(Number_of_Reviews = sum(num_reviews))
print(df_rev_age_group)
# Plot showing Total Number of Reviews by ages up to 25 years old
ggplot(df_rev_age_group, aes(x = ages, y = Number_of_Reviews)) +  
  geom_histogram(stat = 'identity') +  
  labs(x = "Age Group (<=25) that the Lego Caters To",
       y = "Total Number of Reviews",
       title = " Total Number of Reviews ~ Age Groups that Lego caters To (max age: 25 years) ") +  
  theme(axis.text.x = element_text(size = 12)) +
  scale_x_continuous(limits = c(0,29), breaks = seq(0,25,1)) +
  scale_y_continuous(limits = c(0,60000), breaks = seq(0,60000,5000))

# View the Lego set details for age group 8 years, 
# As this age group targeted Lego sets saw max no. of total reviews
df_lego_age_lt_25 %>% filter(ages == 8) %>% count()



# ANS : 
# The customer group that will most likely leave a review on the products is in the Age Group: ** Age: 8 **
#Additional facts, this specific customer group 
#     - Total 420 records of reviews for Age group 8
#     - Has bought Lego sets with wide range of Number of lego pieces
#     - Has bought Lego sets from many different countries
#     - Has mostly given play star rating of 4.0 or above 
#     - Has review difficulty 0 or 1 (mostly 1)

######################### End of Answer to Business Question 1 #########################

################### Business Question 2 : Determine the most popular, expensive product purchased by a particular group of customers ##
### What is the most expensive Lego set purchased by customers who are at least 25 years old (>25 years)?

#Create a data set for the ages greater than or equal to 25
df_lego_ages_gt_25 = df_lego[df_lego["ages"] >= 25,]
#Maximum list price (most expensive lego set)
max_list_price = max(df_lego_ages_gt_25["list_price"])
print(max_list_price)
# Maximum number of review in the lego data set
max_reviews = max(df_lego_ages_gt_25['num_reviews'])
max_reviews

df_lego_ages_gt_25 %>% filter(list_price == max_list_price & num_reviews == max_reviews)

df_lego_ages_gt_25 %>% filter(list_price == max_list_price)

df_lego_ages_gt_25 %>% filter(num_reviews == max_reviews)

df_max_rev_gt_25 <- df_lego_ages_gt_25 %>% group_by(ages) %>% summarise(Number_of_Reviews = sum(num_reviews))
print(df_max_rev_gt_25)
# Plot showing Total Number of Reviews by ages greater than or equal to 25 years old
ggplot(df_max_rev_gt_25, aes(x = ages, y = Number_of_Reviews)) +  
  geom_histogram(stat = 'identity') +  
  labs(x = "Age Group (>=25) that the Lego Caters To",
       y = "Total Number of Reviews",
       title = " Total Number of Reviews ~ Age Groups that Lego caters To (min age: 25 years) ") +  
  theme(axis.text.x = element_text(size = 12)) 

df_expensive_gt_25 <- df_lego_ages_gt_25 %>% group_by(ages) %>% summarise(Max_Price_of_Product = max(list_price))
print(df_expensive_gt_25)
# Plot showing Total Number of Reviews by ages greater than or equal to 25 years old
ggplot(df_expensive_gt_25, aes(x = ages, y = Max_Price_of_Product)) +  
  geom_histogram(stat = 'identity') +  
  labs(x = "Age Group (>=25) that the Lego Caters To",
       y = "Max Price of the Lego product (in US dollars)",
       title = " Maximum Price of the Lego product (in US dollars) ~ Age Groups that Lego caters To (min age: 25 years) ") +  
  theme(axis.text.x = element_text(size = 12)) +
  scale_x_continuous(limits = c(25,30), breaks = seq(25,30,1)) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,25))


# ANS : The most expensive Lego set purchased by customers who are at least 25 years old (>=25 years)
#       has the List price : 259.87 US$ 
#       Further details are as below:
#     ages list_price num_reviews piece_count play_star_rating review_difficulty country
#    ----- ---------- ----------- ----------- ---------------- ----------------- -------
#      29     259.87           6        1413              4.3                 0      16

######################### End of Answer to Business Question 2 #########################


##### End of Program ####

