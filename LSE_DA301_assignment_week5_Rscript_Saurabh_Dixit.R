# LSE Data Analytics Online Career Accelerator
# DA301: Advanced Analytics for Organisational Impact

# Course 3 Final Assignment - Week 5 - Video Games Analysis R script
# Title : Perform exploratory data analysis (EDA) of Games Sales Data set to derive Insights
# Author : Saurabh Dixit
# Date : 30-June-2022

###############################################################################################
# 

##### ****** START of META DATA FOR GAMES SALES DATASET) ****** ########
#COLNAME  DESCRIPTION
--------- ----------------------------------------------------------
#Rank :   World ranking of the game
#Name :   Name of the video game
#Platform : Video game console on which the game was launched
#Year : Year of launch 
#Genre :  Genre of the video game
#Publisher : Company that published the game
#NA_Sales : Number of games sold in North America (in millions of units)
#EU_Sales :  Number of games sold Europe (in millions of units)
# Global_Sales : Total sales in the world (which is a sum of EU_Sales, NA_Sales and online sales) (in millions of units)

####### *** END OF META DATA *** ####

####### *** PLAN OF ACTION ***  #######

# - Load and clean the data set.
# - Transform data set using string manipulation.
# - Explore, manipulate and visualize data to gain insights.
# - Interpret and provide insights.

###### *** END OF PLAN OF ACTION *** ####
###############################################################################

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
sum(is.na (games_sales))

# delete all the records with missing values
games_sales_new <-na.omit(games_sales)
head(games_sales_new)

# Still some N/A values in Year, filter those out and store only the records with NO "N/A"
games_sales_new <- games_sales_new[games_sales_new["Year"]!="N/A",]

dim(games_sales_new)
sum(is.na (games_sales_new))

##############################################################################

###### *** STRING MANIPULATION *** ####
str_new <- " demonstrate string manipulation"
str_to_upper(str_new)
str_name <- games_sales_new$Name[1]
str_platform <- games_sales$Platform[1]
str_year <- games_sales$Year[1]
str_genre <- games_sales$Genre[1]
str_publisher <- games_sales$Publisher[1]
str_nasales <- games_sales$NA_Sales[1]
str_eusales <- games_sales$EU_Sales[1]

# Create some meaningful statements using String manipulation
statement1 <- str_c ("The Game ",str_name," was launched in the Year ", str_year, " on ",str_platform, " platform by " , str_publisher)
print(statement1)
statement2 <- str_c ("The Game ",str_name, " sold ",str_nasales, " Million units in North America")
statement3 <- str_c ("The Game ",str_name, " sold ",str_eusales, " Million units in Europe")
print(statement2)
print(statement3)
str_c("Length of Statement 1 is: ", toString(str_length(statement1)))
str_c("Length of Statement 2 is: ", toString(str_length(statement2)))
str_c("Length of Statement 3 is: ", toString(str_length(statement3)))

###### *** EXPLORE, MANIPULATE AND VISUALISE DATA TO GAIN INSIGHTS *** ####
#Check column names
colnames(games_sales_new)

#Check total number of records
dim(games_sales_new)

#Rename the column Name to Game_Title to make more sense of that column 
names(games_sales_new)[names(games_sales_new)=="Name"] <- "Game_Title"
colnames(games_sales_new)

# Count number of Distinct Publishers in the Games dataset
count(distinct(games_sales_new["Publisher"]))

# Count number of Distinct Game Titles in the Games dataset
count(distinct(games_sales_new["Game_Title"]))

#Summarise the Data
summary(games_sales_new)

#Create an Exploratory Report in HTML using DataExplorer
DataExplorer::create_report(games_sales_new)


###### *** INSIGHTS SO FAR BASED ON DataExplorer Report *** ####
# TOTAL NUMBER OF UNIQUE PUBLISHERS : 579
# TOTAL NUMBER OF UNIQUE GAME TITLES : 11493
# ABOVE NUMBERS SHOW THAT SOME PUBLISHERS HAVE PRODUCED AND RELEASED MORE THAN ONE GAME FROM THEIR STUDIOS
# MOST FREQUENTLY SEEN PLATFORM IS DS WHICH IS A HANDHELD GAME CONSOLE PRODUCED BY NINTENDO. 
# FOLLOWED BY DS ARE THE PLAYSTATION 2 AND PLAYSTATION 3 PLATFORMS IN TERMS OF HIGH FREQUENCY
# MOST FREQUENTLY OCCURRING GENRE IS ACTION, FOLLOWED BY SPORTS
# MOST NUMBER OF GAME LAUNCHES HAPPENED IN THE YEAR 2009 FOLLOWED BY 2008 AND 2010

###### *** MANIPULATE AND VISUALIZE DATA *** ####
# Change data type of Year column into Numeric and store it in an object
date_test <- as.integer(games_sales_new$Year)
date_test
as_tibble(date_test)
class(date_test)

#Change data type of Year col from Char to Integer by initializing it with the newly created object
games_sales_new$Year = date_test

#Sense check the Year column and the dataframe
as_tibble(games_sales_new)

# Check for Normal Distribution using Normality test
# Specify the qqnorm function; draw a qqplot using the total_seconds data:
qqnorm(games_sales_new$Global_Sales, col="blue", xlab="z Value", ylab="Time")

# Specify the qqline function; add a reference line to the qqplot:
qqline(games_sales_new$Global_Sales, col="red", lwd=2) 

# Randomize 5000 rows from the dataframe to run Shapiro-Wilk test
rand_games_sample <- games_sales_new[sample(nrow(games_sales_new), size=5000),]
as_tibble(rand_games_sample)

#  Run a Shapiro-Wilk test for Global_Sales
shapiro.test (rand_games_sample$Global_Sales)

#  Run a Shapiro-Wilk test for NA_Sales
shapiro.test (rand_games_sample$NA_Sales)

#  Run a Shapiro-Wilk test for EU_Sales
shapiro.test (rand_games_sample$EU_Sales)

# Find the Total Global Sales for each Publisher
df_sales_by_pub = games_sales_new %>% group_by(Publisher) %>% 
  summarise(Total_Sales = sum(Global_Sales))
 
df_sales_by_pub

df_top10 = arrange(df_sales_by_pub, desc(Total_Sales)) %>% slice(1:10)

ggplot(df_top10 ,aes(x = Publisher, y = Total_Sales)) +  
  geom_bar(stat = "identity", fill="steel blue") +  
  labs(x = "Publisher Name",
        y = "Global Sales in Million Units",
        title = "Top 10 Publishers by Global Sales in Million Units") +  # [3] Specify titles.
        theme(axis.text.x =element_text(size = 10))+
        coord_flip()


# Show the Top 10 Platforms in terms of Total Global Sales
df_plat_sales = games_sales_new %>% group_by(Platform) %>% 
  summarise(Total_Sales = sum(Global_Sales))

df_top10_plat = arrange(df_plat_sales, desc(Total_Sales)) %>% slice(1:10)

df_top10_plat

ggplot(df_top10_plat, aes(x = Platform, y = Total_Sales)) +  
  geom_bar(stat = "identity", fill="steel blue") +  
  labs(x = "Platform",
       y = "Global Sales in Million Units",
       title = "Top 10 Platforms by Global Sales in Million Units") +  # [3] Specify titles.
  theme(axis.text.x =element_text(size = 10))+
  coord_flip()

# Question : How many Distinct Gaming Platforms do we have in the data set
distinct(games_sales_new["Platform"]) %>% count()


# Scatter plot showing North America Sales by each Gaming Platform
ggplot(games_sales_new, aes(x = Platform, y = NA_Sales)) +  
  geom_point() +  
  labs(x = "Platform",
       y = "North America Sales in Million Units",
       title = " North America Sales in Million Units ~ Platform") +  
  theme(axis.text.x =element_text(size = 10))


# Scatter plot showing Europe Sales by each Gaming Platform
ggplot(games_sales_new, aes(x = Platform, y = EU_Sales)) +  
  geom_point() +  
  labs(x = "Platform",
       y = "Europe Sales in Million Units",
       title = " Europe Sales in Million Units ~ Platform") +  
  theme(axis.text.x =element_text(size = 10))

# Find out how many games have been launched by each of the Publishers
df_game_by_publisher = games_sales_new %>% group_by(Publisher) %>% count(Publisher)
names(df_game_by_publisher) <- c("Publisher", "Number_Of_Games")   

as_tibble(df_game_by_publisher)

test_df = df_game_by_publisher[order(-df_game_by_publisher$Number_Of_Games),] 
dim(test_df)

###### *** ADDITIONAL INSIGHTS** ####
###############################################################################
# Total 577 Publishers
# Top 5 Publishers in terms of Global Sales are:
#  1. Nintendo
#  2. Electronic Arts
#  3. Activision
#  4. Sony Computer Entertainment
#  5. Ubisoft
#------------------------------------------------
# Top 5 Platforms in terms of Global Sales are:
#  1. PS2            1233.
#  2. X360            970.
#  3. PS3             949.
#  4. Wii             910.
#  5. DS              819.
# ------------------------------------------------
# Distinct Gaming Platforms : 31
# ------------------------------------------------

###### *** END OF PROGRAM - WEEK 5** ####


