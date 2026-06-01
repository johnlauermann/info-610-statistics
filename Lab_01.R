

# Lab 1: Data exploration -------------------------------------------------

## In this lab, we'll explore NYC Open Data to learn the basics of data management in R. 


# Set your working directory
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

## and to double check...
getwd()


# Pull data from the NYC Street Tree Census (https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh/about_data)
## Download as a CSV to your working directory, then:
data <- read.csv("alltrees.csv")



# Part 1: Explore dimensions of the data ----------------------------------

## view data frame
View(data)

## see only the first rows
head(data)
head(data, 10) # this specifies how many first rows to view

## count dimensions of the data
nrow(data)
ncol(data)
dim(data)  ## or this gives both

## list the variables
ls(data)

## identify the data types
class(data$spc_common) ## use $ to reference a variable within the data frame
sapply(data, class) ## for all variables, using sapply() to iterate through each column

## find unique values in a variable
unique(data$spc_common)

## find null values using is.na()
sum(is.na(data$spc_common))  ## for a single variable
colSums(is.na(data))  ## for the entire data frame



# Part 2: Summarize data for a single species -----------------------------

## for this, we'll import the dplyr library, which has more specialized data management tools
install.packages("dplyr") ## if you need to install it...
library(dplyr)

## query the data from the web, download as a csv, then:
ash <- data %>%               ## %>% is a 'pipe operator'. it means do this then continue to next command
  filter(spc_common == "ash")   ## filter() returns rows that match your query

## create frequency and proportional tables
frequency <- table(ash$borough)
print(frequency)

proportion <- prop.table(frequency)
print(proportion)

## create a pivot table
pivot <- ash %>%  ## The %>% is the 'pipe' operator. It means do this, then keep going.
  group_by(nta_name) %>%  ## group_by() groups all rows that match the query
  summarize(count = n())  ## summarize() can be any descriptive statistic on the group. I want the total count. 
print(pivot)

## basic data visualizations
### histograms, which show distribution of a single variable
hist(ash$tree_dbh)

### descriptive statistics
mean(ash$tree_dbh)
median(ash$tree_dbh)
quantile(ash$tree_dbh)


### bar charts, which summarize counts by attribute
health_summary <- ash %>%
  group_by(health) %>%
  summarize(mean_dbh = mean(tree_dbh))
barplot(health_summary$mean_dbh, names.arg = health_summary$health)



# Part 3: Summarize your chosen borough -----------------------------------

## common species
table(data$spc_common[data$borough == "Bronx"]) # [] will filter based on a query

## or with some dplyr syntax
bronx <- data %>%
  filter(borough == "Bronx") 

pivot <- bronx %>%
  group_by(spc_common) %>%
  summarize(count = n()) %>%
  slice_max(n = 5, order_by = count)


# You're on your own for visualization, but here's one idea: a histogram. 
## basic setup
hist(x = bronx$tree_dbh[bronx$spc_common == "honeylocust"]) 

## change the bin size
hist(x = bronx$tree_dbh[bronx$spc_common == "honeylocust"], 
     breaks = 100) 

## add some labels
hist(x = bronx$tree_dbh[bronx$spc_common == "honeylocust"], 
     breaks = 100, 
     main = "Honey Locust Trees in the Bronx", 
     xlab = "Trunk Diameter (in)") 

## and play with colors
hist(x = bronx$tree_dbh[bronx$spc_common == "honeylocust"], 
     breaks = 100, 
     col = "lightgreen",
     border = "darkgrey",
     main = "Honey Locust Trees in the Bronx", 
     xlab = "Trunk Diameter (in)") 

## see also Winston Chang's (2026) R Graphics Cookbook: https://r-graphics.org/