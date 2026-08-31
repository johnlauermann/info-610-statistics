# Lab 2: descriptive statistics

# in this lab, we'll use these libraries:
library(dplyr)      # for data management
library(readr)      # for data cleaning
library(ggplot2)    # for designing data visualizations
library(treemapify) # for tree maps


# set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

# load your data from NYC Open Data
## Go to the City Payroll records, query for a fiscal year, and download. 
## Save it to your working directory. https://data.cityofnewyork.us/City-Government/Citywide-Payroll-Data-Fiscal-Year-/k397-673e/about_data 

# load raw data
dataraw <- read.csv("payroll.csv")

# data cleaning
data <- dataraw %>%
  mutate(Regular.Gross.Paid = parse_number(Regular.Gross.Paid)) 



# Part 1: descriptive statistics ------------------------------------------

## You can use base r functions to find descriptives
mean(data$Regular.Gross.Paid)
median(data$Regular.Gross.Paid)
sd(data$Regular.Gross.Paid)
var(data$Regular.Gross.Paid)
max(data$Regular.Gross.Paid)
min(data$Regular.Gross.Paid)

## now query by full time and part time status
full_time <- data %>%
  filter(Regular.Hours >= 1820) %>%
  filter(Leave.Status.as.of.June.30 == "ACTIVE")

part_time <- data %>%
  filter(Regular.Hours < 1820) %>%
  filter(Leave.Status.as.of.June.30 == "ACTIVE")

mean(full_time$Regular.Gross.Paid)
median(full_time$Regular.Gross.Paid)
sd(full_time$Regular.Gross.Paid)
var(full_time$Regular.Gross.Paid)

mean(part_time$Regular.Gross.Paid)
median(part_time$Regular.Gross.Paid)
sd(part_time$Regular.Gross.Paid)
var(part_time$Regular.Gross.Paid)



# Part 2: query subsets ---------------------------------------------------

# list unique values in a column
unique(data$Agency.Name)

# one option is to use the [] filter
mean(data$Regular.Gross.Paid[data$Agency.Name == "POLICE DEPARTMENT"])
median(data$Regular.Gross.Paid[data$Agency.Name == "POLICE DEPARTMENT"])
sd(data$Regular.Gross.Paid[data$Agency.Name == "POLICE DEPARTMENT"])
var(data$Regular.Gross.Paid[data$Agency.Name == "POLICE DEPARTMENT"])

# another option is to use subset()
police <- subset(data, data$Agency.Name == "POLICE DEPARTMENT")
mean(police$Regular.Gross.Paid)
median(police$Regular.Gross.Paid)
sd(police$Regular.Gross.Paid)
var(police$Regular.Gross.Paid)

# or you could use filter() and summarize() from the dplyr library
police <- data %>%
  filter(Agency.Name == "POLICE DEPARTMENT") %>%
  summarize(mean = mean(Regular.Gross.Paid),
            median = median(Regular.Gross.Paid),
            sd = sd(Regular.Gross.Paid),
            var = var(Regular.Gross.Paid))

# and try some kind of visualization
## using hist() from base R
hist(police$Regular.Gross.Paid)

## or using ggplot tools
ggplot(data = police, aes(x = Regular.Gross.Paid)) +
  geom_histogram() + 
  theme_gray()



# Part 3: compare means across groups -------------------------------------

# One option is to use the aggregate() function
mean_by_agency <- aggregate(x = data$Regular.Gross.Paid,
                           by = list(data$Agency.Name),
                           FUN = mean)
print(mean_by_agency)

# another option is to use group_by() from dplyr
summary_by_agency <- data %>%
  
  ## this will group all records by categorical variable
  group_by(Agency.Name) %>%
  
  ## and then use summarize() to calculate descriptive stats by group
  summarize(total_pay = sum(Regular.Gross.Paid),
            mean_pay = mean(Regular.Gross.Paid),
            median_pay = median(Regular.Gross.Paid),
            max_pay = max(Regular.Gross.Paid),
            min_pay = min(Regular.Gross.Paid)
            )
print(summary_by_agency)


# and include some data visualization
## let's use ggplot. 
## it runs on a layering structure called the 'grammar of graphics' (hence, gg)

# define plot space
ggplot()

# add data
ggplot(data = summary_by_agency)

# map variables to plot space
ggplot(data = summary_by_agency, 
       aes(area = total_pay, fill = median_pay, label = Agency.Name))

# add the chart type
ggplot(data = summary_by_agency, 
       aes(area = total_pay, fill = median_pay, label = Agency.Name)) +  ## + is layering operator. it works kind of like %>% in dplyr
  geom_treemap() 


# define some colors
ggplot(data = summary_by_agency, 
       aes(area = total_pay, fill = median_pay, label = Agency.Name)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", grow = TRUE) 

# add labels
ggplot(data = summary_by_agency, 
       aes(area = total_pay, fill = median_pay, label = Agency.Name)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", grow = TRUE) + 
  labs(title = "Public Salaries in NYC", 
       subtitle = "Median Salary by Agency") 
