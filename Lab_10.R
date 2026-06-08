
# Lab 10: k-means cluster analysis ----------------------------------------

library(tidyverse)  # dependency for tidycensus
library(tidycensus) # census api
library(dplyr)      # data management
library(cluster)    # cluster analysis
library(factoextra) # visualizations


# load data
## sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

## define a vector with variables
variables <- c(
  PovertyRate  = "S1701_C03_046E",
  Bachpct = "S1501_C02_012E",
  Postgradpct = "S1501_C02_013E",
  UnemploymentRate = "S2301_C04_001E",
  MedConRent = "B25058_001E",
  MedHomeValue = "B25077_001E", 
  Renterspct = "DP04_0046PE" 
  )

## pull from API
data <- get_acs(geography = "county",
                variables = variables,
                output = "wide",
                year = 2023)


##  remove unnecessary variables & missing values
data <- data %>% select(c(Bachpct, MedConRent, MedHomeValue, Postgradpct, 
                          PovertyRate, Renterspct, UnemploymentRate))
data <- na.omit(data)




# Question 1: what variables did you choose and why? ----------------------
# visualizing data
## scatterplot pairs
pairs(data, 
      na.action = na.omit, 
      upper.panel = NULL)

# heat maps
distance <- get_dist(na.omit(data))
fviz_dist(distance, 
          gradient = list(low = "blue", mid = "white", high = "red"),
          show_labels = FALSE)




# Question 2: find value of k --------------------------------------------
# various kinds of elbow plots to assess optimal value of k
fviz_nbclust(data, kmeans, method = "wss")




# Question 3: interpret cluster anlaysis ----------------------------------
# define cluster
k4 <- kmeans(data, centers = 4, nstart = 25)

# see results
k4

# see the individual component options
names(k4)

## call individual components
k4$size
k4$centers
k4$betweenss
k4$withinss
k4$totss




# Question 4: visualize the clusters --------------------------------------

fviz_cluster(k4, data = data, geom = "point", main = "k = 4", ggtheme = theme_minimal())
