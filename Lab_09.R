
# Lab 9: t-tests and ANOVA ------------------------------------------------

library(dplyr)
library(lsr)
library(stringr)
library(tidyverse)
library(tidycensus)


# load data from Census API
## sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your API key")

## find a list of variables here
## it includes code, description, sampling population, and finest geographic scale of availability
variablelist <- load_variables(2024, "acs5", cache = TRUE)

#define vectors with variables
variables <- c(
  Population = "B01003_001E",
  PovertyRate  = "S1701_C03_046E",
  Bachpct = "S1501_C02_012E",
  UnemploymentRate = "S2301_C04_001E")

NYcounties <- c("Kings", "Queens", "New York", "Bronx", 
                "Richmond", "Westchester", 
              "Suffolk", "Nassau", "Rockland", "Putnam")
NJcounties <- c("Bergen", "Hudson", 
              "Passaic", "Middlesex", "Monmouth", "Ocean", 
              "Somerset", "Essex", 
              "Union", "Morris", "Sussex", "Hunterdon")
  
## pull data tables by state, then connect them
NYdata <- get_acs(geography = "tract",
                state = "New York",
                county = NYcounties,
                variables = variables,
                output = "wide",
                year = 2024)
NJdata <- get_acs(geography = "tract",
                  state = "New Jersey",
                  county = NJcounties,
                  variables = variables,
                  output = "wide",
                  year = 2024)

## append one table to the other
data <- rbind(NYdata, NJdata)  

##data cleaning to remove unnecessary variables and omit null values
ls(data)
str(data)
data <- data %>% select(-ends_with("M"))  #remove unnecessary columns
mean(data$PovertyRate) # this won't work because of the NA value in the first tract of Bronx County (aka Rikers Island)
data <- na.omit(data) # remove NAs
mean(data$PovertyRate)  # now it works!




# Question 1) run t-tests to compare two groups ---------------------------
## for this test, you need two groups. For example, you could use the NY and NJ data frames as separate groups. 
## I'm going to create a new grouping variable based on whether a tract is suburban or inner city. 

## create a suburb/central city column
data <- data %>%
  ## mutate creates a new column
  mutate(
    # intially set all to suburb
    PlaceType = "Suburb", 
    # then change if the name contains certain words
    PlaceType = if_else(
      # | will search for this pattern OR that pattern
      str_detect(NAME, "Bronx County|Kings County|New York County|Queens County|Richmond County"), 
      "City", 
      PlaceType)
    )

# and filter out two groups
city <- data %>% 
  filter(PlaceType == "City")
suburb <- data %>% 
  filter(PlaceType == "Suburb")


## check whether the two groups satisfy assumptions for t-tests
## see here for a summary of assumptions: https://www.statology.org/t-test-assumptions/

### are they normally distributed?
hist(city$PovertyRate, breaks = 100, main = "New York Metro Poverty, 2024", 
     ylab = "Census Tracts", xlab = "Poverty Rate")
hist(suburb$PovertyRate, breaks = 100, main = "Poverty in the New York Metro, 2024", 
     ylab = "Census Tracts", xlab = "Poverty Rate", col = "red")
mean(city$PovertyRate)
median(city$PovertyRate)
mean(suburb$PovertyRate)
median(suburb$PovertyRate)

### do they have similar variance?
var(city$PovertyRate)
var(suburb$PovertyRate)
var(city$UnemploymentRate)
var(suburb$UnemploymentRate)
var(city$Bachpct)
var(suburb$Bachpct)

## run the t-tests to compare variable values in each group
t.test(x = city$PovertyRate,       # group 1 sample mean
       y = suburb$PovertyRate,     # group 2 sample mean
       alternative = "two.sided",  # one or two sided test
       mu = 0,                     # hypothesized difference between the means
       paired = FALSE, 
       var.equal = FALSE,          # since variances are not equal (per above), use this option
       conf.level = .95)           # desired level of confidence

t.test(city$UnemploymentRate, suburb$UnemploymentRate, 
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = .95)

t.test(city$Bachpct, suburb$Bachpct, 
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = .95)

## or alternatively, you can use the [] query from base R to work directly with the original dataframe
t.test(data$PovertyRate[data$PlaceType == "City"], 
       data$PovertyRate[data$PlaceType == "Suburb"], 
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = .95)

## assess the effect sizes using cohens D (you'll need the lsr library)
### for more on Cohen's D, see https://statisticsbyjim.com/basics/cohens-d/ 
cohensD(city$PovertyRate, suburb$PovertyRate)
cohensD(city$UnemploymentRate, suburb$UnemploymentRate)
cohensD(city$Bachpct, suburb$Bachpct)



# Question 2) ANOVA to compare across three or more groups ----------------
#again, you'll need a grouping variable. 
#In my case, I'm going to subset the five boroughs of NYC, and use each borough as a group. 

## create a county column using the NAME string
city <- city %>%
  mutate(
    # case_when is like an if_else statement, but for multiple possibilities
    Borough = case_when(
      str_detect(NAME, "Bronx County") ~ "Bronx",
      str_detect(NAME, "Kings County") ~ "Brooklyn",
      str_detect(NAME, "New York County") ~ "Manhattan",
      str_detect(NAME, "Queens County") ~ "Queens",
      str_detect(NAME, "Richmond County") ~ "Staten Island",
    )
  )

#configure the ANOVA, with formula main variable ~ group variable
one.way.poverty <- aov(PovertyRate ~ Borough, data = city)
summary(one.way.poverty)
one.way.unemployement <- aov(UnemploymentRate ~ Borough, data = city)
summary(one.way.unemployement)
one.way.bach <- aov(Bachpct ~ Borough, data = city)
summary(one.way.bach)

#the ANOVA will tell you whether there is a difference across all of the groups,
#without explaining where the differences occur. 
#for that, use a post-hoc test like Tukey's HSD
#see more at https://rpubs.com/aaronsc32/post-hoc-analysis-tukey 

## post-hoc tests to assess difference between group pairs
posthoctest.poverty <- TukeyHSD(one.way.poverty)
posthoctest.poverty
posthoctest.unemployment <- TukeyHSD(one.way.unemployement)
posthoctest.unemployment
posthoctest.bach <- TukeyHSD(one.way.bach)
posthoctest.bach

