# Lab 8: logistic regression

library(dplyr)    # data management
library(haven)    # for reading SPSS data
library(survey)   # for weigting survey data


# load data
## for this, we'll use Pew ATP data: https://www.pewresearch.org/dataset/american-trends-panel-wave-161/

# Set your working directory
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

# load raw data
data_raw <- read_spss("C:/Users/johnl/My Drive (jlauerma@pratt.edu)/Teaching/INFO 610 Intro to Statistics/Lessons/11_logistic_regression/W161_Feb25/ATP W161.sav")

# and weight the survey
data_weighted <- svydesign(ids = ~0,
                           data = data_raw,
                           weights = ~WEIGHT_W161)



# Q1: Select and visualize variables --------------------------------------

# define variables of interest
variables <- c("SATIS_W161", "F_CREGION", "F_USR_SELFID", "F_AGECAT", "F_GENDER",
               "F_EDUCCAT", "F_HISP", "F_RACECMB", "F_MARITAL", "F_RELIGCAT1", 
               "F_PARTYSUM_FINAL", "F_INTFREQ", "WEIGHT_W161")

# define null codes
null_codes <- c("9", "99")

# filter and remove 'don't know/null/refused'
data <- data_raw %>%
  select(all_of(variables))

# replace null codes with null
data <- data %>%
  mutate(across(
    all_of(variables),
    ~ifelse(as.character(.x) %in% null_codes, NA, .x))) 

# remove null values and recode to 1/0 structure
data <- data %>%
  filter(!is.na(SATIS_W161)) %>%
  mutate(SATIS_W161 = ifelse(SATIS_W161 == 1, 1, 0))




# Q1: Define and justify your research question ---------------------------
## In my case, what predicts whether a person is satisifed with direction of the country? 

# testing correlations, using Spearman's rho b/c Y is categorical
cor.test(data$SATIS_W161, data$F_AGECAT, method = "spearman")
cor.test(data$SATIS_W161, data$F_GENDER, method = "spearman")
cor.test(data$SATIS_W161, data$F_EDUCCAT, method = "spearman")
cor.test(data$SATIS_W161, data$F_MARITAL, method = "spearman")
cor.test(data$SATIS_W161, data$F_PARTYSUM_FINAL, method = "spearman")
cor.test(data$SATIS_W161, data$F_INTFREQ, method = "spearman")




# Q2: define and interpret logistic models with only one variable  --------

## define my predictors
predictors <- c("F_AGECAT", "F_GENDER", "F_EDUCCAT", "F_MARITAL", "F_PARTYSUM_FINAL", "F_INTFREQ")


## write a loop to test each predictor
for (variable in predictors){
  
  ## build the formula
  formula <- as.formula(paste("SATIS_W161 ~", variable))
  
  ## fit the model
  model <- glm(formula, data = data, family = "binomial")
  
  ## print results
  ### model summary
  print(summary(model))
  
  ### odds ratios
  print(exp(coef(model)))
  
  ### confidence intervals
  print(exp(confint.default(model)))
  
  # analysis of deviance
  print(anova(model, test = "Chisq"))
  
}




# Q3: define and interpret a logistic model with all variables ------------

## define the model
log_model <- glm(SATIS_W161 ~ F_AGECAT + F_GENDER + F_EDUCCAT + F_MARITAL + 
                   F_PARTYSUM_FINAL + F_INTFREQ, 
                 data = data, family = "binomial")

## print the results
summary(log_model)

## convert coefficients and confidence intervals to odds-ratio terms
exp(coefficients(log_model))
exp(confint.default(log_model))


## Interpret analysis of deviance table
anova(log_model, "Chisquare")




# Bonus: weight it properly -----------------------------------------------
## Survey data should be weighted. You don't have to for now. But if you wanted to, here's how. 

# make sure Y is in a 1/0 structure
data_raw <- data_raw %>%
  mutate(SATIS_W161 = ifelse(SATIS_W161 == 1, 1, 0))

# weight the data
data_weighted <- svydesign(ids = ~0,
                           data = data_raw,
                           weights = ~WEIGHT_W161)

# define the fomula
logistic_formula <- as.formula(paste("SATIS_W161 ~", paste(predictors, collapse = " + ")))
logistic_formula 

# run the model
design <- svydesign(ids = ~1, weights = ~WEIGHT, data = data)
logistic_model_weighted <- svyglm(
  formula = logistic_formula,
  design = data_weighted,
  family = quasibinomial())

# see results
summary(logistic_model_weighted)
exp(coefficients(logistic_model_weighted))
exp(confint.default(logistic_model_weighted))
