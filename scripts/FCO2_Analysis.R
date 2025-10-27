library(tidyverse)
library(randomForest)
library(randomForest)
library(dplyr)
library(nlme)
library(corrplot)

# Linear Model Building

# Key Question, Does a more frequent turning schedule influence emissions of each GHG, average magnitude - NOT cumulative?

# Approach 1, lets look and see if we can find a difference between pile in emissions accounting for random effects of chamber and other vars which 
# impact emissions. 

# To start, we have many variables. I want to build a model with a subset of them. 
#Step 1 is to identify which variables predict FCO2 best. 

# r^2 cleaned dataset of C02
FCO2_Cleaned<- read_csv("windrow-fluxes/data_clean/CO2_R_2_Filtered.csv")

# Normalizing/scaling variables - to more easily see the impact of each variable

df_scaled <- FCO2_Cleaned %>%
  mutate(un_scaled_DOY = DOY) %>%           # Step 1: store original DOY for later ac structure.
  mutate(across(where(is.numeric) & !matches("DOY"), scale))  # Scale all numeric columns except DOY


# factorize categorical variables

df_categorized <- df_scaled %>%
  mutate(across(where(is.character), as.factor))

# checking if they are factorized

sapply(df_categorized, is.factor)

# removing variables, many are accounted for in other variables, or we do not think are associated with co2 emissions. 
# could include Ch4: (see what happens)


# run cor analysis and remove very correlated data: 


df_categorized <- df_categorized %>%
  select(-Date, -DATE_TIME,-R2_flag_8,-R2_flag_9,-R2_flag_95,-N2O_DRY.DEADBAND,-CH4_DRY.DEADBAND,-CO2_DRY.DEADBAND, -OBSERVATION,-FCO2_DRY
         ,-FCO2_DRY.R2,-FCO2_DRY.LIN_R2,-FCH4_DRY.LIN_dCdt,-FCH4_DRY.dCdt, -FCH4_DRY,-FCH4_DRY.LIN_R2,-FCH4_DRY.R2,-FN2O_DRY,-FN2O_DRY.dCdt,
         -FN2O_DRY.R2,-FN2O_DRY.LIN,-FN2O_DRY.LIN_dCdt,-FN2O_DRY.LIN_R2,-CO2_DRY.initial_value,-CO2_DRY.range,-CO2_DRY.mean,-CO2_DRY.DEADBAND,-FCO2_DRY.LIN_dCdt, 
         -FCO2_DRY.dCdt,-VOLUME_TOTAL,-TimeSinceStart,- DOY.initial_value, -N2O_DRY.range, -PORT, -WNS,-N2O_DRY.mean,-N2O_DRY.initial_value,
          -RNF, -Turning_Status, -ET, -H, -TKE, -bowen_ratio, -LE,-WND,-WNG,-TA.mean)


# filling in NA's with median value for that column - open to other ideas to fill in this data. 

df_categorized <- df_categorized %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

# Creating a linear model to determine which variables to use in the mixed effects model. 

full_lm <- lm(FCO2_DRY.LIN ~ ., data = df_categorized)
summary(full_lm)

# Key takeaways - Days since turning is important, more important than total turns, 
# bulk density is important, chamber elevation is important. Stevens probe measurements as well as weather station. 
# surprised u* was not an important variable. 
# odd that wind direction matters, is that because of the wind or is that correlated with something? 

# using random forest as well:
# had to change u* name to avoid issues

df_categorized <- df_categorized %>%
  rename(u_star = `u*`)

rf_model <- randomForest(FCO2_DRY.LIN ~ ., data = df_categorized, importance = TRUE, na.action = na.omit)

importance(rf_model)
varImpPlot(rf_model)

# similar results to the linear model
# Good to see the relationship between ch4 and co2 exists. Yet can we use FCh4 as a predictor var. Not sure that would make sense (DISCUSS FURTHER)
# Chamber,T (probe), Days since last turn,  swc probe, Bar(this is cool to see), chamber elevation, Bulk Density, Pile....

# Okay we have a good idea of important covars, lets start model building:

# To help with model building removing outliers ( being generous with a 2 times IQR could do more as there are likely spikes from turning)

Q1 <- quantile(df_categorized$FCO2_DRY.LIN, 0.25)
Q3 <- quantile(df_categorized$FCO2_DRY.LIN, 0.75)  
iqr_val <- IQR(df_categorized$FCO2_DRY.LIN, na.rm = TRUE)

# this is 2 IQR could change
lower_bound <- Q1 - 2* iqr_val
upper_bound <- Q3 + 2* iqr_val

# Filtering out outliers - can change in the future, but should help with model
Scaled_Cleaned_Co2_data <- df_categorized[df_categorized$FCO2_DRY.LIN >= lower_bound & df_categorized$FCO2_DRY.LIN <= upper_bound, ]

# creating the base model.


lme_base_model <- lme(
  FCO2_DRY.LIN ~ Pile + DOY + SWC_1.initial_value + TS_1.initial_value +
    TMP + BAR + BulkDensity + Chamber_Elevation + TA.initial_value + TMPA + VWCB +
    DaysSinceLastTurn + TotalTurns + HMD,
  random = ~ 1 | Chamber_Corrected,
  data = Scaled_Cleaned_Co2_data,
  na.action = na.omit
)

summary(lme_base_model)
plot(lme_base_model)
qqnorm(lme_base_model)

# based on these results there are some interesting talking points 
# If Pile is still not significant after controlling for DaysSinceLastTurn and TotalTurns, 
# It suggests that those variables explain the effect of turning better than the categorical label of pile.


# the reduced model does not include pile, but is a similiar set of vars (many of which might be correlated),
# like soil moisture and HMD ect. 

lme_base_mode_reduced <- lme(
  FCO2_DRY.LIN ~ DOY + SWC_1.initial_value + TS_1.initial_value + BAR + BulkDensity + Chamber_Elevation +
    DaysSinceLastTurn + TotalTurns + TA.initial_value + HMD + VWCB,
  random = ~ 1 | Chamber_Corrected,
  data = Scaled_Cleaned_Co2_data,
  na.action = na.omit
)

summary(lme_base_mode_reduced)
plot(lme_base_model)
qqnorm(lme_base_model)

# as we did not lose explanatory power, lets use the reduced model moving forward as it is more simple:
# for auto correlation we will use DOY - but unscaled to avoid negative values: 

Scaled_Cleaned_Co2_data %>%
  group_by(Chamber_Corrected) %>%
  arrange(DOY, .by_group = TRUE) %>%
  summarise(repeats = any(duplicated(DOY)))

# while dates don't repeat, for the scaled date there are some repeats which we have to get rid of for autocorrelation.

Scaled_Cleaned_Co2_data <- Scaled_Cleaned_Co2_data %>%
  group_by(Chamber_Corrected, un_scaled_DOY) %>%
  filter(row_number() == 1) %>%  # Keep only first occurrence
  ungroup()

Scaled_Cleaned_Co2_data <- Scaled_Cleaned_Co2_data %>%
  mutate(un_scaled_DOY = as.numeric(unlist(un_scaled_DOY)))

Scaled_Cleaned_Co2_data <- Scaled_Cleaned_Co2_data %>%
  arrange(Chamber_Corrected, un_scaled_DOY) %>%
  group_by(Chamber_Corrected) %>%
  mutate(measurement_index = row_number()) %>%
  ungroup()

# Add AR(1) autocorrelation structure based on DOY (or time variable) - by chamber not as a whole.
lme_ac <- update(
  lme_base_mode_reduced,
  correlation = corAR1(form = ~ measurement_index | Chamber_Corrected)
)

# letting variance change by pile: 
lme_var_pile <- update(lme_base_mode_reduced, weights = varIdent(form = ~1 | Pile))

# letting variance change by chamber: 
lme_var_chamber <- update(lme_base_mode_reduced, weights = varIdent(form = ~1 | Chamber_Corrected))

# Compare the models
BIC(lme_base_mode_reduced,lme_ac, lme_var_pile, lme_var_chamber)

# the BIC fo the adding AC improves alot. Out of the adding different variances, it only improves the model fit with chamber. 

# combining ac and var 

lme_ac_var <- update(
  lme_ac,
  weights = varIdent(form = ~1 | Chamber_Corrected),
  control = lmeControl(maxIter = 100, msMaxIter = 100, niterEM = 50)
)

# comparing the added ac_var model 

BIC(lme_base_mode_reduced,lme_var_chamber, lme_ac,lme_ac_var)
summary(lme_ac_var)
plot(lme_var_ac)
qqnorm(resid(lme_ac_var))
qqline(resid(lme_ac_var))

# best model is the var ( chamber) and auto- correlation (chamber)

lme_final_with_pile <- update(
  lme_ac_var,
  . ~ . + Pile
)

summary(lme_final_with_pile)

# adding pile makes the model worse
# pile was not an important factor yet other factors that are associated with management where important 
# SWC, TS, DaysSinceLastTurn, Total_Turns,

# Points of Further Discussion
# Here I'm treating chambers individually, is that a mistake? Does this mask the effect of pile ?
# Should I include other variables as Fco2 is one var, but maybe the effect of pile is more clear when looking at interactions between fco2 fch4 and fn2o. 


