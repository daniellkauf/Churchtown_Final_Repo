# Cleaning Raw Flux data individually by Trace Gas: 

# Starting with Co2:

# Linear Flux will be used to avoid over fitting, as often fluxes did not display solely diffusive properties. 
# An R^2 of .9 will be used as a threshold for goodness of fit
# Negative fluxes as well as outliers will be removed as they do not reflect the system. 

# Installing Libraries.

library(ggplot2)
library(dplyr)  
library(readr)


# -9999 is the NA used in licor's software, therefore all -9999 are removed. 
# Given there is no Primary Production in the compost pile - I have removed all negative values. 

# Importing the Dataset. 
Flux_Data_With_Covars <- read_csv("data_clean/Flux_Data_With_Covars.csv")

# Removing all negative values
CO2_NAREMOVED <- Flux_Data_With_Covars %>%
  filter(
    FCO2_DRY.LIN  > 0
  )

# Plot of the Co2 Flux time series of Positive Co2 Flux Values.

ggplot(CO2_NAREMOVED, aes(x = DOY, y = FCO2_DRY.LIN)) +
  geom_point() +
  theme_minimal()

# Plot of the Co2 Flux time series of Positive Co2 Flux Values and R^2 > .9

ggplot(CO2_NAREMOVED, aes(x = DOY, y = FCO2_DRY.LIN, color = FCO2_DRY.LIN_R2 > 0.90)) +
  geom_point() +
  theme_minimal()

# We do see some interesting trends here - Towards the beginning of the experimental cycle more values do not meet the QA/QC criteria (what causes this instability, what is inherently different at the start of the cycle)
# Towards the end of the cycle - it only is low fluxes which do not meet the QA/QC, that makes sense as they are less linear. 

# lets compare groups (r^2 > .9 and r^2 < .95), creating a histogram of each group. 
# creating a column  with True False for .9 r^2 flag and plotting comparison below.

CO2_NAREMOVED <- CO2_NAREMOVED %>%
  mutate(R2_flag_8 = FCO2_DRY.LIN_R2 > 0.8) %>%
  mutate(R2_flag_9 = FCO2_DRY.LIN_R2 > 0.9) %>%
  mutate(R2_flag_95 = FCO2_DRY.LIN_R2 > 0.95)
  
# Histogram of flux values >.9 and less than .9

ggplot(CO2_NAREMOVED, aes(x = FCO2_DRY.LIN, fill = R2_flag_9)) +
  geom_histogram(binwidth = 100, boundary = 0) +
  facet_wrap(~ R2_flag_9, scales = "free_y") +
  theme_minimal()

# Histogram of flux values >.95 and less than .95

ggplot(CO2_NAREMOVED, aes(x = FCO2_DRY.LIN, fill = R2_flag_95)) +
  geom_histogram(binwidth = 100, boundary = 0) +
  facet_wrap(~ R2_flag_95, scales = "free_y") +
  theme_minimal()

# used to find percentage retained - switch R2_flag_9 for the other thresholds to find the percentage retained. 
CO2_NAREMOVED %>%
  summarise(percent_retained = mean(R2_flag_8 > 0.8, na.rm = TRUE) * 100)

# Percentage Retained by cut-off r^2
# 0.95 = 47.5%
# 0.90 = 59.6%
# 0.8 = 72.7

# Visually the higher the r^2 the higher the flux absolute value. Highly negative and highly positive fluxes have higher r^2 values while lower/ around 0 values have poorer r^2 
# When r^2 is increased to .95 fewer negative values are included in the dataset (as these are not reflective of the biological processes going on this is positive )
# yet, this at a cost of biasing the values to higher fluxes. Fluxes also can be due to other processes, which do not invalidate them. 

# .9 is a good starting point, and a commonly used figure in the literature WE WILL BE USING 0.9 R^2 FOR OUR LINEAR CO2 FLUX THRESHOLD MIN R^2

# As the data shows usng a high r^2 eliminates small fluxes. Let's look at low fluxes to see if we can 
# find measurements that have poor r^2 because there is little to no flux.

# Zooming into the low flux measurements  between 100 - 0 umols/m2/s

CO2_NAREMOVED_Low_Flux <- Flux_Data_With_Covars %>%
   filter(
    FCO2_DRY.LIN  > 0,
    FCO2_DRY.LIN < 100
  )

# Lets look at these measurements 

ggplot(CO2_NAREMOVED_Low_Flux, aes(x = DOY, y = FCO2_DRY.LIN, color = FCO2_DRY.LIN_R2 > 0.90)) +
  geom_point() +
  theme_minimal()

# They follow similar trends to the overall data with earlier in the experimental cycle values having poor r^2 and later in the experimental cycle 
# there does seem to be a threshold below high r^2 values are unlikely 25 umoles/m^2/s

CO2_NAREMOVED_Low_Flux <- CO2_NAREMOVED_Low_Flux%>%
  filter(
    CO2_DRY.range  <100
  )

ggplot(CO2_NAREMOVED_Low_Flux, aes(x = DOY, y = FCO2_DRY.LIN, color = FCO2_DRY.LIN_R2 > 0.9)) +
  geom_point() +
  theme_minimal()

# I was thinking of having two ranges of r^2 one for high fluxes (0.9) and one for low fluxes < 100 um/m^2/s but looking at the data
# while it might bias the data, a high r^2 is the best for removing the values we do not want in our dataset. Creating a splitwise QA/QC seems too subjective. 

# Let's eliminate measurements with r^2 < .9 not just create a flag: 

CO2_R_2_Filtered <- CO2_NAREMOVED %>%
  filter(
    FCO2_DRY.LIN_R2  > 0.9
  )

# Simple data characterization:

# Plot the first histogram
hist(CO2_R_2_Filtered$FCO2_DRY.LIN, breaks = "FD", 
     main = "CO2 Emissions", xlab = "FCO2 (umol/m\u00B2/s)")

# log transforming the data:

# Plot the first histogram
hist(log(CO2_R_2_Filtered$FCO2_DRY.LIN), breaks = "FD", 
     main = "CO2 Emissions", xlab = "FCO2 (umol/m\u00B2/s)")

# log transformed data shows more normal distribution.

# Homogeneity of Variance 

boxplot(log(CO2_R_2_Filtered$FCO2_DRY.LIN)~ Pile, data = CO2_R_2_Filtered, 
        main = "Log-transformed FCO2", 
        ylab = "Log(FCO2)", xlab = "Windrow")

# they seem similar, the control pile has more outliers, maybe due to more frequent turning events ?
# We have our data set lets export it into a csv and explore the hypothesis: 

write_csv(CO2_R_2_Filtered, "data_clean/CO2_R_2_Filtered.csv")








