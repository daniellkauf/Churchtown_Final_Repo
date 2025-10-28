# Cleaning Raw Flux data individually by Trace Gas: 

# N2O

# Linear Flux will be used to avoid over fitting, as often fluxes did not display solely diffusive properties. 
# An R^2 of .9 will be used as a threshold for goodness of fit
# Negative fluxes as well as outliers will be removed as they do not reflect the system. 

# Installing Libraries.

library(ggplot2)
library(dplyr)  
library(readr)


# -9999 is the NA used in licor's software, therefore all -9999 are removed. 
# I have removed all negative values as I do not believe they are reflective of the system. 

# Importing the Dataset. 
Flux_Data_With_Covars <- read_csv("data_clean/Flux_Data_With_Covars.csv")

# Removing all negative values
N2O_NAREMOVED <- Flux_Data_With_Covars %>%
  filter(
    FN2O_DRY.LIN  > 0 
  )

# Plot of the n2o Flux time series of Positive n2o Flux Values.

ggplot(N2O_NAREMOVED, aes(x = DOY, y = FN2O_DRY.LIN)) +
  geom_point() +
  theme_minimal()

# Plot of the n2o Flux time series of Positive n2o Flux Values and R^2 > .9

ggplot(N2O_NAREMOVED, aes(x = DOY, y = FN2O_DRY.LIN, color = FN2O_DRY.LIN_R2 > 0.90)) +
  geom_point() +
  theme_minimal()

# it only is low fluxes which do not meet the QA/QC, that makes sense as they are less linear. 
# for n2o the trend is not the same as for co2 and ch4 where there are more measurements which do reach the threshold in the earlier parts of the cycle. 
# This is kinda of interesting but, it might have to do this n2o dynamics as the peak would not be at the start I dont think. I'm not sure about this. Also levels are much lower 


# lets compare groups (r^2 > .9 and r^2 < .95), creating a histogram of each group. 
# creating a column  with True False for .9 r^2 flag and plotting comparison below.

N2O_NAREMOVED <- N2O_NAREMOVED %>%
  mutate(R2_flag_8 = FN2O_DRY.LIN_R2 > 0.8) %>%
  mutate(R2_flag_9 = FN2O_DRY.LIN_R2 > 0.9) %>%
  mutate(R2_flag_95 = FN2O_DRY.LIN_R2 > 0.95)

# Histogram of flux values >.9 and less than .9

ggplot(N2O_NAREMOVED, aes(x = FN2O_DRY.LIN, fill = R2_flag_9)) +
  geom_histogram(binwidth = 100, boundary = 0) +
  facet_wrap(~ R2_flag_9, scales = "free_y") +
  theme_minimal()

# Histogram of flux values >.95 and less than .95

ggplot(N2O_NAREMOVED, aes(x = FN2O_DRY.LIN, fill = R2_flag_95)) +
  geom_histogram(binwidth = 100, boundary = 0) +
  facet_wrap(~ R2_flag_95, scales = "free_y") +
  theme_minimal()

# used to find percentage retained - switch R2_flag_9 for the other thresholds to find the percentage retained. 
N2O_NAREMOVED %>%
  summarise(percent_retained = mean(R2_flag_95 > 0.9, na.rm = TRUE) * 100)

# Percentage Retained by cut-off r^2
# 0.95 = 90.3%
# 0.90 = 93.6%
# 0.8 = 95.8

# Visually the higher the r^2 the higher the flux absolute value. Highly negative and highly positive fluxes have higher r^2 values while lower/ around 0 values have poorer r^2 
# When r^2 is increased to .95 fewer negative values are included in the dataset (as these are not reflective of the biological processes going on this is positive )
# yet, this at a cost of biasing the values to higher fluxes. Fluxes also can be due to other processes, which do not invalidate them. 

# .9 is a good starting point, and a commonly used figure in the literature WE WILL BE USING 0.9 R^2 FOR OUR LINEAR n2o FLUX THRESHOLD MIN R^2

# As the data shows using a high r^2 eliminates small fluxes. Let's look at low fluxes to see if we can 
# find measurements that have poor r^2 because there is little to no flux.

# Zooming into the low flux measurements
# Low flux based off visual analysis

N2O_NAREMOVED_Low_Flux <- Flux_Data_With_Covars %>%
  filter(
    FN2O_DRY.LIN  > 0,
    FN2O_DRY.LIN < 1500
  )

# Lets look at these measurements 

ggplot(N2O_NAREMOVED_Low_Flux, aes(x = DOY, y = FN2O_DRY.LIN, color = FN2O_DRY.LIN_R2 > 0.90)) +
  geom_point() +
  theme_minimal()

# They follow similar trends to the overall data with earlier in the experimental cycle values having poor r^2 and later in the experimental cycle 
# there does seem to be a threshold below high r^2 values are unlikely 100 umoles (double check unit)/m^2/s

N2O_NAREMOVED_Low_Flux <- N2O_NAREMOVED_Low_Flux%>%
  filter(
    FN2O_DRY.LIN  <100
  )

ggplot(N2O_NAREMOVED_Low_Flux, aes(x = DOY, y = FN2O_DRY.LIN, color = FN2O_DRY.LIN_R2 > 0.9)) +
  geom_point() +
  theme_minimal()

# I was thinking of having two ranges of r^2 one for high fluxes (0.9) and one for low fluxes < 100 um/m^2/s but looking at the data
# while it might bias the data, a high r^2 is the best for removing the values we do not want in our dataset. Creating a splitwise QA/QC seems too subjective. 
# It is worth noting that there are high r^2 very low flux values later on the the experimental cycle - not sure what is causing this and would love to have a better hypothesis. 

# Let's eliminate measurements with r^2 < .9 not just create a flag: 

N2O_R_2_Filtered <- N2O_NAREMOVED %>%
  filter(
    FN2O_DRY.LIN_R2  > 0.9
  )

# Simple data characterization:

# Plot the first histogram
hist(N2O_R_2_Filtered$FN2O_DRY.LIN, breaks = "FD", 
     main = "N2O Emissions", xlab = "Fn2o (nmol/m\u00B2/s)")

# log transforming the data:

# Plot the first histogram
hist(log(N2O_R_2_Filtered$FN2O_DRY.LIN), breaks = "FD", 
     main = "N2O Emissions", xlab = "FN2O (nmol/m\u00B2/s)")

# log transformed data shows more normal distribution.

# Homogeneity of Variance 

boxplot(log(N2O_R_2_Filtered$FN2O_DRY.LIN)~ Pile, data = N2O_R_2_Filtered, 
        main = "Log-transformed N2O", 
        ylab = "Log(N2O)", xlab = "Windrow")

# We have our data set lets export it into a csv and explore the hypothesis: 
# Outlier - it does seem the E pile has higher outliers and the c pile lower but the means are around equal. 

write_csv(N2O_R_2_Filtered, "data_clean/N2O_R_2_Filtered.csv")



