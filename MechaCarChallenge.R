#Deliverable 01

# Load library
library(dplyr)

# Load csv
mpg_df <- read.csv(file='Resources/MechaCar_mpg.csv', check.names=F, stringsAsFactors=F)

# Perform linear regression
mpg_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mpg_df)

# Determine the p-value & r^2 value for the model
summary(mpg_lm)


#Deliverable 02

#Load Suspension_Coil.csv
suspension_df <- read.csv(file='Resources/Suspension_Coil.csv', check.names=F, stringsAsFactors=F)

# Create a total summary
total_summary <- suspension_df %>%
  summarize(Mean=mean(PSI), Median=median(PSI),
            Variance=var(PSI), SD=sd(PSI), .groups='keep')
total_summary

# Create a lot summary
lot_summary <- suspension_df %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI),
            SD=sd(PSI), .groups='keep')
lot_summary

#Deliverable 03

#Perform t.test
t.test(suspension_df$PSI, mu=1500)

#Create three more t.test scripts

#Lot 01 test
t.test(subset(suspension_df, Manufacturing_Lot="Lot1")$PSI, mu=1500)

#Lot 02 test
t.test(subset(suspension_df, Manufacturing_Lot="Lot2")$PSI, mu=1500)

#Lot 03 test
t.test(subset(suspension_df, Manufacturing_Lot="Lot3")$PSI, mu=1500)