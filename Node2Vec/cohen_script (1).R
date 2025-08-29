# ** Author: Adebiyi Sobitan, Ph.D.
# *Usage: For calculating cohen's D values
# Understanding Cohen's d:

#. Small effect 0.2<= |d| < 0.5 .Noticeable difference but small
#. Medium effect 0.5 <= |d| < 0.8 .Moderate difference btw groups and likely meaningful
#. Large effect: |d| >= 0.8  . Substantial difference btw groups and very likely meaningful


# Define group statistics for each node

mean_male<- 0.341
sd_male<- 0.088
n_male<- 53    # sample size

mean_Female<-  0.318
sd_Female<- 0.085
n_Female<- 61    # sample size

# calculate pooled standard deviation
sd_pooled<- sqrt(((n_male - 1) * sd_male^2 + (n_Female - 1) * sd_Female^2) / (n_male + n_Female - 2 ))

# Calculate Cohen's d
cohen_d<- (mean_male - mean_Female) / sd_pooled

# output
cat("cohen's d:", cohen_d, "\n")

# Negative means Females have higher mean score than males.
# Positive mean Males have higher mean score than Females.
# When interpreting, focus on the absolute value.


#******* Group calculations for Cohen's d Test ********

setwd("C:/Users/sobitanab/Documents/Ade/Edges_2025")

chooseCRANmirror(ind=1)
install.packages(c("dplyr", "readxl"))
library(dplyr)
library(readxl)

rh_cohen_data<- read_excel("Cohen_LH_input_btw.xlsx")

rh_cohen_data<-read.csv("Edges_and_Weights_stats.csv")

num_col<- c("Mean_Male", "sd_Male", "n_Male", "Mean_Female", "sd_Female", "n_Female")
rh_cohen_data[num_col]<- lapply(rh_cohen_data[num_col], as.numeric)

rh_cohen_data
rh_cohen_data<- rh_cohen_data %>%
       rowwise() %>%
       mutate(
         sd_pooled= sqrt(((n_Male - 1) * sd_Male^2 + (n_Female - 1) * sd_Female^2) / (n_Male + n_Female - 2 )),
         cohen_d= (Mean_Male - Mean_Female) / sd_pooled
       ) %>%
       ungroup()

print(rh_cohen_data)
write.csv(rh_cohen_data, file="Edges_output2.csv")



rh_cohen_data$Mean_Male
