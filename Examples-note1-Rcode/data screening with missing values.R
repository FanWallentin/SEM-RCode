#This file is to show the number of missing values per variable in the data#

Screening
#################################################
# 0. Setup
#################################################

# Install packages if needed
# install.packages(c("tidyverse", "psych", "naniar", "MVN", "mice"))

library(tidyverse)
library(psych)
library(naniar)
library(MVN)
library(mice)
library(lavaan)

#################################################
# 1. Import Data
#################################################
urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note1-Rcode/HOSWGWMIS.txt"
data <- read.table(urlfile,
                   header = TRUE, na.strings = c("-9"))

# Quick check
str(data)
head(data)

#################################################
# 2. Missing Data Screening
#################################################

# Count missing values per variable
colSums(is.na(data))

#################################################
# 3. Descriptive Statistics
#################################################

describe(data)

# Look for impossible values manually if needed
summary(data)

#################################################
# 4. Normality Check
#################################################

mvn(data)

#################################################
# 5. Outlier Detection
#################################################

# Univariate outliers (boxplots)
boxplot(data[,1:3], cex.axis = 0.6, cex.lab = 0.5,main = "Boxplots for all variables")
boxplot(data[,4:6], cex.axis = 0.6, cex.lab = 0.5,main = "Boxplots for all variables")
boxplot(data[,7:9], cex.axis = 0.6, cex.lab = 0.5,main = "Boxplots for all variables")
boxplot(data[,10:12], cex.axis = 0.6, cex.lab = 0.5,main = "Boxplots for all variables")

#################################################
# 6. Handle Missing Data
#################################################

# Option A: Use FIML (recommended for SEM)
# (No changes needed to dataset)

# Option B: Multiple Imputation
imp <- mice(data, m = 5, method = "pmm", seed = 123)
#??mice

# Completed dataset (example: first imputed dataset)
data_imp <- complete(imp, 1)

#################################################
# END OF SCRIPT
#################################################


