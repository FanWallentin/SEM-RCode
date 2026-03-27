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
urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note1-Rcode/hsschools.txt"
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

mvn(data,multivariatePlot = "qq")

#################################################
# 5. Outlier Detection
#################################################

# Univariate outliers (boxplots)
boxplot(data[,2:4], cex.axis = 0.6, cex.lab = 0.5,main = "Boxplots for all variables")
boxplot(data[,5:7], cex.axis = 0.6, cex.lab = 0.5,main = "Boxplots for all variables")
boxplot(data[,8:10], cex.axis = 0.6, cex.lab = 0.5,main = "Boxplots for all variables")
boxplot(data[,11:13], cex.axis = 0.6, cex.lab = 0.5,main = "Boxplots for all variables")


#################################################
# 6. Correlation Matrix
#################################################

cor_matrix <- cor(data[,5:13], use = "pairwise.complete.obs")
round(cor_matrix, 2)

 
#Optional: visualize
 library(corrplot)
 corrplot(cor_matrix, tl.cex = 0.5)
 
 #################################################
 # 7. SPLIT TO TWO FILES
 #################################################
 PA = data[data$SCHOOL== 0,]
 GW = data[data$SCHOOL== 1,]
 
 #################################################
 # END OF SCRIPT
 #################################################
 
