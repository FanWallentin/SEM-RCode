

hsschools <- read.delim("https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note1-Rcode/hsschools.txt")
hsschools = hsschools[,c(1:13)]
sapply(hsschools, class)

#install.packages("dplyr")
require(dplyr)

school = hsschools %>% 
  group_by(SCHOOL)

prop.table(table(hsschools$SCHOOL))

GW = as.data.frame(group_split(school)[1][[1]])
PA = as.data.frame(group_split(school)[2][[1]])

require(MVN)
mvn(GW[,5:13],univariateTest = "SW",multivariatePlot = "qq")
mvn(PA[,5:13],univariateTest = "SW",multivariatePlot = "qq")

## or
summary(hsschools[,5:13])

## or
library(psych)
describeBy(hsschools[,5:13],
           group = hsschools$SCHOOL,
           digits= 4)


           