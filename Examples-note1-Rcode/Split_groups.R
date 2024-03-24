

hsschools <- read.delim("C:/SEM-UU/2024/Examples/Lavaan/hsschools.txt")
hsschools = hsschools[,c(1:13)]
sapply(hsschools, class)

#install.packages("dplyr")
require(dplyr)
hsschools$SCHOOL = as.factor(hsschools$SCHOOL)

school = hsschools %>% 
  group_by(SCHOOL)

GW = group_split(school)[2]
PA = group_split(school)[1]
