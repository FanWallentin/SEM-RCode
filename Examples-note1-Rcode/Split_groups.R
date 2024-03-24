

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
mvn(hsschools[,5:13],univariateTest = "SW",multivariatePlot = "qq")
mtext(paste0("(", "HSSCHOOL", ")"), side=3)
mvn(GW[,5:13],univariateTest = "SW",multivariatePlot = "qq")
mtext(paste0("(", "GW", ")"), side=3)
mvn(PA[,5:13],univariateTest = "SW",multivariatePlot = "qq")
mtext(paste0("(", "PA", ")"), side=3)


           