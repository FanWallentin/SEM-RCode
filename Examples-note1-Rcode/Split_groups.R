

hsschools <- read.delim("https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note1-Rcode/hsschools.txt")
hsschools = hsschools[,c(1:13)]
sapply(hsschools, class)
hsschools[,1:4] <- sapply(hsschools[,1:4],as.factor)

#install.packages("dplyr")
require(dplyr)

school = hsschools %>% 
  group_by(SCHOOL)

prop.table(table(hsschools[,1]))

GW = as.data.frame(group_split(school)[1][[1]])
PA = as.data.frame(group_split(school)[2][[1]])

require(MVN)
mvn(hsschools[,5:13],univariateTest = "SW",multivariatePlot = "qq"); mtext(paste0("(", "HSSCHOOL", ")"), side=3)
mvn(GW[,5:13],univariateTest = "SW",multivariatePlot = "qq"); mtext(paste0("(", "GW", ")"), side=3)
mvn(PA[,5:13],univariateTest = "SW",multivariatePlot = "qq"); mtext(paste0("(", "PA", ")"), side=3)



#install.packages("ggplot2")
library(ggplot2)
library(ggpubr)
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

## HSSCHOOLS
a =  ggplot(hsschools[,1:4], aes(x = SCHOOL)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

b = ggplot(hsschools[,1:4], aes(x = GENDER)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
  
c =  ggplot(hsschools[,1:4], aes(x = AGEYEAR)) +
              geom_bar(alpha=0.5) +
              geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

d =  ggplot(hsschools[,1:4], aes(x = BIRTHMON)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")


ggarrange(a,b,c,d,
          ncol = 2, nrow = 2,labels = "HSSCHOOL" )

## GW

b_1 = ggplot(GW[,1:4], aes(x = GENDER)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
  
c_1 =  ggplot(GW[,1:4], aes(x = AGEYEAR)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

d_1 =  ggplot(GW[,1:4], aes(x = BIRTHMON)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")


ggarrange(ggarrange(b_1,c_1,
          ncol = 2, nrow = 1 ),d_1,nrow = 2,labels = "GW")

## PA
b_2 = ggplot(PA[,1:4], aes(x = GENDER)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

c_2 =  ggplot(PA[,1:4], aes(x = AGEYEAR)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

d_2 =  ggplot(PA[,1:4], aes(x = BIRTHMON)) +
  geom_bar(alpha=0.5) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")


ggarrange(ggarrange(b_2,c_2,
                    ncol = 2, nrow = 1 ),d_2,nrow = 2,labels = "PA")



