#by Annie Ma
#for Chalkbeat New York 

library(dplyr)
library(tidyverse)

rm(list = ls())

#read in and bind charter and public school data for citymath table
df1 <- read.csv("data/SchoolMathResults20132016Public.csv", header = TRUE, 
                stringsAsFactors = FALSE, skip = 6)

df2 <- read.csv("data/CharterSchoolResults20132016PublicMath.csv", 
                header = TRUE, stringsAsFactors = FALSE, skip = 6)

citymath <- rbind(df1, df2)


#observations smaller than 5 are coded as "s" 
#recoded here to 0 for purposes of analysis 
citymath[ citymath == "s" ] <- -0
df1[ df1 == "s" ] <- -0
df2[ df2 == "s" ] <- -0


#Convert types to numeric 
citymath$PassPercent<- as.numeric(citymath$PassPercent)
citymath$PassNum <-  as.numeric(citymath$PassNum)
citymath$Number.Tested <- as.numeric(citymath$Number.Tested)
citymath$L2Num <- as.numeric(citymath$L2Num)
citymath$L2Percent <- as.numeric(citymath$L2Percent)



#calculate a progress measure
progress <- citymath %>% 
  filter(Year == 2016) %>% 
  filter(Grade == "All Grades") %>% 
  mutate(Progress = L2Percent + PassPercent)

#check the overall pass rate in the citymath
passrate <- progress %>% summarise(sum(PassNum)/sum(Number.Tested))

#clean data and wrangle into top and bottom scorers across public and charter schools
city2016math_top10 <- citymath %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2016) %>% 
  arrange(desc(PassPercent)) %>% 
  head(10)

city2016math_bottom10 <- citymath %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2016) %>%
  filter(PassPercent >= 0) %>% 
  arrange(PassPercent) %>% 
  head(10)

#use spread function to reshape dataframe to make percent change/improvement 
#calculation possible 
percentChange <-citymath %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2015 | Year == 2016) %>% 
  select(School.Name, DBN, Mean.Scale.Score, Year) %>% 
  spread(Year, Mean.Scale.Score)

#convert to numeric 
percentChange$`2015` <- as.numeric(percentChange$`2015`)
percentChange$`2016` <- as.numeric(percentChange$`2016`)

#calculate the most improved/declined schools on mean scale scores 
improvedMath <- percentChange %>% 
  mutate(change = (`2016` - `2015`)/`2015` *100) %>% 
  arrange(desc(change)) %>% 
  head(10)

declineMath <- percentChange %>% 
  mutate(change = (`2016` - `2015`)/`2015` *100) %>% 
  arrange(change) %>% 
  head(10)

#integrity check to see how many students in those schools took tests
#identifies instances of opt-out when few people are taking tests, which 
#can distort this measure
numtested_i <- citymath %>% filter(School.Name %in% improvedMath$School.Name) %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2016) %>% 
  select(Number.Tested, School.Name)

numtested_d <- citymath %>% filter(School.Name %in% declineMath$School.Name) %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2016) %>% 
  select(Number.Tested, School.Name)

