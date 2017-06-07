#by Annie Ma
#for Chalkbeat New York 

library(dplyr)
library(tidyr)

rm(list = ls())

#read in and bind charter and public school data for cityela table
df1 <- read.csv("data/SchoolELAResults20132016Public.csv", header = TRUE, 
                stringsAsFactors = FALSE, skip = 6)

df2 <- read.csv("data/CharterSchoolResults20132016PublicELA.csv", 
                header = TRUE, stringsAsFactors = FALSE, skip = 6)

cityela <- rbind(df1, df2)


#observations smaller than 5 are coded as "s" 
#recoded here to 0 for purposes of analysis
cityela[ cityela == "s" ] <- 0 

#convert types to numeric 
cityela$PassPercent<- as.numeric(cityela$PassPercent)
cityela$PassNum <-  as.numeric(cityela$PassNum)
cityela$Number.Tested <- as.numeric(cityela$Number.Tested)
cityela$L2Num <- as.numeric(cityela$L2Num)
cityela$L2Percent <- as.numeric(cityela$L2Percent)
cityela$Mean.Scale.Score <- as.numeric(cityela$Mean.Scale.Score)


#calculate a progress measure
progress <- cityela %>% 
  filter(Year == 2016) %>% 
  filter(Grade == "All Grades") %>% 
  mutate(Progress = L2Percent + PassPercent)

#check the overall pass rate in the cityela
passrate <- progress %>% summarise(sum(PassNum)/sum(Number.Tested))

cityela2016ela_top <- cityela %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2016) %>% 
  arrange(desc(PassPercent)) %>% 
  head(10)

cityela2016_bottom <- cityela %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2016) %>%
  filter(PassPercent >= 0) %>% 
  arrange(PassPercent) %>% 
  head(10)

#use spread function to reshape dataframe to make percent change/improvement 
#calculation possible 
percentChange <-cityela %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2015 | Year == 2016) %>% 
  select(School.Name, DBN, Mean.Scale.Score, Year) %>% 
  spread(Year, Mean.Scale.Score)

percentChange$`2015` <- as.numeric(percentChange$`2015`)
percentChange$`2016` <- as.numeric(percentChange$`2016`)

#calculate the most improved/declined schools on mean scale scores 
improvedELA <- percentChange %>% 
  mutate(change = (`2016` - `2015`)/`2015` *100) %>% 
  arrange(desc(change)) %>% 
  head(10)

declineELA <- percentChange %>% 
  mutate(change = (`2016` - `2015`)/`2015` *100) %>% 
  arrange(change) %>% 
  head(10)

#integrity check to see how many students in those schools took tests
#identifies instances of opt-out when few people are taking tests, which 
#can distort this measure
numtested_i <- cityela %>% filter(School.Name %in% improvedELA$School.Name) %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2016) %>% 
  select(Number.Tested, School.Name)

numtested_d <- cityela %>% filter(School.Name %in% declineELA$School.Name) %>% 
  filter(Grade == "All Grades") %>% 
  filter(Year == 2016) %>% 
  select(Number.Tested, School.Name)
