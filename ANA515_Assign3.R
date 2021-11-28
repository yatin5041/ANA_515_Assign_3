#1.Reading data from csv file
library(tidyverse)
stormdata77 <- read.csv(file = "StormEvents_details-ftp_v1.0_d1977_c20210803.csv" )

#2.Limit the dataframe to given columns
myvars <- c("EPISODE_ID", "EVENT_ID", "STATE", "STATE_FIPS", "EVENT_TYPE", 
            "CZ_TYPE", "CZ_FIPS", "CZ_NAME", "BEGIN_DATE_TIME", "END_DATE_TIME", 
            "SOURCE", "BEGIN_LAT", "BEGIN_LON", "END_LAT", "END_LON")
newdata77 <- stormdata77[myvars]

head(newdata77, 5)

#3.Convert the beginning and ending dates to a "date-time" class 
install.packages("lubridate")
library(lubridate)
library(tidyverse)
newdata1 <-
  mutate(newdata77, BEGIN_DATE_TIME = dmy_hms(newdata77$BEGIN_DATE_TIME), 
      END_DATE_TIME = dmy_hms(newdata77$END_DATE_TIME))


#4.Change state and county names to title case.

str_to_title(newdata1$STATE)
str_to_title(newdata1$CZ_NAME)


#5.Limit to the events listed by county FIPS (CZ_TYPE of "C") and then remove the CZ_TYPE column.
newdata2 <- newdata1 %>%
  filter(CZ_TYPE == C)

#remove column CZ_TYPE
newdata1 %>% select(-CZ_TYPE)


#6.Pad the state and county FIPS with a "0" 

str_pad(newdata1$STATE_FIPS, width = 3, side = "left", pad = "0")
str_pad(newdata1$CZ_FIPS, width = 3, side = "left", pad = "0")

unite(newdata1,FIPS,STATE_FIPS,CZ_FIPS, sep = "_", remove = TRUE)

#7.Change all the column names to lower case

rename_all(newdata1, tolower)

#8. Dataset States

data("state")

us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)


#9.Create a dataframe with the number of events per state in the year of your birth

Newset<- data.frame(table(newdata1$STATE))

Newset1<-rename(Newset, c("state"="Var1"))

merged <- merge(x=Newset1,y=us_state_info,by.x="state", by.y="state")

# resolving error due to mismatch of case in the above line

us_state_info1 <-mutate_all(us_state_info, toupper) 

#rerunning after updating case
merged <- merge(x=Newset1,y=us_state_info1,by.x="state", by.y="state")
                  
#10. plot
library(ggplot2)
storm_plot<-ggplot(merged, aes(x=area, y= Freq)) +
  geom_point(aes(color=region))+
  labs (x="land area (square miles)" , y="# of storm events in 1977") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1))

storm_plot







