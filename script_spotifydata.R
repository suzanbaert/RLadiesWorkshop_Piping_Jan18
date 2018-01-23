library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)


#intro on piping
#use the pizza example
#how does piping work? 
  #functions that take a dataframe
  #data is mentioned upfront

#principle, even though this one is not really useful
str(mtcars)
mtcars %>%
  str()

#for functions with additional arguments



#or more complicated
ggplot(data = mtcars) +
  geom_point(aes(x = mpg, y = wt))

mtcars %>%
  ggplot() +
  geom_point(aes(x = mpg, y = wt))



#data import
spotify_masterdata <- read_csv("data/spotify_worldwide_daily_ranking_090118.csv")

#dimensions of the data
dim(spotify_masterdata)

#names of the data
colnames(spotify_masterdata)
head(spotify_masterdata)

#disovering the data
str(spotify_masterdata)
glimpse(spotify_masterdata)




#how many rows do I have?
#not really  useful, i could have done this via dim() for instance
spotify_masterdata %>%
  count()

#how many countries do i have data for?
spotify_masterdata %>%
  group_by(Region) %>%
  count()

#which are those countries?
spotify_masterdata %>%
  group_by(Region) %>%
  count() %>%
  pull(Region)





#discovering belgium, finding 74 190 rows
spotify_masterdata %>%
  filter(Region == "be")

#if i'm going to do this often, I can save this dataframe as spotify_belgium for instance
spotify_belgium <- spotify_masterdata %>%
  filter(Region == "be")

#let's look at it
glimpse(spotify_belgium)



#how much data do i have
spotify_masterdata %>%
  filter(Region == "be") %>%
  summarise(n=n())

#
spotify_masterdata %>%
  filter(Region == "be") %>%
  summarise(n=n(),
            firstday = min(Date), lastday = max(Date))



#what dates do i have data for?
spotify_masterdata %>%
  filter(Region == "be") %>%
  group_by(Date) %>%
  summarise(n=n())

#Ýwhat is the last date?
#what dates do i have data for?
spotify_masterdata %>%
  filter(Region == "be") %>%
  group_by(Date) %>%
  summarise(n=n()) %>%
  tail()

#or
#what dates do i have data for?
spotify_masterdata %>%
  filter(Region == "be") %>%
  summarise(n=n(), first_date = min(Date), last_date = max(Date))
