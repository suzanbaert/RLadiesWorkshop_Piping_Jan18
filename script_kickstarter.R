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
#source data: kaggle (https://www.kaggle.com/kemical/kickstarter-projects/data)
kickstarter_masterdata <- read_csv("data/kickstarter_data.csv")



#SELECTING COLUMNS

#original dataframe
kickstarter_masterdata

#selecting columns
kickstarter_masterdata %>%
  select(category, currency, goal, pledged)

#selecing range of colums
kickstarter_masterdata %>%
  select(name:deadline)

#deselecting a column
kickstarter_masterdata %>%
  select(-(X14:X17)) %>%
  colnames()


#changing the name of a column
kickstarter_masterdata %>%
  rename(usd_pledged = `usd pledged`)


#saving a new dataframe to use
kickstarter_data <- kickstarter_masterdata %>%
  rename(usd_pledged = `usd pledged`) %>%
  select(-(X14:X17))
  


#FILTERING ROWS
kickstarter_data %>%
  count()


kickstarter_data %>%
  group_by(country) %>%
  count()


kickstarter_data %>%
  group_by(country) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
