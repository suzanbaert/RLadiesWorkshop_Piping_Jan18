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



#is there any data in the last 4 columns?
summary(kickstarter_masterdata)

kickstarter_masterdata %>%
  select(X14, X15, X16, X17)


#or shorter
kickstarter_masterdata %>%
  select(X14:X17)

#
#or shorter
kickstarter_masterdata %>%
  select(X14:X17) %>%
  summarise(na = sum(!is.na(X14)))

kickstarter_masterdata %>%
  select(X14:X17) %>%
  arrange(desc(X14))


glimpse(kickstarter_masterdata)
