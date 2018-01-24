library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)


#intro on piping
#use the pizza example
#how does piping work? 
#syntax of pipe operator



#DATA

#data import
#source data: kaggle (https://www.kaggle.com/kemical/kickstarter-projects/data)
kickstarter <- read_csv("data/kickstarter_data_2018.csv")
country_info <- read_csv("data/country_codes.csv")







#CRAWL EXAMPLES

#how many projects are successful versus other status's?
kickstarter %>%
  count(state) %>%
  arrange(desc(n))


kickstarter %>%
  group_by(state) %>%
  summarise(n = n(), percent = n/nrow(.)) %>%
  arrange(desc(n))



#which project received the most backers?
kickstarter %>%
  select(main_category, category, project = name, backers) %>%
  arrange(desc(backers))



#what different categories are there? 
kickstarter %>%
  group_by(main_category) %>%
  count() %>%
  arrange %>%
  pull(main_category)


#which category has the highest succes
kickstarter %>%
  count(main_category, state) %>%
  spread(state, n) %>%
  select(main_category, successful, failed) %>%
  mutate(total = failed + successful,
         prct_successful = successful/total) %>%
  arrange(desc(prct_successful))


#which category has the highest succes
kickstarter %>%
  count(main_category, state) %>%
  spread(state, n) %>%
  select(main_category, successful, failed) %>%
  mutate(total = failed + successful,
         prct_successful = successful/total) %>%
  arrange(desc(total)) %>%
  ggplot()+
    geom_col(aes(x=fct_inorder(main_category), y=total, fill=prct_successful)) +
    scale_fill_viridis_c(option="viridis", direction = -1, guide = guide_legend("Percent successful")) +
    labs(title = "Number of kickstarters by category and percent successful",
         x= "Category", y="Total amout of kickstarters")



library(forcats)
p + aes(x = fct_inorder(EffectNames))


#filtering based on words

kickstarter %>%
  filter





#which cat


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




