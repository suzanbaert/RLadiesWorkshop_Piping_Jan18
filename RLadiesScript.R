library(tidyverse)

kickstarter <- read_csv("data/kickstarter_data_2018.csv")

kickstarter %>%
  select(ID, name, main_category, category,
         date_launched = launched, date_deadline = deadline,
         currency, goal, pledged, state, backers, country) %>%
  mutate(ID = as.character(ID)) %>%
  saveRDS("kickstarter.RDS")


kickstarter <- readRDS("kickstarter.RDS")


#random sample of 100 rows for column exploration
kickstarter_sample <- kickstarter %>%
  sample_n(100)

glimpse(kickstarter)


#### pipe examples ####
#how many projects are successful versus other status's?
kickstarter %>%
  count(state)


kickstarter %>%
  count(state) %>%
  arrange(desc(n))


kickstarter %>%
  group_by(state) %>%
  summarise(n = n(), perc_successful = n/nrow(.)) %>%
  arrange(desc(n))


kickstarter %>%
  filter(main_category == "Technology") %>%
  group_by(state) %>%
  summarise(n = n(), percent = n/nrow(.)) %>%
  arrange(desc(n))


kickstarter %>%
  group_by(main_category) %>%
  summarise(perc_successful = mean(state == "successful")) %>%
  arrange(desc(perc_successful))

kickstarter %>%
  group_by(main_category) %>%
  summarise(n = n(),
            perc_successful = mean(state == "successful"),
            avg_backers = mean(backers),
            avg_pledged = mean(pledged)) %>%
  arrange(desc(perc_successful))


k


#### COLUMN SELECTIONS ####

#selecting columns: order of selection determines the order in the output
#rename inside the select statement newcol = col
kickstarter_sample %>%
  select(main_category, category, project = name, state)



#selecting a chunk of columns
kickstarter_sample %>%
  select(name:main_category, state, backers) %>%
  rename(project = name)


#deselecting columns
kickstarter_sample %>%
  select(-ID, -(currency:pledged))



#selecting based on naming

#selecting based on names
kickstarter_sample %>%
  select(contains("category"), starts_with("g"), ends_with("pledged"))

#selecting based on regex
kickstarter_sample %>%
  select(matches(".+d$"))



#select based on type of column
#select if
kickstarter_sample %>%
  select_if(is.character)


kickstarter_sample %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm=TRUE)>500)


kickstarter_sample %>%
  select_if(~is.numeric(.) & mean(., na.rm=TRUE)>500)


kickstarter_sample %>%
  select_if(~n_distinct(.) < 20)



#### MAKING NEW COLUMNS ####

#pasting together
#opposite is separate
kickstarter_sample %>%
  unite(goal_currency, currency, goal, sep=" ") %>%
  select(name, goal_currency)

#from calculations
kickstarter_sample %>%
  select(name, pledged, goal) %>%
  mutate(delta_pledged_goal = pledged - goal)

#regex
kickstarter_sample %>%
  mutate(name_first_word = str_extract(name, pattern = "^\\w+")) %>%
  select(name, name_first_word)

kickstarter_sample %>%
  mutate(name_first_word = grep(pattern = "^\\w+", name)) %>%
  select(name, name_first_word)


#### COLUMNS FROM OTHER TABLES ####
country_info <- read_csv("data/country_codes.csv")
glimpse(country_info)

#separating columns
country_info <- country_info %>%
  separate(ISO_CODES, into = c("code_alpha2", "code_alpha3"), sep = " / ")

#joining column data
kickstarter %>%
  select(project = name, state, country) %>%
  left_join(country_info, by = c("country" = "code_alpha2"))





#### ROWS ####


#arranging rows
kickstarter %>%
  select(main_category, category, project = name, backers) %>%
  arrange(desc(backers))



#filtering numeric rows
kickstarter %>%
  filter(goal < 1000) %>%
  select(name, goal)

kickstarter %>%
  #filter(goal <= 1000, goal >= 500) %>%
  filter(between(goal, 500, 1000)) %>%
  select(name, goal)


#filtering character rows
kickstarter %>%
  filter(state == "successful") %>%
  count(state)


kickstarter %>%
  filter(state %in% c("successful", "failed")) %>%
  count(state)


kickstarter %>%
  filter(state != "undefined") %>%
  count(state) %>%
  arrange(desc(n))

#AND clause in filter
kickstarter %>%
  filter(country == "BE", goal > 200000)

#AND and OR clause
kickstarter %>%
  filter(country == "BE", (state == "successful" | goal > 1000000))





#regex
kickstarter %>%
  filter(grepl(pattern=" cats? ", name)) %>%
  select(name, state)

#or with stringr
kickstarter %>%
  #ilter(grepl(pattern=" cats? ", name)) %>%
  filter(str_detect(name, pattern=" cats? ")) %>%
  select(name, state)

kickstarter %>%
  filter(str_detect(str_to_lower(name), pattern=" cats? ")) %>%
  select(name, state)

#word might be in multiple columns
kickstarter %>%
  select(name, contains("category")) %>%
  filter_all(any_vars(str_detect(., pattern = "fashion")))


#advanced filtering
kickstarter %>%
  select(project = name, goal, pledged) %>%
  filter_all(any_vars(.<5000))

kickstarter %>%
  select(project = name, goal, pledged) %>%
  filter_all(any_vars(.<5000)) %>%
  arrange(goal)

kickstarter %>%
  select(name, goal, pledged) %>%
  filter_if(is.numeric, all_vars(.>1000000)) %>%
  mutate(goal = goal/1000000, pledged = pledged/1000000) %>%
  rename(goal_million = goal, pledged_million = pledged)
  
  
  
  
#### miscellaneous ####

#what different categories are there? 
kickstarter %>%
  group_by(main_category) %>%
  arrange %>%
  pull(main_category)

#or pull out a mean as a vector, rather than a tibble of 1x1
kickstarter %>%
  filter(state == "successful", country=="BE") %>%
  summarise(mean(pledged)) %>%
  pull()


#knitr::kable
kickstarter %>%
  group_by(main_category) %>%
  summarise(n = n(),
            perc_successful = mean(state == "successful"),
            avg_backers = mean(backers),
            avg_pledged = mean(pledged)) %>%
  arrange(desc(perc_successful)) %>%
  knitr::kable()



#add print(n=nrow(.))
### in some tibble versions only 10 rows are printed. 
### if you want to force printing all rows, use:



print(n=nrow(.))



#### other uses of piping ####

#webscraping
library(xml2)
library(rvest)

read_html("https://www.relisten.be/playlists/radio1/01-01-2018.html") %>%
  html_nodes(css = ".media-body > h4 > span") %>%
  html_text()



#modeling: purrr and broom
library(tidyverse)
library(broom)

mtcars %>%
  nest(-cyl) %>%
  mutate(model = map(data, ~lm(mpg ~ wt, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)


##many many more