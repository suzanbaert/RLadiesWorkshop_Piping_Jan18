
## reformatting all columns
msleep %>%
  select_all(tolower)

#imagine an unclean database:
msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c("name", "sleep total", "brain weight")

msleep2 %>%
  select_all(~str_replace(., " ", "_"))




# 
# 
# ## **Row names to column**
# 
# Some dataframes have rownames that are not actually a column in itself, like the mtcars dataset: 
#   
#   ```{r}
# mtcars %>%
#   head
# ```
# 
# 
# If you want this column to be an actual column, you can use the `rownames_to_column()` function, and specify a new column name.
# 
# ```{r}
# mtcars %>%
#   rownames_to_column("car_model") %>%
#   head
# ```
