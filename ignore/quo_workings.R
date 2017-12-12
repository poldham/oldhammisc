my_summarise(df, quo(g1))

my_summarise <- function(df, group_var) {
  df %>%
    group_by(!!group_var) %>%
    summarise(a = mean(a))
}

my_select <- function(df, select_var){
  df %>% 
    select(!!select_var)
}
# that works my_select(df = country_data, select_var = quo(iso))

my_filter <- function(df, filter_var, value){
  df %>% 
    filter(!!!filter_var == value)
}


my_fun <- quo(fun)
quo(!!my_fun(x, y, z))
quo(UQ(my_fun)(x, y, z))
my_var <- quo(x)
quo(filter(df, !!my_var == 1))

#https://stackoverflow.com/questions/45134317/how-to-filter-a-data-frame-programmatically-with-dplyr-and-tidy-evaluation
# this works
filter_countrydata <- function(...) {
  filter(country_data, ...)
}

filter_countrydata(iso == "VN") # 

# this also works but on any col
filter_countrydata1 <- function(...) {
  cols <- quos(...)
  filter(country_data, !!!cols)
}



