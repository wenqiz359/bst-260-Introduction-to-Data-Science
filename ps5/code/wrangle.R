library(httr2)
library(tidyverse)
library(jsonlite)
library(janitor)
source("census-key.R")
source("funcs.R")


### Question 1

# Read-in and preprocess population data from the US Census API
api <- "https://api.census.gov/data/2021/pep/population"
request <- request(api) |> req_url_query(get = I("POP_2020,POP_2021,NAME"),
                                         `for` = I("state:*"),
                                         key = census_key)
response <- request |> req_perform()
population <- response |> resp_body_json(simplifyVector = TRUE)|> 
  row_to_names(1)|>
  as_tibble()|>
  select(-state)|>
  rename(state_name=NAME) |>
  pivot_longer(-state_name, names_to = 'year',values_to ='population')|>
  mutate(year = str_remove(year,"POP_"))|>
  mutate(across(-state_name,as.numeric))|>
  mutate(state= state.abb[match(state_name,state.name)]) |> # The state name is matched with state abbreviation. 
  mutate(state = case_when (state_name == "Puerto Rico" ~ "PR",
                            state_name == "District of Columbia" ~ "DC",
                            .default = state))

# The population is pivoted to represent 2020 and 2021, and Puerto Rico and District of Columbia are accounted for.


# Read-in and preprocess region data from a JSON file
url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions <- fromJSON(url,simplifyDataFrame = FALSE)|>
  map_df( function(x){
    data.frame(region = x$region, region_name = x$region_name, state_name = x$states)
  })|>
  mutate(region = factor(as.numeric(region)))|>
  mutate(region_name = ifelse(region_name == "New York and New Jersey, Puerto Rico, Virgin Islands", 
                              "NY & NJ, PR, VI", region_name)) #The region name is adjusted for states in the "New York and New Jersey, Puerto Rico, Virgin Islands" group.


# Combine region data with the population table
population <- left_join(population, regions, by ="state_name")


### Question 2

# Reference to a custom function in the functions.R file (function not shown here)


### Question 3

# Read-in four datasets from the CDC using the custom function `get_cdc_data`
cases_raw <- get_cdc_data('https://data.cdc.gov/resource/pwn4-m3yp.json')
hosp_raw <- get_cdc_data('https://data.cdc.gov/resource/39z2-9zu6.json')
deaths_raw <- get_cdc_data('https://data.cdc.gov/resource/r8kw-7aab.json')
vax_raw <- get_cdc_data('https://data.cdc.gov/resource/rh2h-3yt2.json')


### Question 4

# Create a summary table for the four datasets, detailing the outcome, jurisdiction variable names, rate, time_variable_names.
summary_table <- tibble(
  Outcome = c("cases", "hospitalizations", "deaths", "vaccines"),
  Jurisdiction_variable_name = c("state", "jurisdiction", "state", "state"),
  Rate = c("weekly", "daily", "weekly", "daily"),
  time_variable_names = c("end_date", "collection_date", "end_date", "date")
)


### Question 5

# Wrangle the cases data to extract state, mmwr_year, mmwr_week, cases by filtering relevant states.
cases <- cases_raw |> 
  mutate(cases = parse_number(new_cases), 
         date = as_date(ymd_hms(end_date))) |> #extracting case numbers per state for each week
  filter(state %in% population$state) |> #filtering relevant states
  mutate(mmwr_week = epiweek(date), mmwr_year = epiyear(date)) |> #calculating MMWR week and year
  select(state, mmwr_year, mmwr_week, cases)|>
  arrange(state, mmwr_year, mmwr_week)


### Question 6

# Wrangle the hospitalizations data by filtering relevant states, calculating hospitalizations per day, 
# grouping by state, MMWR week/year, and summing hospitalizations per week
hosp <- hosp_raw |> 
  filter(jurisdiction %in% population$state)|>
  rename(hosp = new_covid_19_hospital, state = jurisdiction)|>
  mutate(hosp = as.numeric(hosp),
         date = as_date(ymd_hms(collection_date)))|>
  mutate(mmwr_week = epiweek(date),mmwr_year = epiyear(date))|>
  select(date, mmwr_week, mmwr_year, hosp, state)|>
  group_by(state,mmwr_week, mmwr_year)|>
  summarize(hosp = sum(hosp), n= n(), .groups = 'drop')|>
  filter(n == 7)|>  # ensures there are 7 valid days of hospitalization data per week to be included.
  select(-n)|>
  arrange(state, mmwr_year, mmwr_week)


### Question 7

# Wrangle the provisional COVID-19 deaths data. Match the states, convert to numeric and parse MMWR week and year.
state_abb_extended <- c(state.abb, "DC", "PR")
state_extended <- c(state.name, "District of Columbia", "Puerto Rico")

deaths <- deaths_raw |> 
  rename(deaths = covid_19_deaths)|>
  mutate(state = state_abb_extended[match(state, state_extended)],
         deaths = as.numeric(deaths),
         date = as_date(ymd_hms(end_date)) )|>
  filter(state %in% population$state) |>
  mutate(mmwr_year = epiyear(date),
         mmwr_week = as.numeric(mmwr_week))|>
  select(state, mmwr_year, mmwr_week, deaths)|>
  arrange(state, mmwr_year, mmwr_week)


### Question 8

# Wrangle the vaccination data. Group by state, MMWR week, and year to get cumulative series complete 
# and booster data for each week.
vax <- vax_raw |> 
  rename(state = location)|>
  mutate(date = as_date(ymd_hms(date)) )|>
  filter(state %in% population$state) |>
  mutate(mmwr_year = epiyear(date),
         mmwr_week = epiweek(date))|>
  group_by(state, mmwr_year, mmwr_week)|>
  summarize(series_complete_cumulative_week = max(as.numeric(series_complete_cumulative)), 
            booster_cumulative_week = max(as.numeric(booster_cumulative)))|> #The cumulative data is taken as the maximum value for each week.
  arrange(state, mmwr_year, mmwr_week)


### Question 9

# Generate all MMWR week and year combinations from 2020 to 2021
all_dates <- data.frame(date = seq(make_date(2020, 1, 25),
                                   make_date(2021, 12, 31), 
                                   by = "week")) |>
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) |>
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date)) 

# A cross join is performed between all MMWR week/year combinations and all states, and the resulting dataset is joined with population data.
dates_and_pop <- cross_join(all_dates, data.frame(state = unique(population$state))) |> 
  left_join(population, by = c("state", "mmwr_year" = "year"))

# The final dataset is created by joining population, cases, hospitalizations, deaths, and vaccination datasets together.
dat <- dates_and_pop|> 
  left_join(cases, by = c('state','mmwr_week','mmwr_year'))|>
  left_join(hosp, by = c('state','mmwr_week','mmwr_year'))|>
  left_join(deaths, by = c('state','mmwr_week','mmwr_year'))|>
  left_join(vax, by = c('state','mmwr_week','mmwr_year'))|>
  arrange(state, mmwr_year, mmwr_week)

# Save the final dataset as an RDS file to the data directory
saveRDS(dat, file = "../data/dat.rds")

