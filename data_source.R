# load libraries
library(readr)
library(dplyr)

# our world in data github source for south africa
owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/South%20Africa.csv"

sa_data <- read_csv(owid_url)

fully_vaccinated <- sa_data %>% 
  dplyr::filter(date == max(date)) %>%
  dplyr::select(people_fully_vaccinated) %>% 
  dplyr::pull()

doses_administered <- sa_data %>% 
  dplyr::filter(date == max(date)) %>%
  dplyr::select(total_vaccinations) %>% 
  dplyr::pull()

at_least_one_dose <- sa_data %>% 
  dplyr::filter(date == max(date)) %>%
  dplyr::select(people_vaccinated) %>% 
  dplyr::pull()

sa_total_pop <- 59.62 * 1000000
sa_adult_pop <- 39798201

