# load libraries
library(readr)
library(dplyr)

# our world in data github source for south africa
owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/South%20Africa.csv"

sa_data <- read_csv(owid_url)

sa_total_pop <- 59.62 * 1000000
sa_adult_pop <- 39798201

# calculate data for the score cards
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

# get daily vaccination numbers
sa_data <- sa_data %>% 
  mutate(
    daily_vaccine_doses = c(total_vaccinations[1], diff(total_vaccinations))
  )

# create plot for daily doses
bar_plot <- ggplot(sa_data, aes(date, daily_vaccine_doses)) + 
  geom_col()

ggplotly(bar_plot)

# plotly
plot_ly(sa_data, x = ~date, y = ~daily_vaccine_doses) %>% 
  add_bars()

# data for last 60 days

# data for last 30 days
  
