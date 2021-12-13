# load libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

# our world in data github source for south africa
owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/South%20Africa.csv"

# Population estimates. Source: http://www.statssa.gov.za/?p=13453,
# https://sacoronavirus.co.za/latest-vaccine-statistics
sa_total_pop <- 59.62 * 1000000
sa_adult_pop <- 39798201

# ui

ui <- dashboardPage(
  dashboardHeader(
    title = "SA Covid-19 Vaccine tracker",
    titleWidth = 300
    
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      menuItem(
        "Provincial",
        tabName = "provincial",
        icon = icon("map-marked")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        #h3("Vaccination progress"),
        fluidRow(
          valueBoxOutput("fully_vaccinated"),
          valueBoxOutput("at_least_one"),
          valueBoxOutput("doses_administered")
          
        )
      ),
      tabItem(
        tabName = "provincial",
        h2("Provincial breakdown")
      )
    )
  )
)

# server
server <- function(input, output, session) {
  
  # data from our world in data github repo
  data_source <- reactive({
    sa_data <- read_csv(owid_url)
  })
  
  # Total Fully vaccinated
  output$fully_vaccinated <- renderValueBox({
    fully_vaccinated <- data_source() %>% 
      dplyr::filter(date == max(date)) %>%
      dplyr::select(people_fully_vaccinated) %>% 
      dplyr::pull()
    
    value <- round((fully_vaccinated / sa_total_pop), 3)
    
    valueBox(
      value = paste0(value * 100,"%"),
      subtitle = "Fully vaccinated",
      color = "olive",
      icon = icon("first-aid")
    )
  })
  
  # Received at least one dose
  output$at_least_one <- renderValueBox({
    at_least_one_dose <- data_source() %>% 
      dplyr::filter(date == max(date)) %>%
      dplyr::select(people_vaccinated) %>% 
      dplyr::pull()
    
    value <- round((at_least_one_dose / sa_total_pop), 3)
    
    valueBox(
      value = paste0(value * 100, "%"),
      subtitle = "Received at least one dose",
      color = "orange",
      icon = icon("plus-square")
    )
  })

  # Total Doses administered
  output$doses_administered <- renderValueBox({
    doses_administered <- data_source() %>% 
      dplyr::filter(date == max(date)) %>%
      dplyr::select(total_vaccinations) %>% 
      dplyr::pull()
    
    valueBox(
      value = paste0(round(doses_administered / 1000000, 2), "M"),
      subtitle = "Doses administered",
      icon = icon("syringe")
    )
  })
  
}

# run app
shinyApp(ui, server)