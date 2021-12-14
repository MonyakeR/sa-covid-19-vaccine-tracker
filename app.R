# load libraries
library(shiny)
library(shinydashboard)
library(readr)
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
        fluidRow(
          valueBoxOutput("fully_vaccinated"),
          valueBoxOutput("at_least_one"),
          valueBoxOutput("doses_administered")
        ),
        fluidRow(
          box(
            width = 12,
            title = "Daily vaccine doses administered",
            #status = "primary",
            plotlyOutput("daily_doses")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Cumulative vaccinations",
            #status = "primary",
            plotlyOutput("cumulative_vaccinations")
          )
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
    
    # calculate daily vaccine doses
    sa_data <- sa_data %>% 
      mutate(
        daily_vaccine_doses = c(total_vaccinations[1], diff(total_vaccinations))
      )
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
  
  # daily doses plotly graph
  output$daily_doses <- renderPlotly({
    plot_ly(data_source(), x = ~date, y = ~daily_vaccine_doses) %>% 
      add_bars(
        marker = list(color = "#3392c5")
      ) %>% 
      layout(
        xaxis= list(title = ""),
        yaxis = list(title = "")
      ) %>% 
      config(displayModeBar = FALSE)
  })
  
  # cummulative vaccination plotly graph
  output$cumulative_vaccinations <- renderPlotly({
    plot_ly(
      data_source(), x = ~date, y = ~people_vaccinated,
      type="scatter",
      mode="lines",
      fill = "tozeroy",
      fillcolor = "#9dcce5",
      line = list(
        color = "#3392c5"
      ),
      name = "Received at least one dose"
    ) %>% 
      add_trace(
        x = ~date,
        y = ~people_fully_vaccinated,
        type="scatter",
        mode="lines",
        fill = "tozeroy",
        fillcolor = "#3392c5",
        line = list(
          color = "#9dcce5"
        ),
        name = "Fully vaccinated"
      ) %>% 
      layout(
        legend = list(
          orientation = "h",
          yanchor = "bottom",
          y = 1.02,
          xanchor = "right",
          x = 1
        ),
        xaxis= list(title = ""),
        yaxis = list(title = "")
      ) %>% 
      config(displayModeBar = FALSE)
      
  })
}

# run app
shinyApp(ui, server)