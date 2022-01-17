# load libraries
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(lubridate)
library(plotly)
library(shinyjs)
library(reactable)
library(htmltools)

# our world in data github source for south africa
owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/South%20Africa.csv"

# metrics data url
metrics_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpXRXO5wpKifBgB-OjRO9YLninQte0NMI23qNBYwjR5vBB9yVyXAiZM_m7liamryGXkc0lWYHN7Xlz/pub?output=csv"

# provinces data url
provinces_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT3i-s3GITMfP1GSnU4xEax6fvKeI88Qgfu2PNgHopMrqcPEbTAJftCmrX5en11VjgAvPY4WtY-elfl/pub?output=csv"

# Population estimates. Source: http://www.statssa.gov.za/?p=13453,
# https://sacoronavirus.co.za/latest-vaccine-statistics
sa_adult_pop <- 39798201

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# colour palette 
pal <- function(x) rgb(colorRamp(c("#FFFFFF", "#3392c5"))(x), maxColorValue = 255)

# get the last updated date
metrics_df <- read_csv(metrics_url)
metrics_df$date <- as.Date(metrics_df$date, "%m/%d/%Y")
last_updated <- max(metrics_df$date)
last_updated_day <- day(last_updated)
last_updated_month <- month(last_updated, label = TRUE)
last_updated_year <- year(last_updated)

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
        "Info",
        tabName = "info",
        icon = icon("info-circle")
      )
    )
  ),
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span id = "pageHeader" class="last-updated"></span>\');
      })
     ')),
      useShinyjs()
    ),
    
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
            width = 6,
            title = "Daily vaccine doses administered",
            #status = "primary",
            selectInput(
              "daily_timeframe",
              label = "",
              choices = c(
                "All time" = as.Date("2021-02-16"),
                "Last 60 days" = Sys.Date() - 60,
                "Last 30 days" = Sys.Date() - 30
              ),
              selected = "All time"
            ),
            plotlyOutput("daily_doses")
          ),
          box(
            width = 6,
            title = "Cumulative vaccinations",
            #status = "primary",
            selectInput(
              "cumulative_timeframe",
              label = "",
              choices = c(
                "All time" = as.Date("2021-02-16"),
                "Last 60 days" = Sys.Date() - 60,
                "Last 30 days" = Sys.Date() - 30
              ),
              selected = "All time"
            ),
            plotlyOutput("cumulative_vaccinations")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = NULL,
            reactableOutput("table")
          )
        )
      ),
      tabItem(
        tabName = "info",
        box(
          width = 12,
          title = "Disclaimer, definitions and data sources",
          HTML(
            "<p>The vaccination dashboard shows the vaccination rates as reported by the health department.
            Every day the health department posts the latest information from its <a href = 'https://sacoronavirus.co.za/latest-vaccine-statistics'>vaccine statistics dashboard</a> on its SA Coronavirus website.
            At present, the numbers shown on the dashboard are a snapshot as at 17:00 on a particular day.
            Data in the two graphs, Daily vaccine doses administered and Cumulative vaccinations comes from 
            <a href = 'https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/country_data/South%20Africa.csv'>Our World in Data.</a></p>" 
          ),
          HTML(
            "
            <p>People 18 years and older are the target population of the vaccination roll-out.
            The estimated number of people in this age group used in the table was obtained from Statistics South Africa.
            South Africa is currently using two vaccines, the one-dose Johnson & Johnson (J&J) shot and the two-dose Pfizer.
            People who receive a J&J shot are fully vaccinated, but people need to receive two doses of Pfizer to be fully vaccinated.
            The health department vaccination dashboard has a disclaimer that states:
            “Data displayed in this dashboard only contains vaccination records captured on the live Electronic Vaccination Data System (EVDS) and excludes vaccination records captured on paper within the last 24 hours.
            Totals will be adjusted as back-capturing and data validation are done.”
            </p>"
          ),
          h4("Definitions"),
          HTML(
            "
            <dl>
              <dt>Adults</dt>
              <dd>- People 18 years and older. <b>Source</b>: Statssa Mid-Year population estimates 2020
              </dd>
              <dt>Individuals Vaccinated</dt>
              <dd>- One dose J&J (single dose regimen) or Pfizer (two dose regimen) first dose administered
              </dd>
              <dt>Fully Vaccinated</dt>
              <dd>- Total Number of Individuals that have received a Johnson & Johnson Vaccine or Pfizer 1st dose & 2nd dose.
              </dd>
            </dl>
            "
          )
        )
      )
    )
  )
)

# server
server <- function(input, output, session) {
  
  # last updated text on header
  shinyjs::html(
    "pageHeader", paste(
      "As of:",
      last_updated_day,
      last_updated_month,
      last_updated_year,
      ",",
      "17:00"
    )
  )
  
  # score cards data stored in google sheets
  score_cards <- reactive({
    
    metrics_df <- read_csv(metrics_url)
    metrics_df$date <- as.Date(metrics_df$date, "%m/%d/%Y")
    
    metrics_df
    
  })
  
  # data from our world in data github repo
  data_source <- reactive({
    
    sa_data <- read_csv(owid_url)
    
    # calculate daily vaccine doses
    sa_data <- sa_data %>% 
      mutate(
        daily_vaccine_doses = c(total_vaccinations[1], diff(total_vaccinations))
      )
  })
  
  # table summary data from google sheets
  provincial_data <- reactive({
    provinces <- read_csv(provinces_url) %>%
      na.omit()
  })
  
  # Total Fully vaccinated
  output$fully_vaccinated <- renderValueBox({
    fully_vaccinated <- score_cards() %>% 
      dplyr::filter(date == max(date) & metric == "fully_vaccinated") %>%
      dplyr::select(value) %>% 
      dplyr::pull()
    
    value <- round((fully_vaccinated / sa_adult_pop), 3)
    
    valueBox(
      value = paste0(value * 100,"%"),
      subtitle = "Fully vaccinated",
      color = "olive",
      icon = icon("first-aid")
    )
  })
  
  # Received at least one dose
  output$at_least_one <- renderValueBox({
    at_least_one_dose <- score_cards() %>% 
      dplyr::filter(date == max(date) & metric == "one_plus") %>%
      dplyr::select(value) %>% 
      dplyr::pull()
    
    value <- round((at_least_one_dose / sa_adult_pop), 3)
    
    valueBox(
      value = paste0(value * 100, "%"),
      subtitle = "Received at least one dose",
      color = "orange",
      icon = icon("plus-square")
    )
  })

  # Total Doses administered
  output$doses_administered <- renderValueBox({
    doses_administered <- score_cards() %>% 
      dplyr::filter(date == max(date) & metric == "total_vaccinations") %>%
      dplyr::select(value) %>% 
      dplyr::pull()
    
    valueBox(
      value = paste0(round(doses_administered / 1000000, 2), "M"),
      subtitle = "Doses administered",
      icon = icon("syringe")
    )
  })
  
  # daily doses plotly graph
  output$daily_doses <- renderPlotly({
    
    data_source() %>% 
      filter(date >= input$daily_timeframe) %>% 
      plot_ly(x = ~date, y = ~daily_vaccine_doses) %>% 
      add_bars(
        marker = list(color = "#3392c5")
      ) %>% 
      layout(
        xaxis= list(title = ""),
        yaxis = list(title = ""),
        hovermode = "x unified"
      ) %>% 
      config(displayModeBar = FALSE)
  })
  
  # cummulative vaccination plotly graph
  output$cumulative_vaccinations <- renderPlotly({
    
    data_source() %>% 
      filter(date >= input$cumulative_timeframe) %>% 
      plot_ly(
        x = ~date, y = ~people_vaccinated,
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
        yaxis = list(title = ""),
        hovermode = "x unified",
        hoverlabel = list(namelength = -1)
      ) %>% 
      config(displayModeBar = FALSE)
  })
  
  # provincial summary table
  output$table <- renderReactable({
    reactable(
      provincial_data(),
      defaultSorted = "percentage_fully_vaccinated",
      defaultColDef = colDef(headerClass = "header", align = "left"),
      columns = list(
        province = colDef(
          name = "Province"
        ),
        # total_adult_population = colDef(
        #   name = "Total adult pupolation",
        #   cell = function(value) {
        #     width <- paste0(value * 100 / max(provincial_data()$total_adult_population), "%")
        #     value <- format(value, big.mark = ",")
        #     value <- format(value, width = 10, justify = "right")
        #     bar <- div(
        #       class = "bar-chart",
        #       style = list(marginRight = "6px"),
        #       div(class = "bar", style = list(width = width, backgroundColor = "#3fc1c9"))
        #     )
        #     div(class = "bar-cell", span(class = "number", value), bar)
        #   }
        # ),
        total_adult_population = colDef(
          name = "Total adult pupolation",
          cell = function(value) {
            value <- format(value, big.mark = ",")
          },
          style = function(value) {
            normalized <- (value - min(provincial_data()$total_adult_population)) / (max(provincial_data()$total_adult_population) - min(provincial_data()$total_adult_population))
            color <- pal(normalized)
            list(background = color)
          }
        ),
        total_vaccinated = colDef(
          name = "Received at least one dose",
          cell = function(value) {
            value <- format(value, big.mark = ",")
          }
        ),
        total_fully_vaccinated = colDef(
          name = "Fully vaccinated",
          cell = function(value) {
            value <- format(value, big.mark = ",")
          }
        ),
        percentage_vaccinated = colDef(
          name = "% of adults vaccinated",
          align = "left",
          cell = function(value) {
            width <- paste0(value)
            bar_chart(value, width = width, fill = "#9dcce5", background = "#e1e1e1")
          }
        ),
        percentage_fully_vaccinated = colDef(
          defaultSortOrder = "desc",
          name = "% of adults fully vaccinated",
          align = "left",
          cell = function(value) {
            width <- paste0(value)
            bar_chart(value, width = width, fill = "#3392c5", background = "#e1e1e1")
          }
        )
      )
    )
  })
}

# run app
shinyApp(ui, server)