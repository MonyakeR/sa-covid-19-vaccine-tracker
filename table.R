library(tidyverse)
library(reactable)
library(htmltools)

sheets_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT3i-s3GITMfP1GSnU4xEax6fvKeI88Qgfu2PNgHopMrqcPEbTAJftCmrX5en11VjgAvPY4WtY-elfl/pub?output=csv"

provincial <- read_csv(sheets_url)

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

reactable(
  provincial,
  defaultSorted = "percentage_fully_vaccinated",
  columns = list(
    province = colDef(
      name = "Province"
    ),
    total_adult_population = colDef(
      name = "Total adult pupolation",
      cell = function(value) {
        width <- paste0(value * 100 / max(provincial$total_adult_population), "%")
        value <- format(value, big.mark = ",")
        value <- format(value, width = 9, justify = "right")
        bar <- div(
          class = "bar-chart",
          style = list(marginRight = "6px"),
          div(class = "bar", style = list(width = width, backgroundColor = "#3fc1c9"))
        )
        div(class = "bar-cell", span(class = "number", value), bar)
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
      name = "Percentage vaccinated",
      align = "left",
      cell = function(value) {
        width <- paste0(value)
        bar_chart(value, width = width, fill = "#9dcce5", background = "#e1e1e1")
      }
    ),
    percentage_fully_vaccinated = colDef(
      defaultSortOrder = "desc",
      name = "Percentage fully vaccinated",
      align = "left",
      cell = function(value) {
        width <- paste0(value)
        bar_chart(value, width = width, fill = "#3392c5", background = "#e1e1e1")
      }
    )
  )
)
