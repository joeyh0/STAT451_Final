library(shiny)
library(shinydashboard)
library(plotly)

header <- dashboardHeader(
  title = "Global Immunization Dashboard"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Scatterplot",
             tabName = "scatter_tab",
             icon = icon("chart-scatter")),
    menuItem("Data Table",
             tabName = "table_tab",
             icon = icon("table")),
    actionButton("run", "Load Data")
  )
)

body <- dashboardBody(
  tabItems(
    
    tabItem(
      tabName = "scatter_tab",
      fluidRow(
        infoBoxOutput("infoCountries", width = 3),
        infoBoxOutput("infoCorr", width = 3),
        infoBoxOutput("infoMin", width = 3),
        infoBoxOutput("infoMax", width = 3)
      ),
      fluidRow(
        box(width = 12, plotlyOutput("scatter"))
      )
    ),
    
    tabItem(
      tabName = "table_tab",
      fluidRow(
        box(width = 12, tableOutput("table"))
      )
    )
  )
)

dashboardPage(header, sidebar, body)
