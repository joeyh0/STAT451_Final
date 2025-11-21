library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(RCurl)
library(plotly)
library(readxl)
library(maps)


# Jaime Data Loading/Cleanup
x <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/refs/heads/master/public/data/cases_deaths/full_data.csv")
y <- read.csv(text = x)
data <- na.omit(y)


dashboardPage(
  dashboardHeader(title = "COVID-19 Global GDP Impact"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("GDP Barplots (Joey)", tabName = "gdp_barplot", icon = icon("chart-bar")),
      menuItem("US Map (Joey)", tabName = "usmap", icon = icon("map")),
      menuItem("Jaime #1", tabName = "GlobalDeaths"),
      menuItem("Jaime #2", tabName = "DeathsByRegion"),
      # menuItem("Dorian", tabName = "VaccinationByState"),
      menuItem("Hannah#1", tabName = "idk1", icon = icon("chart-area")),
      menuItem("Hannah#2", tabName = "idk2", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Joey
      tabItem(tabName = "gdp_barplot", 
              h2("Percent Change in GDP Per Capita (2019 to 2020)"),
              fluidRow(
                box(
                  title = "Plot Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  sliderInput("num_countries", 
                              "Number of Countries to Display (Top N):",
                              min = 5, 
                              max = 30, 
                              value = 15, 
                              step = 1), 
                  selectInput("plot_choice",
                              "Choose Visualization:",
                              choices = c("Most Negatively Affected",
                                          "Most Benefited"),
                              selected = "Most Negatively Affected")
                ),
                box(
                  title = textOutput("plot_title"), 
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("dynamic_bar_plot") 
                )
              )
      ),
      
      tabItem(tabName = "usmap",
              h2("Mainland United States GDP Per Capita"),
              plotOutput("us_gdp_map")
      ),
      
      # Jaime
      tabItem(tabName = "GlobalDeaths",
              h2("Global COVID-19 Deaths Over Time"),
              fluidRow(
                box(
                  title = "Plot Controls",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput(
                    inputId = "dropdown1",
                    label = "Choose a data option:",
                    choices = c("New Deaths", "Total Deaths"),
                    selected = "New Deaths"
                  )
                ),
                box(
                  title = "Global Data Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  plotOutput("plot1") 
                )
              )
      ),
      
      tabItem(tabName = "DeathsByRegion",
              h2("Regional COVID-19 Data Comparison"),
              fluidRow(
                box(
                  title = "Plot Controls",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput(
                    inputId = "dropdown2",
                    label = "Choose a data option:",
                    choices = c("Total Deaths", "Total Cases", "New Deaths", "New Cases"),
                    selected = "Total Deaths"
                  ),
                  selectInput(
                    inputId = "selectRegions",
                    label = "Select Regions:",
                    choices = unique(data$location),
                    multiple = TRUE, 
                    selectize = TRUE
                  )
                ),
                box(
                  title = "Regional Data Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  uiOutput("dynamic_output")
                )
              )
      ),
      
      # Dorian
      # 
      # tabItem(tabName = "VaccinationByState",
      #         h2("Placeholder Title Text"),
      #         fluidRow(
      #           box(
      #           selectInput("state",
      #                       "Choose state:",
      #                       choices = us_state_vaccinations$location)
      #           )
      #         ),
      #         plotOutput("distPlot"),
      #         plotOutput("geoplot")    
      # ),
      # 
      
      # Hannah
      tabItem(tabName = "idk1",
              h2("Placeholder Title Text"),
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
      
      tabItem(tabName = "idk2",
              h2("Placeholder Title Text"),
              fluidRow(
                box(width = 12, tableOutput("table"))
              )
      )
    )
  )
)
