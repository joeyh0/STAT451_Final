library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(RCurl)
library(plotly)
library(readxl)
library(maps)
library(dplyr)


# Jaime Data Loading/Cleanup
x <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/refs/heads/master/public/data/cases_deaths/full_data.csv")
y <- read.csv(text = x)
data <- na.omit(y)

# Dorian Data Loading/Cleanup
us_state_vaccinations = read.csv("data/us_state_vaccinations.csv")
us_state_vaccinations = us_state_vaccinations[, c('location', 'date', 'people_fully_vaccinated_per_hundred')]


dashboardPage(
  dashboardHeader(title = "STAT 451 Final"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("GDP Barplots (Joey)", tabName = "gdp_barplot", icon = icon("chart-bar")),
      menuItem("US Map (Joey)", tabName = "usmap", icon = icon("map")),
      menuItem("U.S. States Vaccination Rates (Dorian)", tabName = "VaccinationByState", icon = icon("chart-line")),
      menuItem("Vaccination Rates (Hannah)", tabName = "idk1", icon = icon("chart-area")),
      menuItem("Global Deaths (Jaime)", tabName = "GlobalDeaths"),
      menuItem("Regional Deaths (Jaime)", tabName = "DeathsByRegion")
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
      # Dorian

      tabItem(tabName = "VaccinationByState",
              h2("U.S. State Vaccination Rates"),
              fluidRow(
                box(
                  title = "State Selection",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("state",
                              "Choose state:",
                              choices = us_state_vaccinations$location)
                )
              ),
              fluidRow(
                box(
                  title = "Vaccination Over Time",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("distPlot") 
                ),
                box(
                  title = "Vaccinations in June 2023",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("geoplot") 
                )
              )
      ),

      
      # Hannah
      tabItem(tabName = "idk1",
              h2("Vaccination Rates vs Healthcare Expenditure"),
              
              # InfoBoxes
              fluidRow(
                infoBoxOutput("infoCountries", width = 3),
                infoBoxOutput("infoCorr", width = 3),
                infoBoxOutput("infoMin", width = 3),
                infoBoxOutput("infoMax", width = 3)
              ),
              
              # Scatterplot
              fluidRow(
                column(
                  width = 12,
                  plotlyOutput("scatter", height = "500px"),
                  style = "margin-bottom: 40px;"
                )
              ),
              
              # Tables side-by-side
              fluidRow(
                box(
                  width = 6,
                  title = "High Income Countries",
                  tableOutput("table_high")
                ),
                box(
                  width = 6,
                  title = "Low Income Countries",
                  tableOutput("table_low")
                )
              )
      ),
      
      
      # Jaime
      tabItem(tabName = "GlobalDeaths",
              h2("Global Deaths"),
              p("There are two data options, new Covid-19 related deaths over time and total Covid-19 related deaths over time."),
              p("Both plots contain the approximate date of when vaccines were first beginning to be mass distributed as well as when the first
    booster vaccines became available (December 2020 and September 2021, respectively). (Information from the CDC)."),
              fluidRow(
                box(
                  title = "Plot Controls",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput(
                    inputId = "dropdown1",
                    label = "Choose a Data Option:",
                    choices = c("New Deaths", "Total Deaths"),
                    selected = "New Deaths"
                  )
                ),
                box(
                  title = "Plot Output",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  plotOutput("plot1") 
                )
              )
      ),
      
      tabItem(tabName = "DeathsByRegion",
              h2("Regional Deaths"),
              p("There are four data options, new and total Covid-19 related deaths, and new and total Covid-19 cases."),
              p("Select at least one region from the dropdown menu to begin plotting. Regions can be added or deleted at any time."),
              fluidRow(
                box(
                  title = "Plot Controls",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput(
                    inputId = "dropdown2",
                    label = "Choose a Data Option:",
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
                  title = "Plot Output",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  uiOutput("dynamic_output")
                )
              )
      )
      
    
    )
  )
)
