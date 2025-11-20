# group ui :/
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "States vs. National Average: People Fully Vaccinated per Hundred",
                  titleWidth = 650),

  #choose number of states
  dashboardSidebar(
    selectInput("state",
                "Choose state:",
                choices = us_state_vaccinations$location)
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(
    tabItems(
      tabItem(tabName = "State Vaccinations",
        plotOutput("distPlot"),
        plotOutput("geoplot")
      )
    )
  )
)
