# group ui :/
# i am writing a bunch of bullshit yaaaaaaaaaaaay
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "States vs. National Average: People Fully Vaccinated per Hundred",
                  titleWidth = 650),

  #choose number of states
  dashboardSidebar(
    sidebarMenu(
      menuItem("State Vaccinations", tabName = "State_Vaccinations")
    )
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(
    tabItems(
      tabItem(tabName = "State_Vaccinations",
        fluidRow(
          box(
            selectInput("state", "Choose state:", choices = us_state_vaccinations$location)
          )
          box(plotOutput("distPlot")),
          box(plotOutput("geoplot"))
        )
      )
    )
  )
)
