library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)

dashboardPage(
  dashboardHeader(title = "COVID-19 Global GDP Impact"),
  dashboardSidebar(
    sidebarMenu(
      menuItemOutput("barplots_menu_output"), 
      menuItem("US Map", tabName = "usmap", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "barplots", 
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
      )
    )
  )
)
