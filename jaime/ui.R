library(tidyverse)
library(RCurl)
library(scales)

x <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/refs/heads/master/public/data/cases_deaths/full_data.csv")
y <- read.csv(text = x)
data <- na.omit(y)

fluidPage(
  
  p(""),
  p("Jaime Li, Stat 451, AUTUMN 2025"),
  titlePanel("Visualization of COVID-19 Death-Related Data"),
  p("To view the dataset used in this webpage, please visit: "),
  p("https://github.com/owid/covid-19-data/tree/master/public/data/cases_deaths/full_data.csv"),
  h3("Plot Descriptions:"),
  p("In the first plot, there are two options, 1) new deaths and 2) total deaths over time. 
    Both plots contain the approximate date of when vaccines were first beginning to be mass distributed as well as when the first
    booster vaccines became available (December 2020 and September 2021, respectively.)"),
  p("The second plot has four data options (all over time): 1) total deaths, 2) new deaths, 3) total cases, and 4) new cases. 
    Any number of regions can be added or deleted from the visualization at any point. The plot will not show until at least one
    region is selected from the drop down menu."),

  sidebarLayout(

    sidebarPanel(
      h3("Global Data"),
      selectInput(
        inputId = "dropdown1",
        label = "Chose a data option:",
        choices = c("New Deaths", "Total Deaths"),
        selected = "New Deaths" # Initially selected value
      ),
      
      h3("Regional Data"),
      selectInput(
        inputId = "dropdown2",
        label = "Chose a data option:",
        choices = c("Total Deaths", "Total Cases", "New Deaths", "New Cases"),
        selected = "Total Deaths" # Initially selected value
      ),
      # input countries/regions wanted
      selectInput(
        inputId = "selectRegions",
        label = "Select Regions:",
        choices = unique(data$location),
        multiple = TRUE, # This enables multiple selections
        selectize = TRUE # This enhances the dropdown with search and checkbox-like appearance
      )
    ),
    
    

    mainPanel(
      h3("Global Data"),
      plotOutput("plot1"),
      h3("Regional Data"),
      uiOutput("dynamic_output")
    )
  )
)
