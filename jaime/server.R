library(tidyverse)
library(RCurl)
library(scales)

function(input, output){

  # GET THE DATA FOR PLOTS LATER ON
  
  x <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/refs/heads/master/public/data/cases_deaths/full_data.csv")
  y <- read.csv(text = x)
  data <- na.omit(y)
  
  world_data <- data %>% 
    group_by(date) %>% 
    select(date, new_cases, new_deaths, total_cases, total_deaths) %>% 
    summarise(new_cases = sum(new_cases),
              new_deaths = sum(new_deaths),
              total_cases = sum(total_cases),
              total_deaths = sum(total_deaths)) %>% 
    filter(new_cases != 0) 
  world_data <- world_data[1:174,]
  
  world_data$date <- as.Date(world_data$date)
  
  data_limit <- data[data$date <= "2023-05-14",]
  
  country_data <- data_limit %>% 
    filter(new_cases != 0)
  country_data$date <- as.Date(country_data$date)
  
  # PLOT 1 = line plot for global deaths/cases
  output$plot1 = renderPlot({
    # get plot, if else 
    if (input$dropdown1 == "Total Deaths") {
      ggplot(data = world_data, aes(x = date, y = total_deaths, group = 1)) +
        geom_rect(aes(xmin = as.Date("2020-12-20"), 
                      xmax = as.Date("2021-09-19"), 
                      ymin = -Inf, 
                      ymax = Inf),
                  fill = "beige", alpha = 0.1, inherit.aes = FALSE) +
        geom_rect(aes(xmin = as.Date("2021-09-20"), 
                      xmax = as.Date("2023-05-14"), 
                      ymin = -Inf, 
                      ymax = Inf),
                  fill = "lightblue", alpha = 0.01, inherit.aes = FALSE) +
        geom_line() +
        scale_x_date(date_breaks = "1 year",    # show one tick per year
                     date_labels = "%Y") +
        labs(title = "Total Covid-19 Related Deaths Over Time",
             y = "Total Global Deaths",
             x = "Year") +
        theme_bw() +
        scale_y_continuous(
          breaks = c(0, 1e7, 2e7, 3e7),           
          labels = c("0", "10M", "20M", "30M")      
        ) +
        geom_vline(xintercept = as.Date("2020-12-20"), linetype = "dashed", col = "blue")+
        geom_vline(xintercept = as.Date("2021-09-19"), linetype = "dashed", col = "blue") +
        geom_text(x = as.Date("2021-04-01"), y = 0, 
                  label = "First Available \nVaccines", 
                  vjust = -1,
                  size = 4,
                  col = "darkblue") +
        geom_text(x = as.Date("2022-01-15"), y = 0, 
                  label = "Booster Shots\n Become Available", 
                  vjust = -1,
                  size = 4,
                  col = "darkblue")}
    else {
      ggplot(data = world_data, aes(x = date, y = new_deaths, group = 1)) +
        geom_rect(aes(xmin = as.Date("2020-12-20"), 
                      xmax = as.Date("2021-09-19"), 
                      ymin = -Inf, 
                      ymax = Inf),
                  fill = "beige", alpha = 0.1, inherit.aes = FALSE) +
        geom_rect(aes(xmin = as.Date("2021-09-20"), 
                      xmax = as.Date("2023-05-14"), 
                      ymin = -Inf, 
                      ymax = Inf),
                  fill = "lightblue", alpha = 0.01, inherit.aes = FALSE) +
        geom_line() +
        scale_x_date(date_breaks = "1 year",    # show one tick per year
                     date_labels = "%Y") +
        labs(title = "New Covid-19 Related Deaths Over Time",
             y = "New Deaths",
             x = "Year") +
        theme_bw() +
        geom_vline(xintercept = as.Date("2020-12-20"), linetype = "dashed", col = "blue")+
        geom_vline(xintercept = as.Date("2021-09-19"), linetype = "dashed", col = "blue") +
        geom_text(x = as.Date("2021-04-01"), y = 0, 
                  label = "First Available \nVaccines", 
                  vjust = 0,
                  size = 4,
                  col = "darkblue") +
        geom_text(x = as.Date("2022-01-15"), y = 0, 
                  label = "Booster Shots\n Become Available", 
                  vjust = 0,
                  size = 4,
                  col = "darkblue") +
        scale_y_continuous(
          breaks = c(0, 1e5, 2e5, 3e5, 4e5),           
          labels = c("0", "100,000", "200,000", "300,000", "400,000")   
        )} 
    
    
  })

  output$plot2 = renderPlot({
    col_name <- "total_deaths"
    if(input$dropdown2 == "Total Deaths") {
      col_name <- "total_deaths"}
    else if (input$dropdown2 == "New Deaths") {
      col_name <- "new_deaths"}
    else if (input$dropdown2 == "Total Cases") {
      col_name <- "total_cases"}
    else {col_name <- "new_cases"}
    
    new_data <- country_data[country_data$location %in% input$selectRegions,]
    ggplot(data = new_data, aes(x = date, 
                                y = new_data[,col_name],
                                group = location, 
                                color = location)) +
      geom_line() +
      theme_bw() +
      labs(title = paste("Covid-19", input$dropdown2),
           y = input$dropdown2,
           x = "Year",
           color = "Region") +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y") +
      scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1))
    
  })
  
  output$dynamic_output <- renderUI({
    
    if (length(input$selectRegions) > 0) {
      plotOutput("plot2")  # show a plot
    } else {
      div("Please select at least one region from the dropdown menu to begin plotting for Plot 2.")  # show text
    }
  })
  }
