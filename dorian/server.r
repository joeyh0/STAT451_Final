library(shiny)
library(shinydashboard)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    us_state_vaccinations_filter = us_state_vaccinations[us_state_vaccinations$location == input$state, ]
    us_state_vaccinations_filter = us_state_vaccinations_filter[! is.na(us_state_vaccinations_filter$people_fully_vaccinated_per_hundred), ]
    us_state_vaccinations_filter = rbind(us_state_vaccinations_filter, nat_avgs)
    
    ggplot(us_state_vaccinations_filter, aes(x = as.Date(date), y = people_fully_vaccinated_per_hundred, color = location)) +
      geom_line(size = 1.2) +
      labs(title = "People Fully Vaccinated per Hundred as Compared \nWith the National Average", x = "Date", y = "People Per Hundred") +
      theme_minimal()
  })
  
  output$geoplot <- renderPlot({
    geoplot
  })
}
