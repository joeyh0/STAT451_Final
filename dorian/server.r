library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(maps)

#choose necessary columns
us_state_vaccinations = read.csv("data/us_state_vaccinations.csv")
us_state_vaccinations = us_state_vaccinations[, c('location', 'date', 'people_fully_vaccinated_per_hundred')]
us_map <- map_data("state")

#choose rows
us_state_vaccinations_late = us_state_vaccinations[us_state_vaccinations$date == "2023-05-10", ]

#remove states / regions not included in USA map
share_states = intersect(tolower(us_state_vaccinations_late$location), us_map$region)
us_state_vaccinations_late = us_state_vaccinations_late[tolower(us_state_vaccinations_late$location) %in% share_states, ]

#join data to include geographic data
map_df <- us_map %>%
  left_join(us_state_vaccinations_late %>% mutate(loc_lower = tolower(location)),
            by = join_by(region == loc_lower))

#construct map
geoplot <- ggplot(map_df, aes(long, lat, group = group, fill = people_fully_vaccinated_per_hundred)) +
  geom_polygon(color = "white", linewidth = 0.3) +  # Draw state borders
  coord_fixed(1.3) +                           # Keep aspect ratio
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    name = "GDP Per Capita (USD)",
    labels = scales::label_dollar(prefix = "$", big.mark = ","),
    breaks = c(50000, 75000, 100000),
    na.value = "gray80"
  ) +
  theme_minimal() +
  labs(
    title = "People Fully Vaccinated Per Hundred",
    subtitle = "Measured on June 10, 2023"
  )


nat_avgs <- data.frame(date = 0, location = "National Average", people_fully_vaccinated_per_hundred = 0)
us_state_vaccinations_rv = us_state_vaccinations[!is.na(us_state_vaccinations$people_fully_vaccinated_per_hundred), ]

for (time in unique(us_state_vaccinations_rv$date)){
  us_state_vaccinations_time = us_state_vaccinations_rv[us_state_vaccinations_rv$date == time, ]
  avg = mean(us_state_vaccinations_time$people_fully_vaccinated_per_hundred)
  nat_avg <- data.frame(date = time, location = "National Average", people_fully_vaccinated_per_hundred = mean(avg))
  nat_avgs <- rbind(nat_avgs, nat_avg)
  #us_state_vaccinations_wa <- rbind(us_state_vaccinations_wa, nat_avg)
} 

nat_avgs <- tail(nat_avgs, -1)

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
