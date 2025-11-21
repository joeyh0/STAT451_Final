library(shiny)
library(tidyverse)
library(scales)
library(shinydashboard) 
library(readxl) 
library(maps)

# Joey Data Loading/Cleanup

gdp_data <- read_csv("data/Country_GDP.csv")
gdp_data_clean <- gdp_data %>%
  filter(country_name != "EGIPTO, REPUBLICA ARABE DE" & country_name != "ISLA DE SAN MARTIN (PARTE FRANCESA)" & !is.na(region) & country_name != "SANTO TOME Y PRINCIPE") %>%
  filter(year %in% c(2019, 2020)) %>%
  pivot_wider(id_cols = c(country_name), names_from = year, values_from = gdp_per_capita, names_prefix = "gdp_") %>%
  rename(gdp_2019 = gdp_2019, gdp_2020 = gdp_2020) %>%
  mutate(pct_change = ((gdp_2020 - gdp_2019) / gdp_2019) * 100) %>%
  filter(!is.na(pct_change))

state_gdp <- read_excel("data/State_GDP.xlsx")
colnames(state_gdp) <- c('State', 'GDP')

state_gdp <- state_gdp %>%
  mutate(region = tolower(State))

us_states <- map_data("state")

map_data_final <- us_states %>%
  left_join(state_gdp, by = "region") %>%
  filter(region != "district of columbia")

# Jaime Data Loading/Cleanup

x <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/refs/heads/master/public/data/cases_deaths/full_data.csv")
y <- read.csv(text = x)
data <- na.omit(y)

# Data for Global Plot (Plot 1)
world_data <- data %>% 
  group_by(date) %>% 
  select(date, new_cases, new_deaths, total_cases, total_deaths) %>% 
  summarise(new_cases = sum(new_cases),
            new_deaths = sum(new_deaths),
            total_cases = sum(total_cases),
            total_deaths = sum(total_deaths)) %>% 
  filter(new_cases != 0) 
world_data <- world_data[1:174,] # Assuming this is a deliberate row limit
world_data$date <- as.Date(world_data$date)

# Data for Regional Plot (Plot 2)
data_limit <- data[data$date <= "2023-05-14",]
country_data <- data_limit %>% 
  filter(new_cases != 0)
country_data$date <- as.Date(country_data$date)


# Hannah Data Loading/Cleanup

vax <- read_excel("data/wuenic2023rev_web-update.xlsx", sheet = "MCV1") %>%
  select(country, `2022`) %>%
  rename(Country = country,
         Immunization = `2022`)

exp <- read.csv("data/expenditure.csv", skip = 4) %>%
  select(Country = Country.Name, Expenditure = X2022)

inc <- read_excel("data/country_incomes.xlsx") %>%
  select(Country = Economy, Income = `Income group`) %>%
  filter(Income == "High income")

merged_data <- inc %>%
  inner_join(exp, by = "Country") %>%
  inner_join(vax, by = "Country") %>%
  drop_na()

shinyServer(function(input, output) {
  
  # Joey Plots
  
  output$barplots_menu_output <- renderMenu({
    menuItem("Countries by GDP change", 
             tabName = "barplots", 
             icon = icon("chart-bar"))
  })
  
  plot_data_params <- reactive({
    n <- input$num_countries
    if (grepl("Negatively Affected", input$plot_choice)) {
      data <- gdp_data_clean %>% arrange(pct_change) %>% head(n) %>%
        mutate(country_name = factor(country_name, levels = country_name))
      list(data = data, color = "#CC0000", label_prefix = "", label_hjust = -0.15, 
           label_color = "white", y_limits = c(min(data$pct_change) * 1.05, 0))
    } else {
      data <- gdp_data_clean %>% arrange(desc(pct_change)) %>% head(n) %>%
        mutate(country_name = factor(country_name, levels = rev(country_name)))
      list(data = data, color = "#0072B2", label_prefix = "+", label_hjust = 1.15, 
           label_color = "white", y_limits = c(0, max(data$pct_change) * 1.05))
    }
  })
  
  output$plot_title <- renderText({
    paste0("Top ", input$num_countries, " Countries ", input$plot_choice, ": Percent Change in GDP Per Capita (2019 to 2020)")
  })
  
  output$dynamic_bar_plot <- renderPlot({
    
    plot_list <- plot_data_params()
    data_to_plot <- plot_list$data
    p_color <- plot_list$color
    p_prefix <- plot_list$label_prefix
    p_hjust <- plot_list$label_hjust
    p_label_color <- plot_list$label_color
    p_y_limits <- plot_list$y_limits
    
    ggplot(data_to_plot, aes(x = country_name, y = pct_change)) +
      geom_col(fill = p_color) +
      geom_text(
        aes(label = paste0(p_prefix, round(pct_change, 1), "%")),
        hjust = p_hjust,
        size = 4,
        color = p_label_color
      ) +
      coord_flip() +
      labs(x = NULL, y = "Percent Change (%)") +
      scale_y_continuous(limits = p_y_limits) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$us_gdp_map <- renderPlot({ 
    ggplot(map_data_final, aes(x = long, y = lat, group = group, fill = GDP)) +
      geom_polygon(color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "mako",
        direction = -1,
        name = "GDP Per Capita (USD)",
        labels = scales::label_dollar(prefix = "$", big.mark = ","),
        breaks = c(50000, 75000, 100000),
        na.value = "gray80"
      ) +
      
      labs(
        title = "Mainland United States GDP Per Capita by State"
      ) +
      
      coord_map("albers", lat0 = 30, lat1 = 40) +
      
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  # Jaime Plots
  
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
  
  # Hannah Plots
  
  theData <- reactive({
    merged_data
  })
  
  thePoint <- reactive({
    df <- theData()
    if (nrow(df) > 0) df[1, ] else NULL
  })
  
  # Main scatterplot
  thePlot <- reactive({
    df <- theData()
    
    ggplot(df, aes(x = Expenditure, y = Immunization, text = Country)) +
      geom_point(color = "steelblue", size = 3) +
      ggtitle("MCV1 Coverage vs Health Expenditure in 2022") +
      xlab("Health Expenditure per Capita (USD)") +
      ylab("MCV1 Coverage (percent)")
  })
  
  output$scatter <- renderPlotly({
    ggplotly(thePlot(), tooltip = c("text", "x", "y"))
  })
  
  output$table <- renderTable({
    theData()
  })
  
  output$infoCountries <- renderInfoBox({
    df <- theData()
    infoBox("Countries", nrow(df), color = "blue")
  })
  
  output$infoCorr <- renderInfoBox({
    df <- theData()
    c <- cor(df$Expenditure, df$Immunization, use = "complete.obs")
    infoBox("Correlation", round(c, 3), color = "blue")
  })
  
  output$infoMin <- renderInfoBox({
    df <- theData()
    infoBox("Min MCV1", min(df$Immunization), color = "blue")
  })
  
  output$infoMax <- renderInfoBox({
    df <- theData()
    infoBox("Max MCV1", max(df$Immunization), color = "blue")
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Immunization_scatter", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 980, height = 400,
          units = "px", pointsize = 12,
          bg = "white", res = NA)
      print(thePlot())
      dev.off()
    },
    contentType = "image/png"
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "immunization_data.csv"
    },
    content = function(file) {
      write.csv(theData(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$plot_clickedpoints <- renderTable({
    df <- theData()
    res <- nearPoints(df,
                      input$plot_click,
                      "Expenditure",
                      "Immunization")
    if (nrow(res) == 0)
      return()
    res
  })
  
  output$plot_hoverinfo <- renderPrint({
    cat("Hover (throttled):\n")
    str(input$plot_hover)
  })
  
  
})
