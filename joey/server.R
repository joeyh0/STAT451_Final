library(shiny)
library(tidyverse)
library(scales)
library(shinydashboard) 
library(readxl) 
library(maps) 

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

shinyServer(function(input, output) {
  
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
})
