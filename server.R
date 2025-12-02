library(shiny)
library(tidyverse)
library(scales)
library(shinydashboard) 
library(readxl) 
library(maps)
library(RCurl)
library(plotly)

# Joey Data Loading/Cleanup

gdp_data <- read_csv("data/Country_GDP.csv")
gdp_data_clean <- gdp_data %>%
  filter(country_name != "EGIPTO, REPUBLICA ARABE DE" & 
           country_name != "ISLA DE SAN MARTIN (PARTE FRANCESA)" & 
           !is.na(region) & 
           country_name != "SANTO TOME Y PRINCIPE") %>%
  filter(year %in% c(2019, 2020)) %>%
  pivot_wider(id_cols = c(country_name), 
              names_from = year, 
              values_from = gdp_per_capita, 
              names_prefix = "gdp_") %>%
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

# Global data
world_data <- data %>%
  group_by(date) %>%
  summarise(
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths),
    total_cases = sum(total_cases),
    total_deaths = sum(total_deaths)
  ) %>%
  filter(new_cases != 0)

world_data <- world_data[1:174,]
world_data$date <- as.Date(world_data$date)

# Regional plot data
data_limit <- data[data$date <= "2023-05-14",]
country_data <- data_limit %>% filter(new_cases != 0)
country_data$date <- as.Date(country_data$date)

# Hannah data

vax <- read_excel("data/wuenic2023rev_web-update.xlsx", sheet = "MCV1") %>%
  select(country, `2022`) %>%
  rename(Country = country, Immunization = `2022`)

exp <- read.csv("data/expenditure.csv", skip = 4) %>%
  select(Country = Country.Name, Expenditure = X2022)

inc <- read_excel("data/country_incomes.xlsx") %>%
  select(Country = Economy, Income = `Income group`) %>%
  filter(Income == "High income")

inc_low <- read_excel("data/country_incomes.xlsx") %>%
  select(Country = Economy, Income = `Income group`) %>%
  filter(Income == "Low income")

merged_data <- inc %>%
  inner_join(exp, by = "Country") %>%
  inner_join(vax, by = "Country") %>%
  drop_na()

merged_data_low <- inc_low %>%
  inner_join(exp, by = "Country") %>%
  inner_join(vax, by = "Country") %>%
  drop_na()

# Dorian Data Loading

us_state_vaccinations = read.csv("data/us_state_vaccinations.csv")
us_state_vaccinations = us_state_vaccinations[, c('location', 'date', 'people_fully_vaccinated_per_hundred')]
us_map <- map_data("state")
us_state_vaccinations = us_state_vaccinations[!is.na(us_state_vaccinations$people_fully_vaccinated_per_hundred), ]

us_state_vaccinations_late = us_state_vaccinations[us_state_vaccinations$date == "2023-05-10", ]
us_state_vaccinations_late["34229", "location"] = "New York"
share_states = intersect(tolower(us_state_vaccinations_late$location), us_map$region)
us_state_vaccinations_late = us_state_vaccinations_late[tolower(us_state_vaccinations_late$location) %in% share_states, ]

map_df <- us_map %>%
  left_join(us_state_vaccinations_late %>% mutate(loc_lower = tolower(location)),
            by = join_by(region == loc_lower))

geoplot <- ggplot(map_df, aes(long, lat, group = group, fill = people_fully_vaccinated_per_hundred)) +
  geom_polygon(color = "white", linewidth = 0.3) +
  coord_fixed(1.3) +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "Value") +
  theme_minimal() +
  labs(title = "People Fully Vaccinated Per Hundred",
       subtitle = "Measured on June 10, 2023")

us_state_vaccinations_rv = us_state_vaccinations
nat_avgs <- us_state_vaccinations_rv %>%
  group_by(date) %>%
  summarise(people_fully_vaccinated_per_hundred = mean(people_fully_vaccinated_per_hundred, na.rm = TRUE)) %>%
  mutate(location = "National Average", date = as.character(date)) %>%
  select(date, location, people_fully_vaccinated_per_hundred)

# SERVER

shinyServer(function(input, output) {

  # Joey -----------------------------------------------------------------------
  
  output$barplots_menu_output <- renderMenu({
    menuItem("Countries by GDP change", tabName = "barplots", icon = icon("chart-bar"))
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
    paste0("Top ", input$num_countries, " Countries ", input$plot_choice, 
           ": Percent Change in GDP Per Capita (2019 to 2020)")
  })

  output$dynamic_bar_plot <- renderPlot({
    plot_list <- plot_data_params()
    data_to_plot <- plot_list$data
  
    ggplot(data_to_plot, aes(x = country_name, y = pct_change)) +
      geom_col(fill = plot_list$color) +
      geom_text(aes(label = paste0(plot_list$label_prefix, round(pct_change, 1), "%")),
                hjust = plot_list$label_hjust, size = 4, color = plot_list$label_color) +
      coord_flip() +
      labs(x = NULL, y = "Percent Change (%)") +
      scale_y_continuous(limits = plot_list$y_limits) +
      theme_minimal()
  })

  output$us_gdp_map <- renderPlot({
    ggplot(map_data_final, aes(x = long, y = lat, group = group, fill = GDP)) +
      geom_polygon(color = "white", size = 0.1) +
      scale_fill_viridis_c(option = "mako", direction = -1, name = "GDP Per Capita (USD)",
                           labels = label_dollar(prefix = "$", big.mark = ",")) +
      labs(title = "Mainland United States GDP Per Capita by State") +
      coord_map("albers", lat0 = 30, lat1 = 40) +
      theme_void()
  })

  # Jaime ----------------------------------------------------------------------

  output$plot1 = renderPlot({
    if (input$dropdown1 == "Total Deaths") {
      ggplot(world_data, aes(x = date, y = total_deaths)) +
        geom_rect(aes(xmin = as.Date("2020-12-20"), xmax = as.Date("2021-09-19"),
                      ymin = -Inf, ymax = Inf),
                  fill = "beige", alpha = 0.1, inherit.aes = FALSE) +
        geom_line() +
        theme_bw()
    } else {
      ggplot(world_data, aes(x = date, y = new_deaths)) +
        geom_rect(aes(xmin = as.Date("2020-12-20"), xmax = as.Date("2021-09-19"),
                      ymin = -Inf, ymax = Inf),
                  fill = "beige", alpha = 0.1, inherit.aes = FALSE) +
        geom_line() +
        theme_bw()
    }
  })

  output$plot2 = renderPlot({
    colmap <- c(
      "Total Deaths" = "total_deaths",
      "New Deaths" = "new_deaths",
      "Total Cases" = "total_cases",
      "New Cases" = "new_cases"
    )
    col_name <- colmap[[input$dropdown2]]

    new_data <- country_data[country_data$location %in% input$selectRegions,]

    ggplot(new_data, aes(x = date, y = .data[[col_name]], 
                         group = location, color = location)) +
      geom_line(size = 1.25) +
      theme_bw()
  })

  output$dynamic_output <- renderUI({
    if (length(input$selectRegions) > 0) {
      plotOutput("plot2")
    } else {
      div("Please select at least one region.")
    }
  })

  # Dorian ---------------------------------------------------------------------

  output$distPlot <- renderPlot({
    us_state_vaccinations_filter = us_state_vaccinations[
      us_state_vaccinations$location == input$state, ]
    us_state_vaccinations_filter = us_state_vaccinations_filter[
      !is.na(us_state_vaccinations_filter$people_fully_vaccinated_per_hundred), ]
    us_state_vaccinations_filter = rbind(us_state_vaccinations_filter, nat_avgs)

    ggplot(us_state_vaccinations_filter, 
           aes(x = as.Date(date), 
               y = people_fully_vaccinated_per_hundred, 
               color = location)) +
      geom_line(size = 1.2) +
      theme_minimal()
  })

  output$geoplot <- renderPlot({ geoplot })

  # Hannah ---------------------------------------------------------------------

  theData <- reactive({ merged_data })

  thePoint <- reactive({
    df <- theData()
    if (nrow(df) > 0) df[1, ] else NULL
  })

  # Combine both income groups
  df_combined <- reactive({
    bind_rows(
      merged_data  %>% mutate(IncomeGroup = "High income"),
      merged_data_low %>% mutate(IncomeGroup = "Low income")
    )
  })

  # Scatter plot with working legend fix
  thePlot <- reactive({
    df <- df_combined()

    ggplot(df, aes(
      x = Expenditure,
      y = Immunization,
      color = IncomeGroup,
      text = Country
    )) +
      geom_point(size = 3, alpha = 1) +
      scale_color_manual(values = c(
        "High income" = "steelblue",
        "Low income" = "red"
      )) +
      guides(color = guide_legend(override.aes = list(size = 6))) +
      ggtitle("COVID Immunization Coverage vs Health Expenditure (2022)") +
      xlab("Health Expenditure per Capita (USD)") +
      ylab("Vaccination Coverage (%)") +
      theme_minimal(base_size = 14)
  })

  output$scatter <- renderPlotly({
    p <- thePlot()

    ggplotly(
      p,
      tooltip = c("text", "x", "y"),
      dynamicTicks = TRUE,
      originalData = TRUE
    ) %>%
      layout(
        legend = list(
          title = list(text = "Income Group"),
          x = 1,
          y = 1
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        xaxis = list(showgrid = FALSE),
        yaxis = list(showgrid = FALSE)
      )
  })

  output$table <- renderTable({ theData() })

  output$infoCountries <- renderInfoBox({
    df <- theData()
    infoBox("Countries", nrow(df), color = "blue")
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Immunization_scatter", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 980, height = 400)
      print(thePlot())
      dev.off()
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() { "immunization_data.csv" },
    content = function(file) {
      write.csv(theData(), file, row.names = FALSE)
    }
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

  output$table_high <- renderTable({ merged_data })
  output$table_low  <- renderTable({ merged_data_low })

})

