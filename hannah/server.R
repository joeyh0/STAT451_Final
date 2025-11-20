library(tidyverse)
library(readxl)
library(plotly)

function(input, output) {
  
  # Load data sets when buttn is clicked
  theData <- reactive({
    req(input$run)
    
    vax <- read_excel("wuenic2023rev_web-update.xlsx", sheet = "MCV1") %>%
      select(country, `2022`) %>%
      rename(Country = country,
             Immunization = `2022`)
    
    exp <- read.csv("expenditure.csv", skip = 4) %>%
      select(Country = Country.Name, Expenditure = X2022)
    
    inc <- read_excel("country_incomes.xlsx") %>%
      select(Country = Economy, Income = `Income group`) %>%
      filter(Income == "High income")
    
    merged <- inc %>%
      inner_join(exp, by = "Country") %>%
      inner_join(vax, by = "Country") %>%
      drop_na()
    
    merged
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
  
}
