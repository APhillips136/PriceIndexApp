install.packages("readxl")
install.packages("tidyverse")
install.packages("shiny")
install.packages("plotly")
install.packages("ggplot2")
install.packages("shinyGovstyle")
install.packages("data.table")
install.packages("magrittr")
install.packages("aws.ec2metadata")
install.packages("aws.s3")
install.packages("readr")
install.packages("dplyr")
install.packages("stringr")
install.packages("scales")
install.packages("shinyBS")
install.packages("htmlwidgets")
install.packages("openxlsx")


library(readxl)
library(tidyverse)
library(shiny)
library(plotly)
library(ggplot2)
library(shinyGovstyle)
library(shinyWidgets)
library(data.table)
library(magrittr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(dplyr)
library(stringr)
library(scales)
library(shinyBS)
library(htmlwidgets)
library(openxlsx)

#load in and save the data
Jul_PPI <- aws.s3::s3read_using(FUN=readxl::read_excel,object="PriceIndexApp/240729_PPI_Comparisons.xlsx",sheet = "data",bucket = "s3-ranch-004")

# Reshape the data from wide format to long format
Jul_PPI_long <- Jul_PPI %>%
  pivot_longer(
    cols = -Year,
    names_to = "Index",
    values_to = "Value"
  )

# Drop the "2015 = 100" from the end of the Index variables
Jul_PPI_long <- Jul_PPI_long %>%
  mutate(Index = str_replace(Index, " 2015=100$", ""))

#Seperate out index variable
Jul_PPI_long <- Jul_PPI_long %>%
  separate(Index, into = c("index_type", "index"), sep = " - ")

# Handle cases where there is no hyphen
Jul_PPI_long$index_type[is.na(Jul_PPI_long$index_type)] <- ""
Jul_PPI_long$index[is.na(Jul_PPI_long$index)] <- Jul_PPI_long$index_type[is.na(Jul_PPI_long$index)]

# Replace Index_Type with "Aggregate Index" where Index_Type is equal to Index
Jul_PPI_long$index_type[Jul_PPI_long$index_type == Jul_PPI_long$index] <- "Aggregate Index"

# Replace Index_Type with "OBR Price Index" for specific Index values
obr_price_indices <- c("RPI", "RPIX", "CPI", "Actual rents for housing", "Consumer expenditure deflator", "GDP deflator")
Jul_PPI_long$index_type[Jul_PPI_long$index %in% obr_price_indices] <- "OBR Price Index"

# Create the Shiny app
ui <- fluidPage(
  titlePanel("PPI Comparison"),
  sidebarLayout(
    sidebarPanel(
      h4("Select/Deselect Indices"),
      uiOutput("checkboxGroup")
    ),
    mainPanel(
      plotlyOutput("linePlot")
    )
  )
)

server <- function(input, output, session) {
  # Generate checkbox group inputs based on Index_Type
  output$checkboxGroup <- renderUI({
    checkboxGroupInput("selected_indices", "Indices:",
                       choices = unique(Jul_PPI_long$index),
                       selected = unique(Jul_PPI_long$index_type == "OBR Price Index"))
  })
  
  # Render the plotly line graph
  output$linePlot <- renderPlotly({
    filtered_data <- Jul_PPI_long %>%
      filter(index %in% input$selected_indices)
    
    p <- plot_ly(filtered_data, x = ~Year, y = ~Value, color = ~index, type = 'scatter', mode = 'lines') %>%
      layout(title = 'PPI Comparison Over Time',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Index 2015 = 100'))
    
    p
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)