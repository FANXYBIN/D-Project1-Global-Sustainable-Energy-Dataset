library(shiny)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(dplyr)
library(DT)

###############################################################################

# Clean the dataset

# Load the dataset
data <- read.csv("~/Downloads/global-data-on-sustainable-energy.csv")

# Fill NA values with 0 in the specified columns
data$Renewables....equivalent.primary.energy.[is.na(data$Renewables....equivalent.primary.energy.)] <- 0
data$Renewable.electricity.generating.capacity.per.capita[is.na(data$Renewable.electricity.generating.capacity.per.capita)] <- 0

# Fill the remaining NA values with the average value according to "Entity"
data <- data %>%
  group_by(Entity) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Replace any remaining NaN values with 0
data[is.na(data)] <- 0

###############################################################################

# Change slider color
setSliderColor <- function(color, sliderId) {
  
  # some tests to control inputs
  stopifnot(!is.null(color))
  stopifnot(is.character(color))
  stopifnot(is.numeric(sliderId))
  stopifnot(!is.null(sliderId))
  
  # the css class for ionrangeslider starts from 0
  # therefore need to remove 1 from sliderId
  sliderId <- sliderId - 1
  
  # create custom css background for each slider
  # selected by the user
  sliderCol <- lapply(sliderId, FUN = function(i) {
    paste0(
      ".js-irs-", i, " .irs-single,",
      " .js-irs-", i, " .irs-from,",
      " .js-irs-", i, " .irs-to,",
      " .js-irs-", i, " .irs-bar-edge,",
      " .js-irs-", i,
      " .irs-bar{  border-color: transparent;background: ", color[i+1],
      "; border-top: 1px solid ", color[i+1],
      "; border-bottom: 1px solid ", color[i+1],
      ";}"
    )
  })
  
  # insert this custom css code in the head
  # of the shiy app
  custom_head <- tags$head(tags$style(HTML(as.character(sliderCol))))
  return(custom_head)
}

###############################################################################

# Define UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Global Sustainable Energy Dashboard", titleWidth = 350),
  dashboardSidebar(
    setSliderColor(c("green"), sliderId = 1),
    sliderInput("year", "Select Year:", 
                min = min(data$Year), max = max(data$Year),
                value = c(min(data$Year), max(data$Year)),
                step = 1, sep = ""),
    selectInput("country", "Select Country:", choices = unique(data$Entity)),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Raw data", tabName = "rawdata", icon = icon("th"))
    )),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(title = "Renewable Electricity Generating Capacity", status = "success", solidHeader = TRUE,
                    collapsible = TRUE, plotOutput("renewablePlot", height = 300)),
                box(title = "Electricity Generation", status = "success", solidHeader = TRUE,
                    collapsible = TRUE, plotOutput("generationPlot", height = 300))
              ),
              fluidRow(
                box(title = "CO2 Emissions vs Low-Carbon Electricity", status = "success", solidHeader = TRUE,
                    collapsible = TRUE, plotOutput("CO2Plot", height = 300)),
                box(title = "Top 10 Countries by CO2 Emissions", status = "success", solidHeader = TRUE,
                    collapsible = TRUE, plotOutput("topCO2Plot", height = 300))
              )
      ),
      tabItem("rawdata",
              dataTableOutput("dataTable"),
              verbatimTextOutput("rawtable")
      )
    )
  )
)


###############################################################################

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(Entity == input$country,
             Year >= input$year[1], Year <= input$year[2])
  })
  
  top_co2_data <- reactive({
    data %>%
      filter(Year == max(input$year)) %>%
      group_by(Entity) %>%
      summarize(total_CO2 = sum(Value_co2_emissions_kt_by_country, na.rm = TRUE)) %>%
      arrange(desc(total_CO2)) %>%
      head(10)
  })
  
  output$renewablePlot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Renewable.electricity.generating.capacity.per.capita), stat = "identity") +
      labs(y = "Renewable Capacity per Capita") +
      theme_minimal()
  })
  
  output$generationPlot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = Year)) +
      geom_bar(aes(y = Electricity.from.fossil.fuels..TWh., fill = "Fossil Fuels"), stat = "identity") +
      geom_bar(aes(y = Electricity.from.nuclear..TWh., fill = "Nuclear"), stat = "identity") +
      geom_bar(aes(y = Electricity.from.renewables..TWh., fill = "Renewables"), stat = "identity") +
      labs(y = "Electricity Generation (TWh)", fill = "Sources") +
      theme_minimal()
  })
  
  output$CO2Plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = Low.carbon.electricity....electricity.,
                   y = Value_co2_emissions_kt_by_country)) +
      geom_point() +
      stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
      labs(x = "Low-carbon electricity (% electricity)", y = "CO2 Emissions (kt)") +
      theme_minimal()
  })
  output$topCO2Plot <- renderPlot({
    df <- top_co2_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = reorder(Entity, total_CO2), y = total_CO2)) +
      geom_bar(stat = "identity", fill = "springgreen4") +
      coord_flip() +
      labs(x = "Country", y = "Total CO2 Emissions (kt)") +
      theme_minimal()
  })
  output$dataTable <- renderDataTable({
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })  
}

###############################################################################

# Run the application
shinyApp(ui = ui, server = server)
