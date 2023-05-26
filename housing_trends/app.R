library(shiny)
library(tidyverse)


#loads data, filters to relevant policy years
permit_data <- read.csv("permit_data.csv") %>%
  filter(year >= 2018)

#formats data as a year
permit_data$year <- as.Date(as.character(permit_data$year), format = "%Y")
permit_data$units <- as.numeric(permit_data$units)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Residential Permit Trends in Utah"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="type", label="Building type", c("sfd", 
                                                           "duplexes", 
                                                           "townhomes", 
                                                           "small_multiplexes",
                                                           "large_multifamily"),
                  
                  selected = "duplexes")
    ),
    
    mainPanel(
      plotOutput("linePlot")
    )
  )
)


server <- function(input, output) {
  filtered_data <- reactive(
    permit_data %>%
      filter(type == input$type)
  )
  
  output$linePlot <- renderPlot({
    ggplot(data = filtered_data(), mapping = aes(x = year, y = units)) + geom_line(colour="blue") + labs(title="Residential Units Permitted in Utah") + xlab("Year") + ylab("Units") + theme_classic() 
  })
}


shinyApp(ui = ui, server = server)