library(shiny)
library(tidyverse)
load("data/movies.rdata")

# Define UI
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      # Select variable for y-axis
      selectInput(inputId = "y", label = "Y-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "audience_score"),
      # Select variable for x-axis
      selectInput(inputId = "x", label = "X-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "critics_score"),
      sliderInput(inputId = "alpha",
                  label = "Alpha:",
                  min = 0, max = 1,
                  value = 0.5),
      radioButtons(inputId = "colorby",
                   label = "ColorBy",
                   choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                   selected = "imdb_num_votes",
                   inline = FALSE),
      checkboxInput(inputId = "displaydf",
                    label = "DisplayDF",
                    value = FALSE,
                    width = '100%'),
      # Select which types of movies to plot
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select movie type(s):",
                         choices = c("Documentary", "Feature Film",
                                     "TV Movie"),
                         selected = "Feature Film")
      ),
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      uiOutput(outputId = "n"),
      
      conditionalPanel(
        condition = "input.displaydf == true",
        DT::dataTableOutput("mytable")
      )
      
    )
  )
)

  # Define server function
  server <- function(input, output) {
    
    # Create the scatterplot object the plotOutput function is expecting
    output$scatterplot <- renderPlot({
      ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$colorby)) +
        geom_point(alpha = input$alpha) 
    })
    
    output$mytable <- DT::renderDataTable({movies})

    # Create a subset of data filtering for chosen title types
    movies_subset <- reactive({
      req(input$selected_type)
      filter(movies, title_type %in% input$selected_type)
    })
    
    output$n <- renderUI({
      types <- movies_subset()$title_type %>%
        factor(levels = input$selected_type)
      counts <- table(types)
      
      HTML(paste("There are",
                 counts,
                 input$selected_type,
                 "movies in this dataset.
 <br>"))
    })
    
  }

shinyApp(ui = ui, server = server)

