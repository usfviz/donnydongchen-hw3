rm(list = ls())
cat("\014") 

library(shiny)
library(plotly)
library(ggplot2)
library(GGally)

df <- read.table("dataset_Facebook.csv", sep=";", header=T)
unique_month <- unique(c(as.character(df$Post.Month)))

# # Define UI for application
ui <- navbarPage("Facebook Metrics",
           tabPanel("Bubble Plot",
                    sidebarLayout(
                      sidebarPanel(checkboxGroupInput("month", "Select Month", choices = unique_month, selected = unique_month)),
                      mainPanel(plotlyOutput("bubble_plot"))
                    )
                    ),
           tabPanel("Scatter Plot",
                    sidebarLayout(
                      sidebarPanel(checkboxGroupInput("month", "Select Month", choices = unique_month, selected = unique_month)),
                      mainPanel(plotlyOutput("scatter_plot"))
                    )
                    ),
           tabPanel("Parallel Coordinates Plot",
                    sidebarLayout(
                      sidebarPanel(checkboxGroupInput("category", "Select Month", choices = unique_month, selected = unique_month)),
                    mainPanel(plotlyOutput("pc_plot"))
                    )
                    )
            )

# Define server logic required to draw a histogram
server <- function(input, output) {
   # generate reactive data based on input$ from ui.R (input <- data.frame(category=2))
   data <- reactive(subset(df, Post.Month %in% input$month))
  
   # draw
   output$bubble_plot <- renderPlotly({
     plt <- ggplot(data(), aes(like, Lifetime.Post.Total.Impressions)) +
     geom_point(aes(colour = Type))
     ggplotly(plt)
   })x
   
   output$scatter_plot <- renderPlotly({
     plt <- ggpairs(data(), c(8:10), mapping=ggplot2::aes(color = Type))
     ggplotly(plt)
   })
   
   output$pc_plot <- renderPlotly({
     plt <- ggparcoord(data(), 8:12, groupColumn= "Type") + 
       theme(axis.text.x = element_text(angle = 90), axis.title = element_blank())
     ggplotly(plt)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

