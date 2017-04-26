rm(list = ls())
cat("\014") 

packageList = c('ggplot2', 'shiny', 'plotly', 'GGally')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

library(shiny)
library(plotly)
library(ggplot2)
library(GGally)

df <- read.table("dataset_Facebook.csv", sep=";", header=T)

# # Define UI for application
ui <- navbarPage("Facebook Metrics",
           tabPanel("Bubble Plot",plotlyOutput("bubble_plot")),
           tabPanel("Scatter Plot",plotlyOutput("scatter_plot")),
           tabPanel("Parallel Coordinates Plot",plotlyOutput("pc_plot"))
            )

# Define server logic required to draw a histogram
server <- function(input, output) {
   # generate reactive data based on input$ from ui.R (input <- data.frame(category=2))
  
   # draw
   output$bubble_plot <- renderPlotly({
     plt <- ggplot(df, aes(like, Lifetime.Post.Total.Impressions)) +
     geom_point(aes(colour = Type))
     ggplotly(plt)
   })
   
   output$scatter_plot <- renderPlotly({
     plt <- ggpairs(df, c(8:10), mapping=ggplot2::aes(color = Type))
     ggplotly(plt)
   })
   
   output$pc_plot <- renderPlotly({
     plt <- ggparcoord(df, 8:12, groupColumn= "Type") + 
       theme(axis.text.x = element_text(angle = 90), axis.title = element_blank())
     ggplotly(plt)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

