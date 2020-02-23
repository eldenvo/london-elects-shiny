#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(themedenv)
library(scales)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("London Assembly Elections"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("lab_list_pct",
                        p("Labour share of vote on the list:", style = "color:#d50000"),
                        min = 0,
                        max = 60,
                        value = 40),
            sliderInput("con_list_pct",
                        p("Conservative share of vote on the list:", style = "color:#0087DC"),
                        min = 0,
                        max = 60,
                        value = 22),
            sliderInput("ld_list_pct",
                        p("Liberal Democrat share of vote on the list:", style = "color:#FAA61A"),
                        min = 0,
                        max = 60,
                        value = 7),
            sliderInput("grn_list_pct",
                        p("Green share of vote on the list:", style = "color:#6AB023"),
                        min = 0,
                        max = 60,
                        value = 6),
            sliderInput("other_list_pct",
                        p("Other share of vote on the list:", style = "color:#70147A"),
                        min = 0,
                        max = 60,
                        value = 4)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
