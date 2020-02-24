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
#library(themedenv)
library(scales)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("London Assembly Elections"),

    # fluid panels
    fluidRow(
        ## first column - assembly constituency radio butto
        column(4,
               h4("Assembly Constituency Winners"),
               radioButtons("radio", label = "Barnet and Camden",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "Bexley and Bromley",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"),
               radioButtons("radio", label = "Brent and Harrow",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "City and East",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "Croydon and Sutton",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"),
               radioButtons("radio", label = "Ealing and Hillingdon",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "Enfield and Haringey",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "Greenwich and Lewisham",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "Havering and Redbridge",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"),
               radioButtons("radio", label = "Lambeth and Southwark",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "Merton and Wandsworth",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "North East",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons("radio", label = "South West",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"),
               radioButtons("radio", label = "West Central",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con")
               
               ),
        ## second panel for list share
        
        column(4, offset = 0,
               h4("Assembly List Party share of vote"),
            sliderInput("lab_list_pct",
                        p("Labour", style = "color:#d50000"),
                        min = 0,
                        max = 60,
                        value = 40),
            sliderInput("con_list_pct",
                        p("Conservative", style = "color:#0087DC"),
                        min = 0,
                        max = 60,
                        value = 22),
            sliderInput("ld_list_pct",
                        p("Liberal Democrat", style = "color:#FAA61A"),
                        min = 0,
                        max = 60,
                        value = 7),
            sliderInput("grn_list_pct",
                        p("Green", style = "color:#6AB023"),
                        min = 0,
                        max = 60,
                        value = 6),
            sliderInput("other_list_pct",
                        p("Other", style = "color:#70147A"),
                        min = 0,
                        max = 60,
                        value = 4)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("resultsPlot")
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
