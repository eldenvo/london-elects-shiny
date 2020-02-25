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
               radioButtons(csv$LAC19CD[1], label = "Barnet and Camden",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[2], label = "Bexley and Bromley",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"),
               radioButtons(csv$LAC19CD[3], label = "Brent and Harrow",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[4], label = "City and East",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[5], label = "Croydon and Sutton",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"),
               radioButtons(csv$LAC19CD[6], label = "Ealing and Hillingdon",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[7], label = "Enfield and Haringey",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[8], label = "Greenwich and Lewisham",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[9], label = "Havering and Redbridge",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"),
               radioButtons(csv$LAC19CD[10], label = "Lambeth and Southwark",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[11], label = "Merton and Wandsworth",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[12], label = "North East",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"),
               radioButtons(csv$LAC19CD[13], label = "South West",
                            inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"),
               radioButtons(csv$LAC19CD[14], label = "West Central",
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
        column(4, offset = 0,
               h4("Outcome"),
            plotOutput('plot')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    result <- reactive({

        radio_ins <- reactiveValuesToList(input)
        
        constituency_winners <- tibble(code = names(radio_ins),
                                       seat_winner = unlist(radio_ins, use.names = F)) %>%
            filter(code %in% csv$LAC19CD)
        
    
        lab_seats <- length(constituency_winners %>% filter(seat_winner == "Lab") %>% pull(seat_winner)) 
        con_seats <- length(constituency_winners %>% filter(seat_winner == "Con") %>% pull(seat_winner))
        ld_seats <- length(constituency_winners %>% filter(seat_winner == "LD") %>% pull(seat_winner))
        
        result1 <- run_assembly_election(lab_seats, con_seats, ld_seats,
                                        input$lab_list_pct, input$con_list_pct, input$ld_list_pct, input$grn_list_pct, input$other_list_pct)
        
        
        
        return(result1)
        
    })
    
    output$plot <- renderPlot({
        
        p <- result() %>%
            mutate(overall = constituency_seats + seats_won) %>%
            select(party, constituency = constituency_seats, list = seats_won, overall) %>%
            pivot_longer(-1, names_to = "type", values_to = "seats") %>%
            mutate(party = fct_relevel(party, party_names)) %>%
            ggplot(aes(x = party, y = seats, fill = party)) +
            geom_col() +
            scale_y_continuous(breaks = 0:14) +
            facet_wrap(~type, ncol = 1)
        
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
