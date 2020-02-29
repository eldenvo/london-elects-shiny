library(shiny)
library(tidyverse)
library(themedenv)
library(scales)
library(sf)
library(here)
library(purrr)
library(extrafont)
library(shinythemes)
library(RColorBrewer)
source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("yeti"),

  # Application title
  titlePanel("London Assembly Elections"),

  # fluid panels
  fluidRow(
    ## first column - assembly constituency radio butto
    column(
      4,
      div(style = "font-size: 10px; padding: 0px 0px; margin-top:0",
      h4("Assembly Constituency Winners", align="center"),
      radioButtons(csv$LAC19CD[1],
        label = "Barnet and Camden",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[2],
        label = "Bexley and Bromley",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"
      ),
      radioButtons(csv$LAC19CD[3],
        label = "Brent and Harrow",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[4],
        label = "City and East",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[5],
        label = "Croydon and Sutton",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"
      ),
      radioButtons(csv$LAC19CD[6],
        label = "Ealing and Hillingdon",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[7],
        label = "Enfield and Haringey",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[8],
        label = "Greenwich and Lewisham",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[9],
        label = "Havering and Redbridge",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"
      ),
      radioButtons(csv$LAC19CD[10],
        label = "Lambeth and Southwark",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[11],
        label = "Merton and Wandsworth",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[12],
        label = "North East",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Lab"
      ),
      radioButtons(csv$LAC19CD[13],
        label = "South West",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"
      ),
      radioButtons(csv$LAC19CD[14],
        label = "West Central",
        inline = T, choiceNames = party_radio_colours, choiceValues = party_radio_choices, selected = "Con"
      )
    )),
    ## second panel for list share

    column(4,
      offset = 0,
      h4("Assembly List Party share of vote", align="center"),
      numericInput("lab_list_pct",
        p("Labour", style = "color:#d50000"),
        min = 0,
        max = 100,
        value = 40.3,
        step = 0.1
      ),
      numericInput("con_list_pct",
        p("Conservative", style = "color:#0087DC"),
        min = 0,
        max = 100,
        value = 29.2,
        step = 0.1
      ),
      numericInput("ld_list_pct",
        p("Liberal Democrat", style = "color:#FAA61A"),
        min = 0,
        max = 60,
        value = 6.3,
        step = 0.1
      ),
      numericInput("grn_list_pct",
        p("Green", style = "color:#6AB023"),
        min = 0,
        max = 100,
        value = 8,
        step = 0.1
      ),
      numericInput("other_list_pct",
        p("Other", style = "color:#70147A"),
        min = 0,
        max = 100,
        value = 6.5,
        step = 0.1
      ),
      br(),

      ### adding potential warning text below middle column

      htmlOutput("equal_list"),
      br(),
      htmlOutput("sum_text"),
      br(),
      actionButton("set_2016", "2016 London election results"),
      br(),
      actionButton("set_2018", "2018 Local election results"),
      br(),
      actionButton("set_2019", "2019 European election results")
    ),

    # Show a plot of the generated election outcome
    column(4,
      offset = 0,
      h4("Election Results" ,align="center"),
      plotOutput("plot", height = 540),
      br(),

      ## and a map of the constituencies
      plotOutput("map")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## use the inputs to run the election, based on function defined in 'helpers.r'
  result <- reactive({
    input_list <- reactiveValuesToList(input)

    constituency_winners <- tibble(
      code = names(input_list),
      seat_winner = unlist(input_list, use.names = F)
    ) %>%
      filter(code %in% csv$LAC19CD)

    lab_seats <- length(constituency_winners %>% filter(seat_winner == "Lab") %>% pull(seat_winner))
    con_seats <- length(constituency_winners %>% filter(seat_winner == "Con") %>% pull(seat_winner))
    ld_seats <- length(constituency_winners %>% filter(seat_winner == "LD") %>% pull(seat_winner))
    oth_seats <- length(constituency_winners %>% filter(seat_winner == "Oth") %>% pull(seat_winner))

    result1 <- run_assembly_election(
      lab_seats, con_seats, ld_seats, oth_seats,
      input$lab_list_pct, input$con_list_pct, input$ld_list_pct, input$grn_list_pct, input$other_list_pct
    )

    return(result1)
  })

  ## create constituency winners to use for map
  constituencies <- reactive({
    input_list <- reactiveValuesToList(input)

    constituency_winners <- tibble(
      code = names(input_list),
      seat_winner = unlist(input_list, use.names = F)
    ) %>%
      filter(code %in% csv$LAC19CD) %>%
      arrange(code) %>%
      pull(seat_winner)

    return(constituency_winners)
  })

  ## add up percentages in list vote for warning if 100+

  equal_list <- reactive({
    
    list_inputs <- c(input$lab_list_pct, input$con_list_pct, input$ld_list_pct, input$grn_list_pct, input$other_list_pct)
    
    length <- length(unique(list_inputs))

    return(length)
  })
  
  ## add up percentages in list vote for if equal
  
  sum_list <- reactive({
    sum_list <- sum(input$lab_list_pct, input$con_list_pct, input$ld_list_pct, input$grn_list_pct, input$other_list_pct)
    
    return(sum_list)
  })

  ## col chart for number of seats won

  output$plot <- renderPlot({
    p <- result() %>%
      mutate(Overall = constituency_seats + seats_won) %>%
      select(party, Constituency = constituency_seats, List = seats_won, Overall) %>%
      pivot_longer(-1, names_to = "type", values_to = "seats") %>%
      mutate(party = fct_relevel(party, party_names)) %>% {
      ggplot(., aes(x = party, y = seats, fill = party)) +
      geom_col() +
      geom_text(aes(y = case_when(.$seats > 4 ~ 2, .$seats < 5 ~ .$seats + 2), label = seats), size = 10, family = "Lato", color = "gray15") +
      scale_y_continuous(breaks = 0:16) +
      scale_fill_manual(values = party_colours) +
      facet_wrap(~type, ncol = 1) +
      labs(
        title = "",
        y = "",
        x = "",
        fill = ""
      ) +
      guides(fill = "none") +
      theme_denv(background.color = "white") +
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank()
      )}

    print(p)
  })

  ## super constituency map for seat winners

  output$map <- renderPlot({
    map <- shapefile %>%
      arrange(lac18cd) %>%
      mutate(seat_winner = constituencies()) %>%
      ggplot() +
      geom_sf(aes(fill = seat_winner), show.legend = F, size = 0.5, color = "black") +
      theme_void() +
      scale_fill_manual(values = c(Lab = "#d50000", Con = "#0087DC", LD = "#FAA61A", Oth = "#70147A")) +
      labs(subtitle = "") +
      theme_denv(background.color = "transparent") +
      theme(
        axis.text = element_blank(),
        panel.grid.major = element_line(colour = "transparent")
      )

    print(map)
  })

  ## warning text in case of list adding up to more than 100

  output$sum_text <- renderText({
    if (sum_list() > 100) {
      paste0("<font color=\"#FF0000\"><b>","Warning: ", "</b></font>", "The list adds up to more than 100%. Please reduce the totals.")
    }
    else {
      paste0("The list adds up to ", sum_list(), "%")
    }
  })
  
  ## warning text in case of list percentages being the same
  
  output$equal_list <- renderText({
    if (equal_list() == 5) {
      ""
    }
    else {
      paste0("<font color=\"#FF0000\"><b>","Warning: ", "</b></font>", "two or more of the list percentages are the same. The result created will be wrong. Please change some of the list percentages so that none of them are equal.")
    }
  })

  observeEvent(input$set_2016, {
    updateRadioButtons(session, csv$LAC19CD[1], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[2], selected = "Con")
    updateRadioButtons(session, csv$LAC19CD[3], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[4], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[5], selected = "Con")
    updateRadioButtons(session, csv$LAC19CD[6], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[7], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[8], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[9], selected = "Con")
    updateRadioButtons(session, csv$LAC19CD[10], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[11], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[12], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[13], selected = "Con")
    updateRadioButtons(session, csv$LAC19CD[14], selected = "Con")
    updateNumericInput(session, "lab_list_pct", value = 40.3)
    updateNumericInput(session, "con_list_pct", value = 29.2)
    updateNumericInput(session, "ld_list_pct", value = 6.3)
    updateNumericInput(session, "grn_list_pct", value = 8.0)
    updateNumericInput(session, "other_list_pct", value = 6.5)
  })

  observeEvent(input$set_2018, {
    updateRadioButtons(session, csv$LAC19CD[1], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[2], selected = "Con")
    updateRadioButtons(session, csv$LAC19CD[3], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[4], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[5], selected = "Con")
    updateRadioButtons(session, csv$LAC19CD[6], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[7], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[8], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[9], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[10], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[11], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[12], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[13], selected = "LD")
    updateRadioButtons(session, csv$LAC19CD[14], selected = "Lab")
    updateNumericInput(session, "lab_list_pct", value = 47.0)
    updateNumericInput(session, "con_list_pct", value = 30.8)
    updateNumericInput(session, "ld_list_pct", value = 12.7)
    updateNumericInput(session, "grn_list_pct", value = 5.9)
    updateNumericInput(session, "other_list_pct", value = 0.5)
  })

  observeEvent(input$set_2019, {
    updateRadioButtons(session, csv$LAC19CD[1], selected = "LD")
    updateRadioButtons(session, csv$LAC19CD[2], selected = "Oth")
    updateRadioButtons(session, csv$LAC19CD[3], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[4], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[5], selected = "LD")
    updateRadioButtons(session, csv$LAC19CD[6], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[7], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[8], selected = "LD")
    updateRadioButtons(session, csv$LAC19CD[9], selected = "Oth")
    updateRadioButtons(session, csv$LAC19CD[10], selected = "LD")
    updateRadioButtons(session, csv$LAC19CD[11], selected = "LD")
    updateRadioButtons(session, csv$LAC19CD[12], selected = "Lab")
    updateRadioButtons(session, csv$LAC19CD[13], selected = "LD")
    updateRadioButtons(session, csv$LAC19CD[14], selected = "LD")
    updateNumericInput(session, "lab_list_pct", value = 23.8)
    updateNumericInput(session, "con_list_pct", value = 7.9)
    updateNumericInput(session, "ld_list_pct", value = 26.9)
    updateNumericInput(session, "grn_list_pct", value = 12.3)
    updateNumericInput(session, "other_list_pct", value = 17.7)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
