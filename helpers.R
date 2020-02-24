library(tidyverse)
library(themedenv)
library(scales)
library(RColorBrewer)

party_radio_colours <- list(HTML("<p style='color:#d50000;'>Labour</p>"),
                            HTML("<p style='color:#0087DC;'>Conservative</p>"),
                            HTML("<p style='color:#FAA61A;'>Lib Dem</p>"))

party_radio_choices <- c("Lab", "Con", "LD")


run_assembly_election <- function(lab.seats,
                                  con.seats,
                                  lib.seats = 0,
                                  grn.seats = 0,
                                  brx.seats = 0,
                                  lab.pct,
                                  con.pct,
                                  lib.pct,
                                  grn.pct,
                                  brx.pct){
  
  schedule <- tibble(
    party = c("lab", "con", "lib dem", "green", "brexit"),
    constituency_seats = c(lab.seats, con.seats, lib.seats, grn.seats, brx.seats),
    votes = c(lab.pct, con.pct, lib.pct, grn.pct, brx.pct),
    quota = constituency_seats + 1,
    seats_won = 0
  )
  
  outputs <- tibble(round = c(1:11),
                    frame = list(schedule))
  
  for (i in c(2:11)){
    
    tt <- outputs %>%
      slice(i - 1) %>%
      select(frame) %>% 
      unnest(frame)
    
    tt <- tt %>%
      mutate(round_vote = votes / (quota + seats_won),
             seats_won = case_when(round_vote == max(round_vote) ~ seats_won + 1,
                                   round_vote != max(round_vote) ~ seats_won + 0))
    
    outputs <- outputs %>%
      mutate(frame = list(tt))
    
  }
  
  results <- outputs %>% 
    slice(11) %>% 
    unnest(frame)
  
  return(results)}