library(tidyverse)
library(themedenv)
library(scales)
library(RColorBrewer)
library(here)

party_radio_colours <- list(HTML("<p style='color:#d50000;'>Labour</p>"),
                            HTML("<p style='color:#0087DC;'>Conservative</p>"),
                            HTML("<p style='color:#FAA61A;'>Lib Dem</p>"),
                            HTML("<p style='color:#70147A;'>Other</p>"))

party_radio_choices <- c("Lab", "Con", "LD", "Oth")

party_names <- c("Labour", "Conservative", "Liberal Democrat", "Green", "Other")
party_colours <- c("#d50000", "#0087DC", "#FAA61A", "#6AB023", "#70147A")

csv <- read_csv(here::here("data", "London_Assembly_Constituencies_December_2019_Names_and_Codes_in_England.csv")) %>%
  mutate(winner_16 = c("Lab", "Con", "Lab", "Lab", "Con", "Lab", "Lab", "Lab", "Con", "Lab", "Lab", "Lab", "Con", "Con"))

cand <- read_csv(here::here("data", "candidate-results.csv"))


##cand %>%
##  group_by(party) %>%
##  summarise(xx = sum(number_votes, na.rm = T),
##            n = n()) %>%
##  ungroup() %>%
##  arrange(-xx) %>%
##  mutate(pct = (xx/sum(xx))*100)

cand %>%
  group_by(borough_name, party) %>%
  summarise(xx = sum(number_votes, na.rm = T), n = n()) %>%
  ungroup() %>%
  filter(party %in% c("LAB", "CON", "LD", "GRE", "UKIP")) %>%
  ungroup() %>%
  filter(borough_name %in% c("Havering", "Redbridge")) %>%
  group_by(party) %>%
  summarise(res = sum(xx)/sum(n)) %>%
  ungroup() %>%
  filter(res == max(res))


shapefile <- st_read(here("data/London_Assembly_Constituencies_December_2018_Boundaries_EN_BFC", 
                          "London_Assembly_Constituencies_December_2018_Boundaries_EN_BFC.shp"))

run_assembly_election <- function(lab.seats,
                                  con.seats,
                                  lib.seats = 0,
                                  lab.pct,
                                  con.pct,
                                  lib.pct,
                                  grn.pct,
                                  brx.pct){
  
  schedule <- tibble(
    party = party_names,
    constituency_seats = c(lab.seats, con.seats, lib.seats, 0, 0),
    votes = c(lab.pct, con.pct, lib.pct, grn.pct, brx.pct),  ### modifications to avoid equal numbers
    quota = constituency_seats + 1,
    seats_won = 0
  ) 
  
  outputs <- tibble(round = c(1:11),
                    frame = list(schedule))
  
  for (i in c(2:12)){
    
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