library(tidyverse)
library(hoopR)
library(future)


join_espn_nba_game_info <- function(season){
  
  # season is YYYY numeric where the 2023-24 season would be considered 2024
  
  # parallelize joining of schedule information
  plan(multisession)
  

  espn_sched <- load_nba_pbp(seasons = season) %>% 
    select(game_id, game_date, home_team_mascot, away_team_mascot) %>% 
    group_by(game_id, game_date, home_team_mascot, away_team_mascot) %>% 
    rename(home_team_name = home_team_mascot, 
           away_team_name = away_team_mascot) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    select(-n) %>% 
    rename(espn_game_id = game_id) %>% 
    mutate(game_date = as_date(game_date))
  
  nba_sched <- nba_schedule(season = season - 1)
  
  plan(NULL)
  
  nba_game_info <- nba_sched %>% 
    select(game_date, game_id, game_sequence, series_text, arena_city, home_team_name, away_team_name, 
           home_team_wins, home_team_losses, away_team_wins, away_team_losses) %>% 
    rename(nba_game_id = game_id) %>% 
    mutate(game_date = as_date(game_date))
  
  all_game_info <- espn_sched %>% 
    left_join(nba_game_info, by = join_by(game_date, home_team_name, away_team_name)) %>% 
    filter(series_text != "Preseason")
  
  
  return(all_game_info)
  
  
}

join_pbp_info <- function(nba_game_ids, season){
  
  game_info <- join_espn_nba_game_info(season) %>% filter(nba_game_id %in% nba_game_ids)
  
  espn_ids <- game_info$espn_game_id
  
  nba_pbp <- nba_pbps(nba_game_ids) %>% 
    select(game_id, period, time_quarter, away_player1:home_player5) %>% 
    rename(nba_game_id = game_id)
  
  espn_pbp <- load_nba_pbp(seasons = season) %>% 
    filter(game_id %in% espn_ids)
  
  nba_pbp <- nba_pbp %>% 
    left_join(
      game_info %>% select(nba_game_id, espn_game_id), 
              by = join_by(nba_game_id))
  
  pbp_total <- espn_pbp %>% 
    left_join(nba_pbp, 
              by = join_by(game_id == espn_game_id, period_number == period, 
                           clock_display_value == time_quarter)
            ) %>% 
    distinct(id, .keep_all = TRUE) %>% 
    fill(nba_game_id:home_player5)
  
  return(pbp_total)
  
}

get_reg_pbp <- function(season){
  
  games <- join_espn_nba_game_info(season) %>% filter(series_text == "")
  
  game_ids <- games$nba_game_id
  
  plan(multisession)
  
  pbp <- join_pbp_info(nba_game_ids = game_ids, season = season)
  
  plan(NULL)
  
  return(pbp)
  
}

