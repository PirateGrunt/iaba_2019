library(tidyverse)
library(ncaahoopR)

data('ncaa_colors')
fsu_color <- ncaa_colors$primary_color[ncaa_colors$ncaa_name == 'Florida St.']
lsu_color <- ncaa_colors$secondary_color[ncaa_colors$ncaa_name == 'LSU']

tbl_excitement <- tibble(
  game_id = get_game_ids('Florida State')
) %>% 
  mutate(
    excitement = map_dbl(game_id, game_excitement_index)
  ) %>%
  filter(!is.na(excitement))

tbl_fsu_pbp <- get_pbp_game(tbl_excitement$game_id)

tbl_fsu_pbp <- tbl_fsu_pbp %>% 
  group_by(game_id) %>% 
  mutate(
    elapsed = max(secs_remaining_absolute) - secs_remaining_absolute
    , at_home = home == 'Florida State'
    , score_diff = ifelse(
        at_home
      , score_diff
      , -score_diff)
    , opponent = ifelse(
        at_home
      , away
      , home
      )
    , tie = (score_diff == 0 & (home_score > 0 | away_score > 0))
    ) %>% 
  ungroup() %>% 
  inner_join(tbl_excitement)

tbl_fsu_pbp <- tbl_fsu_pbp %>% 
  inner_join(
    select(ncaa_colors, ncaa_name, opponent_color = primary_color)
    , by = c('opponent' = 'ncaa_name')
  )

game_fsu_lsu <- tbl_fsu_pbp %>% 
  filter(home == 'LSU' | away == 'LSU') %>% 
  arrange(desc(excitement)) %>% 
  slice(1) %>% 
  dplyr::pull(game_id) %>% 
  unique()

tbl_pbp_fsu_lsu <- tbl_fsu_pbp %>% 
  filter(game_id == game_fsu_lsu)
  
tbl_ties_fsu_lsu <- tbl_pbp_fsu_lsu %>%
  filter(tie)

plt_fsu_lsu_flow <- game_flow(
  game_id = game_fsu_lsu
  , home_col = fsu_color
  , away_col = lsu_color)

save(
    file = 'data/ncaa.rda'
  , fsu_color
  , lsu_color
  , tbl_excitement
  , tbl_fsu_pbp
  , tbl_pbp_fsu_lsu
  , game_fsu_lsu
  , plt_fsu_lsu_flow
)
