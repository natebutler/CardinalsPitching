library(tidyverse)
library(Lahman)
library(retrosheet)

Pitching <- Lahman::Pitching
Batting <- Lahman::Batting
People <- Lahman::People
Teams <- Lahman::Teams
Fielding <- Lahman::Fielding

Pitch_sum <- Lahman::Pitching %>%
  filter(yearID >= 2021) %>%
  group_by(playerID) %>%
  summarise(total_games = sum(G),
            total_starts = sum(GS),
            total_ipouts = sum(IPouts),
            total_wins = sum(W),
            total_losses = sum(L),
            total_hits = sum(H),
            total_runs = sum(R),
            total_earned_runs = sum(ER),
            total_homeruns = sum(HR),
            total_strikeouts = sum(SO),
            .groups = "drop"
            ) %>%
  left_join(People %>% select(playerID, nameFirst, nameLast, birthDate), by = 'playerID') %>%
  mutate(total_ERA = (total_earned_runs / (total_ipouts /3)) * 9) %>%
  filter(total_games > 5) %>%
  arrange(desc(total_ERA))

Cards_starters <- Lahman::Pitching %>%
  filter(yearID >= 2021 & teamID == 'SLN') %>%
  left_join(People, by = 'playerID') %>%
  filter(GS > 0)

ggplot(Cards_starters, aes(x = ERA, y = SO, group = yearID, color = yearID)) +
  geom_point()

data_2022 = sabRmetrics::download_baseballsavant(
  start_date = "2022-01-01",
  end_date = "2022-12-31",
)

saveRDS(data_2022, "statcast2022.RData")

data_2021 = sabRmetrics::download_baseballsavant(
  start_date = "2021-01-01",
  end_date = "2021-12-31",
)

saveRDS(data_2021, "statcast2021.RData")

PLAYERIDMAP <- read_csv("C:/Users/qbcar/github/Stat430Baseball/PLAYERIDMAP.csv")

cards_2022 <- data_2022 %>%
  filter(home_team == 'STL' | away_team == 'STL') %>%
  left_join(PLAYERIDMAP %>% select(IDPLAYER, TEAM, MLBID, MLBNAME), by = c("pitcher_id" = 'MLBID'))

cards_pitch <- cards_2022 %>%
  filter(TEAM == 'STL')

cards_summary <- cards_pitch %>%
  group_by(pitcher_id, MLBNAME) %>%
  summarise()

library(dplyr)

# Number of pitches thrown by each pitcher
pitch_counts <- cards_pitch %>%
  group_by(pitcher_id) %>%
  summarise(
    total_pitches = n(),
    pitch_types_thrown = list(unique(pitch_type)),
    .groups = 'drop'
  )

# Number of each type of pitch thrown by each pitcher
pitch_type_counts <- cards_pitch %>%
  group_by(pitcher_id, pitch_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = pitch_type, values_from = count, values_fill = 0)

# Combine the two if desired
combined_pitch_data <- left_join(pitch_counts, pitch_type_counts, by = "pitcher_id")

stat2022 <- readRDS('statcast2022.RData')

