library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(nflreadr)

sterb_analytics_theme <- function(..., base_size = 12) {
    ggplot2::theme(
        text = ggplot2::element_text(family = "Bahnschrift", size = base_size),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(color = "black", face = "bold"),
        axis.text = ggplot2::element_text(color = "black", size = base_size),
        plot.title.position = "plot",
        plot.title = ggplot2::element_text(
            size = base_size * 1.52, face = "bold", color = "black",
            vjust = .02, hjust = 0.08
        ),
        plot.subtitle = ggplot2::element_text(size = base_size * 1.08, color = "black", hjust = 0.08),
        plot.caption = ggplot2::element_text(size = base_size, color = "black"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#cccccc"),
        panel.background = ggplot2::element_rect(fill = "#f8f8f8"),
        plot.background = ggplot2::element_rect(fill = "#ffffff"),
        panel.border = ggplot2::element_blank()
    )
}

loadSeasons <- 2021:2024

pbp <- nflreadr::load_pbp(loadSeasons) %>%
    filter(season_type == "REG")

playerStats <- nflreadr::load_player_stats(stat_type = "defense", seasons = loadSeasons) %>%
    filter(season_type == "REG")

fSeasons <- 2022:2024

pbp_clean <- pbp %>%
    filter(
        season %in% fSeasons,
        rush == 1 | pass == 1,
        !is.na(epa),
        !is.na(defteam)
    ) %>%
    filter(wp <= 0.95 & wp >= 0.05) %>%
    mutate(season_week = paste0(season, "_", week))

playerIDs <- unique(playerStats$player_id)

fPlayerDisplayName <- "Nick Bosa"

fPlayerID <- playerStats %>%
    filter(player_display_name == fPlayerDisplayName) %>%
    select(player_id) %>%
    distinct() %>%
    pull()

fPlayerStats <- playerStats %>%
    filter(season %in% fSeasons) %>%
    filter(player_id == fPlayerID) %>%
    mutate(season_week = paste0(season, "_", week)) %>%
    select(player_id, player_name, player_display_name, season_week, team, position, season, week)



pbp_team_week <- pbp_clean %>%
    group_by(defteam, season_week) %>%
    summarise(
        plays = n(),
        epa = mean(epa, na.rm = TRUE),
        success_rate = mean(success, na.rm = TRUE),
        .groups = "drop"
    )

pbp_with_player <- pbp_team_week %>%
    left_join(fPlayerStats, by = c("defteam" = "team", "season_week")) %>%
    group_by(season_week) %>%
    mutate(with_player = ifelse(any(player_display_name == fPlayerDisplayName), 1, 0)) %>%
    ungroup() %>%
    mutate(
        with_player = ifelse(is.na(with_player), 0, with_player),
        is_player_team = ifelse(!is.na(player_id), 1, 0)
    ) %>%
    select(with_player, is_player_team, defteam, season_week, plays, epa, success_rate)

pbp_summary <- pbp_with_player %>%
    group_by(with_player, defteam) %>%
    summarise(
        n_weeks = n(),
        avg_plays = mean(plays, na.rm = TRUE),
        avg_epa = mean(epa, na.rm = TRUE),
        avg_success_rate = mean(success_rate, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    left_join(nflreadr::load_teams() %>% select(team_abbr, team_name, team_color, team_color2, team_logo_wikipedia),
        by = c("defteam" = "team_abbr")
    )  %>% 
    filter(with_player == 0)  %>% 
    arrange(avg_epa)
