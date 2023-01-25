#' Pull player-level statistics for a given season
#'
#' @description
#' `pull_season_table()` pulls different player-level statistics from
#' basketball-reference. This data is pulled from the season stats tables on the
#' site, an example of which can be seen [here](https://www.basketball-reference.com/leagues/NBA_2023_totals.html).
#' Simply specify the season and stat table you want to pull, and this function
#' will pull it and put the table in a tibble.
#'
#' @param season Season the stats were collected in.
#' @param table Specific statistic table to be pulled. This can be any of the
#' following:
#'    * `"total"`
#'    * `"per game"`
#'    * `"per 36 minutes"`
#'    * `"per 100 possessions"`
#'    * `"advanced"`
#'    * `"play-by-play"`
#'    * `"shooting"`
#'    * `"adjusted shooting"`
#'
#' @export
#'
#' @examples
#' # Pull per game stats for players in the 2021 season
#' # pull_season_table(season = 2021, table = "per game")
pull_season_table <- function(season, table) {
  input_data <- table_translator(table = table)

  table_url <- stringr::str_c("https://www.basketball-reference.com/leagues/NBA_",
                              season, "_", input_data$url, ".html")

  # Pulling tables
  # Adjusted shooting table is hidden behind comments
  if(table == "adjusted shooting") {
    raw_table <- rvest::read_html(table_url) %>%
      rvest::html_nodes(xpath = '//comment()') %>%
      rvest::html_text() %>%
      paste(collapse = '') %>%
      rvest::read_html() %>%
      rvest::html_node(input_data$css) %>%
      rvest::html_table()
  } else {
    raw_table <- rvest::read_html(table_url) %>%
      rvest::html_node(input_data$css) %>%
      rvest::html_table()
  }

  colnames(raw_table) <- make.names(names(raw_table), unique = TRUE)

  raw_table <- colname_cleaner(raw_table, table)

  table <- raw_table %>%
    dplyr::mutate(season = season,
                  table = table) %>%
    dplyr::select(table, season, dplyr::everything())

  print(table)
}

#### Helper Functions ####

# Translating table input into url and css strings
table_translator <- function(table) {
  input_dictionary <- dplyr::tibble(
    input = c("total", "per game", "per 36 minutes", "per 100 possessions",
              "advanced", "play-by-play", "shooting", "adjusted shooting"),
    url = c("totals", "per_game", "per_minute", "per_poss", "advanced",
            "play-by-play", "shooting", "adj_shooting"),
    css = c("#totals_stats", "#per_game_stats", "#per_minute_stats",
            "#per_poss_stats", "#advanced_stats", "#pbp_stats",
            "#shooting_stats","#adj-shooting"))

  input_row <- input_dictionary %>% dplyr::filter(.data$input == table)
}


# Function to clean up column names
# This is a pretty messy function that matches the og pulled names to full names
colname_cleaner <- function(raw_table, table) {
  if(table == "play-by-play") {
    colname_dictionary <- dplyr::tibble(
      old_name = c("X", "X.1", "X.2", "X.3", "X.4", "Totals", "Totals.1",
                   "Position.Estimate", "Position.Estimate.1",
                   "Position.Estimate.2", "Position.Estimate.3",
                   "Position.Estimate.4", "X....Per.100.Poss.",
                   "X....Per.100.Poss..1", "Turnovers", "Turnovers.1",
                   "Fouls.Committed", "Fouls.Committed.1", "Fouls.Drawn",
                   "Fouls.Drawn.1", "Misc.", "Misc..1", "Misc..2"),
      new_name = c("rank", "player", "position", "age", "team", "games_played",
                   "minutes_played", "position_estimate_point_guard",
                   "position_estimate_shooting_guard",
                   "position_estimate_small_forward",
                   "position_estimate_power_forward",
                   "position_estimate_center",
                   "plus_minus_per_100_possessions_on_court",
                   "net_plus_minus_per_100_possessions", "turnovers_bad_pass",
                   "turnovers_lost_ball", "fouls_committed_shooting",
                   "fouls_committed_offensive", "fouls_drawn_shooting",
                   "fouls_drawn_offensive", "points_generated_by_assists",
                   "and_ones", "field_goal_attempts_blocked"))
  } else if(table == "shooting") {
    colname_dictionary <- dplyr::tibble(
      old_name = c("X", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6", "X.7", "X.8",
                   "X.9", "X..of.FGA.by.Distance", "X..of.FGA.by.Distance.1",
                   "X..of.FGA.by.Distance.2", "X..of.FGA.by.Distance.3",
                   "X..of.FGA.by.Distance.4", "X..of.FGA.by.Distance.5", "X.10",
                   "FG..by.Distance", "FG..by.Distance.1", "FG..by.Distance.2",
                   "FG..by.Distance.3", "FG..by.Distance.4",
                   "FG..by.Distance.5", "X.11", "X..of.FG.Ast.d",
                   "X..of.FG.Ast.d.1", "X.12", "Dunks", "Dunks.1", "X.13",
                   "Corner.3s", "Corner.3s.1", "X.14", "Heaves", "Heaves.1"),
      new_name = c("rank", "player", "position", "age", "team", "games_played",
                   "minutes_played", "field_goal_percent",
                   "average_distance_of_field_goal_attempt", "x.1",
                   "percent_of_field_goal_attempts_two_pointer",
                   "percent_of_field_goal_attempts_0-3_ft",
                   "percent_of_field_goal_attempts_3-10_ft",
                   "percent_of_field_goal_attempts_10-16_ft",
                   "percent_of_field_goal_attempts_16_ft-three_pointer",
                   "percent_of_field_goal_attempts_three_pointer",
                   "x.2", "field_goal_percent_two_pointer",
                   "field_goal_percent_0-3_ft", "field_goal_percent_3-10_ft",
                   "field_goal_percent_10-16_ft",
                   "field_goal_percent_16_ft-three_pointer",
                   "field_goal_percent_three_pointer", "x.3",
                   "percent_of_two_pointers_assisted",
                   "percent_of_three_pointers_assisted", "x.4",
                   "percent_of_field_goal_attempts_dunks", "dunks", "x.5",
                   "percent_of_three_pointers_from_corner",
                   "field_goal_percent_corner_three", "x.6", "heave_attempts",
                   "heave_makes"))
  } else if(table == "adjusted shooting") {
    colname_dictionary <- dplyr::tibble(
      old_name = c("X", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6", "X.7",
                   "Player.Shooting..", "Player.Shooting...1",
                   "Player.Shooting...2", "Player.Shooting...3",
                   "Player.Shooting...4", "Player.Shooting...5",
                   "Player.Shooting...6", "Player.Shooting...7", "X.8",
                   "League.Adjusted", "League.Adjusted.1", "League.Adjusted.2",
                   "League.Adjusted.3", "League.Adjusted.4",
                   "League.Adjusted.5", "League.Adjusted.6",
                   "League.Adjusted.7", "X.9", "X.10", "X.11"),
      new_name = c("rank", "player", "position", "age", "team", "games_played",
                   "minutes_played", "x.1", "field_goal_percent",
                   "two_pointer_percent", "three_pointer_percent",
                   "effective_field_goal_percent", "free_throw_percent",
                   "true_shooting_percent", "free_throw_attempt_rate",
                   "three_point_attempt_rate", "x.2",
                   "league_adjusted_field_goal_percent",
                   "league_adjusted_two_pointer_percent",
                   "league_adjusted_three_pointer_percent",
                   "league_adjusted_effective_field_goal_percent",
                   "league_adjusted_effective_free_throw_percent",
                   "league_adjusted_effective_true_shooting_percent",
                   "league_adjusted_effective_free_throw_attempt_rate",
                   "league_adjusted_effective_three_point_attempt_rate", "x.3",
                   "points_added_by_field_goal_shooting",
                   "points_added_by_true_shooting"))
  } else if(table %in% c("total", "per game", "per 36 minutes")) {
    colname_dictionary <- dplyr::tibble(
      old_name = c("Rk", "Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG",
                   "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.",
                   "eFG.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL",
                   "BLK", "TOV", "PF", "PTS"),
      new_name = c("rank", "player", "position", "age", "team", "games_played",
                   "games_started", "minutes_played", "field_goals",
                   "field_goals_attempted", "field_goal_percent",
                   "three_pointers_made", "three_pointers_attempted",
                   "three_pointer_percent", "two_pointers_made",
                   "two_pointers_attempted", "two_pointer_percent",
                   "effective_field_goal_percent", "free_throws_made",
                   "free_throws_attempted", "free_throw_percent",
                   "offensive_rebounds", "defensive_rebounds", "total_rebounds",
                   "assists", "steals", "blocks", "turnovers", "personal_fouls",
                   "points"))
  } else if(table == "per 100 possessions") {
    colname_dictionary <- dplyr::tibble(
      old_name = c("Rk", "Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG",
                   "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.",
                   "eFG.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL",
                   "BLK", "TOV", "PF", "PTS", "X", "ORtg", "DRtg"),
      new_name = c("rank", "player", "position", "age", "team", "games_played",
                   "games_started", "minutes_played", "field_goals",
                   "field_goals_attempted", "field_goal_percent",
                   "three_pointers_made", "three_pointers_attempted",
                   "three_pointer_percent", "two_pointers_made",
                   "two_pointers_attempted", "two_pointer_percent",
                   "effective_field_goal_percent", "free_throws_made",
                   "free_throws_attempted", "free_throw_percent",
                   "offensive_rebounds", "defensive_rebounds", "total_rebounds",
                   "assists", "steals", "blocks", "turnovers", "personal_fouls",
                   "points", "x.1", "offensive_rating", "defensive_rating"))
  } else if(table == "advanced") {
    colname_dictionary <- dplyr::tibble(
      old_name = c("Rk", "Player", "Pos", "Age", "Tm", "G", "MP", "PER", "TS.",
                   "X3PAr", "FTr", "ORB.", "DRB.", "TRB.", "AST.", "STL.", "BLK.",
                   "TOV.", "USG.", "X", "OWS", "DWS", "WS", "WS.48", "X.1",
                   "OBPM", "DBPM", "BPM", "VORP"),
      new_name = c("rank", "player", "position", "age", "team", "games_played",
                   "minutes_played", "player_efficiency_rating",
                   "true_shooting_percent", "three_point_attempt_rate",
                   "free_throw_attempt_rate", "offensive_rebound_percent",
                   "defensive_rebound_percent", "total_rebound_percent",
                   "assist_percent", "steal_percent", "block_percent",
                   "turnover_percent", "usage_percent", "x.1",
                   "offensive_win_shares", "defensive_win_shares", "win_shares",
                   "win_shares_per_48", "x.2", "offensive_box_plus_minus",
                   "defensive_box_plus_minus", "box_plus_minus",
                   "value_over_replacement_player"))
  }


  raw_table_new_names <- raw_table %>%
    stats::setNames(
      colname_dictionary$new_name[match(names(raw_table),
                                        colname_dictionary$old_name)])

  numeric_columns <- dplyr::tibble(
    fields = c("age", "and_ones", "assist_percent", "assists",
               "average_distance_of_field_goal_attempt", "block_percent",
               "blocks", "box_plus_minus", "defensive_box_plus_minus",
               "defensive_rating", "defensive_rebound_percent",
               "defensive_rebounds", "defensive_win_shares", "dunks",
               "effective_field_goal_percent", "field_goal_attempts_blocked",
               "field_goal_percent", "field_goal_percent_0-3_ft",
               "field_goal_percent_10-16_ft",
               "field_goal_percent_16_ft-three_pointer",
               "field_goal_percent_3-10_ft",
               "field_goal_percent_corner_three",
               "field_goal_percent_three_pointer",
               "field_goal_percent_two_pointer", "field_goals",
               "field_goals_attempted", "fouls_committed_offensive",
               "fouls_committed_shooting", "fouls_drawn_offensive",
               "fouls_drawn_shooting", "free_throw_attempt_rate",
               "free_throw_percent", "free_throws_attempted",
               "free_throws_made", "games_played", "games_started",
               "heave_attempts", "heave_makes",
               "league_adjusted_effective_field_goal_percent",
               "league_adjusted_effective_free_throw_attempt_rate",
               "league_adjusted_effective_free_throw_percent",
               "league_adjusted_effective_three_point_attempt_rate",
               "league_adjusted_effective_true_shooting_percent",
               "league_adjusted_field_goal_percent",
               "league_adjusted_three_pointer_percent",
               "league_adjusted_two_pointer_percent", "minutes_played",
               "net_plus_minus_per_100_possessions",
               "offensive_box_plus_minus", "offensive_rating",
               "offensive_rebound_percent", "offensive_rebounds",
               "offensive_win_shares", "percent_of_field_goal_attempts_0-3_ft",
               "percent_of_field_goal_attempts_10-16_ft",
               "percent_of_field_goal_attempts_16_ft-three_pointer",
               "percent_of_field_goal_attempts_3-10_ft",
               "percent_of_field_goal_attempts_dunks",
               "percent_of_field_goal_attempts_three_pointer",
               "percent_of_field_goal_attempts_two_pointer",
               "percent_of_three_pointers_assisted",
               "percent_of_three_pointers_from_corner",
               "percent_of_two_pointers_assisted", "personal_fouls",
               "player_efficiency_rating",
               "plus_minus_per_100_possessions_on_court", "points",
               "points_added_by_field_goal_shooting",
               "points_added_by_true_shooting",
               "points_generated_by_assists", "position_estimate_center",
               "position_estimate_point_guard",
               "position_estimate_power_forward",
               "position_estimate_shooting_guard",
               "position_estimate_small_forward", "steal_percent", "steals",
               "three_point_attempt_rate", "three_pointer_percent",
               "three_pointers_attempted", "three_pointers_made",
               "total_rebound_percent", "total_rebounds",
               "true_shooting_percent", "turnover_percent", "turnovers",
               "turnovers_bad_pass", "turnovers_lost_ball",
               "two_pointer_percent", "two_pointers_attempted",
               "two_pointers_made", "usage_percent",
               "value_over_replacement_player", "win_shares",
               "win_shares_per_48")) %>%
    dplyr::filter(fields %in% colnames(raw_table_new_names))

  if(table == "play-by-play") {
    raw_table_new_names %>%
      dplyr::filter(rank != "Rk") %>%
      dplyr::select(-rank, -dplyr::contains("x.")) %>%
      dplyr::mutate(dplyr::across(
        .cols = c("position_estimate_center", "position_estimate_point_guard",
                  "position_estimate_power_forward",
                  "position_estimate_shooting_guard",
                  "position_estimate_small_forward"),
        .fns = ~ gsub("%", "", .))) %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::any_of(numeric_columns$fields),
                                  .fns = ~ as.numeric(.))) %>%
      dplyr::mutate(dplyr::across(
        .cols = c("position_estimate_center", "position_estimate_point_guard",
                  "position_estimate_power_forward",
                  "position_estimate_shooting_guard",
                  "position_estimate_small_forward"),
        .fns = ~ . / 100))
  } else {
    raw_table_new_names %>%
      dplyr::filter(rank != "Rk") %>%
      dplyr::select(-rank, -dplyr::contains("x.")) %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::any_of(numeric_columns$fields),
                                  .fns = ~ as.numeric(.)))
  }
}
