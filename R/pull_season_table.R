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

  raw_table <- rvest::read_html(table_url) %>%
    rvest::html_node(input_data$css) %>%
    rvest::html_table()

  colnames(raw_table) <- make.names(names(raw_table), unique = TRUE)

  table <- raw_table %>% dplyr::mutate(season = season)

  print(table)
}

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
