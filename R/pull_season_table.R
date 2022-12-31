#' Pull season tables
#'
#' @param season Season the stats were collected in.
#' @param table Specific statistic table to be pulled.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' pull_season_table(season = 2021, table = "per_game")
pull_season_table <- function(season, table) {
  table_url <- stringr::str_c("https://www.basketball-reference.com/leagues/NBA_",
                              season, "_", table, ".html")

  raw_table <- rvest::read_html(table_url) %>%
    rvest::html_node(stringr::str_c("#", table, "_stats")) %>%
    rvest::html_table()

  colnames(raw_table) <- make.names(names(raw_table), unique = TRUE)

  table <- raw_table %>% dplyr::mutate(season = season)

  print(table)
}
