#' A function to extract the rune log
#'
#' @description A function to extract the type of rune a player got and the time
#' at which the rune was got.
#'
#' @param player_data A list returned by `get_player_data`
#'
#' @return The function returns a dataframe containing player ID,
#' match ID, match date, in game time in second, name of the rune you got.
#'
#' @examples
#' \dontrun{
#' get_rune_log_df(player_data)
#' }


get_rune_log_df <- function(player_data){
  # Check input
  if (!is.list(player_data)){
    stop('player_data must be the list given by get_player_data().')
  }

  # Initialize a list to store rune_log_df for each match
  rune_log_list <- list()

  # Get rune log
  for (i in 1:length(player_data)){
    n_rows <- length(player_data[[i]]$runes_log)
    rune_log_df <- data.frame(player_id = rep(player_data[[i]]$account_id, n_rows),
                              match_id = rep(player_data[[i]]$match_id, n_rows),
                              date = rep(player_data[[i]]$start_time, n_rows),
                              in_game_time = sapply(player_data[[i]]$runes_log, function(x) return(x$time)),
                              rune = sapply(player_data[[i]]$runes_log, function(x) return(x$key)))

      dplyr::mutate(rune_log_df, date = lubridate::date(lubridate::as_datetime(date)),
               rune = dplyr::case_when(rune == 0 ~ "double damage",
                              rune == 1 ~ "haste",
                              rune == 2 ~ "illusion",
                              rune == 3 ~ "invisibility",
                              rune == 4 ~ "regenaration",
                              rune == 5 ~ "bounty",
                              rune == 6 ~ "arcane"))

    # Append dataframe to the list
    rune_log_list[[i]] <- rune_log_df
  }

  # Merge all the dataframes in the list
  rune_log_df <- purrr::reduce(rune_log_list, dplyr::full_join)

  # Return
  return(rune_log_df)
}
