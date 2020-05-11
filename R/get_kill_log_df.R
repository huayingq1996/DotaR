#' A function to extract the kill log
#'
#' @description A function to extract enemy heroes killed and the time
#' at which they were killed in a match
#'
#' @param player_data A list returned by `get_player_data`, list
#'
#' @return The function that returns a dataframe containing player ID,
#' match ID, match date, in game time in second, name of enemy
#' hero killed.
#'
#' @examples
#' \dontrun{
#' get_kill_log_df(player_data)
#' }


get_kill_log_df <- function(player_data){
  # Check input
  if (!is.list(player_data)){
    stop('player_data must be the list returned by get_player_data().')
  }

  # Initialize a list to store kill_log_df
  kill_log_list <- list()

  # Get kill log
  for (i in 1:length(player_data)){
    n_rows <- length(player_data[[i]]$kills_log)
    kill_log_df <- data.frame(player_id = rep(player_data[[i]]$account_id, n_rows),
                              match_id = rep(player_data[[i]]$match_id, n_rows),
                              date = rep(player_data[[i]]$start_time, n_rows),
                              in_game_time = sapply(player_data[[i]]$kills_log, function(x) return(x$time)),
                              kill = sapply(player_data[[i]]$kills_log, function(x) return(x$key)))

    kill_log_df$date <- lubridate::date(lubridate::as_datetime(kill_log_df$date))
    kill_log_df$kill <- as.character(kill_log_df$kill)
    kill_log_df$kill <- stringr::str_extract_all(kill_log_df$kill, "(?<=npc_dota_hero_)[:graph:]+")
    kill_log_df$kill <- stringr::str_replace_all(kill_log_df$kill, "_", " ")

    # Append dataframe to kill_log_list
    kill_log_list[[i]] <- kill_log_df
  }

  # Merge all dataframes in list
  kill_log_df <- purrr::reduce(kill_log_list, dplyr::full_join)

  # Return kill_log_df
  return(kill_log_df)
}
