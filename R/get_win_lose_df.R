#' A function to extract win/lose status
#'
#' @description A function to extract the match outcome.
#'
#' @param player_data A list returned by `get_player_data`
#'
#' @return The function returns a dataframe containing player ID,
#' match ID, match date, and the match outcome. Win = 1, loss = 0.
#'
#' @examples
#' \dontrun{
#' get_win_lose_df(player_data)
#' }

get_win_lose_df <- function(player_data){
  # Check input
  if (!is.list(player_data)){
    stop('player_data must be the list returned by get_player_data().')
  }

  # Initialize a list to store win_lose_df
  win_lose_list <- list()

  # Get win_lose_df
  for (i in 1:length(player_data)){
    win_lose_df <- data.frame(player_id = player_data[[i]]$account_id,
                              match_id = player_data[[i]]$match_id,
                              date = player_data[[i]]$start_time,
                              win  = player_data[[i]]$win)

    # Convert UNIX timestamp to Y-M-D
    win_lose_df$date <- lubridate::date(lubridate::as_datetime(win_lose_df$date))

    # Append dataframe to win_lose_list
    win_lose_list[[i]] <- win_lose_df
  }

  # Merge all dataframes in list
  win_lose_df <- purrr::reduce(win_lose_list, dplyr::full_join)

  # Change win from interger to character
  win_lose_df$win <- as.character(win_lose_df$win)

  # Return win_lose_df
  return(win_lose_df)
}
