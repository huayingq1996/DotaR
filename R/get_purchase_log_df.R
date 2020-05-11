#' A function to extract the purchase log
#'
#' @description A function to extract item purchased and the time at which the item
#' was purchased
#'
#' @param player_data A list returned by `get_player_data`
#'
#' @return The function returns a dataframe containing player ID,
#' match ID, match date, in game time in second, name of item purchased.
#'
#' @examples
#' \dontrun{
#' get_purchase_log_df(player_data)
#' }


get_purchase_log_df <- function(player_data){
  # Check input
  if (!is.list(player_data)){
    stop('player_data must be the list returned by get_player_data().')
  }

  # Initialize a list to store purchase_log_df
  purchase_log_list <- list()

  # Get purchase log
  for (i in 1:length(player_data)){
    n_rows <- length(player_data[[i]]$purchase_log)
    purchase_log_df <- data.frame(player_id = rep(player_data[[i]]$account_id, n_rows),
                                  match_id = rep(player_data[[i]]$match_id, n_rows),
                                  date = rep(player_data[[i]]$start_time, n_rows),
                                  in_game_time = sapply(player_data[[i]]$purchase_log, function(x) return(x$time)),
                                  item = sapply(player_data[[i]]$purchase_log, function(x) return(x$key)))

    purchase_log_df$date <- lubridate::date(lubridate::as_datetime(purchase_log_df$date))
    purchase_log_df$item <- as.character(purchase_log_df$item)

    # Append dataframe to purchase_log_list
    purchase_log_list[[i]] <- purchase_log_df
  }

  # Merge all dateframe in list
  purchase_log_df <- purrr::reduce(purchase_log_list, dplyr::full_join)

  # Return purchase_log_df
  return(purchase_log_df)
}
