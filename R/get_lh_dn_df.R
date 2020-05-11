#' A function to extract the last hit/deny data
#'
#' @description A function to extract the last hit/deny data from
#' the list returned by `get_player_data`
#'
#' @param player_data A list returned by `get_player_data`, lsit
#'
#' @return The function returns a dataframe containing player ID,
#' match ID, match date, last hit count, and deny count
#' @export
#'
#' @examples
#' \dontrun{
#' get_lh_dn_df(player_data)
#' }



get_lh_dn_df <- function(player_data){
  # Check input
  if (!is.list(player_data)){
    stop('player_data must be the list output by get_player_data().')
  }

  # Initialize a list to store lh_dn_df for each match
  lh_df_list <- list()

  # Construct data frame
  for (i in 1:length(player_data)){
    lh_dn_df <- data.frame(player_id = rep(player_data[[i]]$account_id, length(player_data[[i]]$lh_t)),
                           match_id = rep(player_data[[i]]$match_id, length(player_data[[i]]$lh_t)),
                           date = rep(player_data[[i]]$start_time, length(player_data[[i]]$lh_t)),
                           in_game_time = seq(0, length(player_data[[i]]$lh_t) - 1),
                           last_hit = sapply(player_data[[i]]$lh_t, function(x) return(x)),
                           deny = sapply(player_data[[i]]$dn_t, function(x) return(x)))
    # Convert date from UNIX time stamp to Y-M-D
    lh_dn_df$date <- lubridate::date(lubridate::as_datetime(lh_dn_df$date))

    # Append dataframe to the list
    lh_df_list[[i]] <- lh_dn_df
  }
  # Merge all the datasets in the list
  lh_dn_df <- purrr::reduce(lh_df_list, dplyr::full_join)

  # Return the dataset
  return(lh_dn_df)
}
