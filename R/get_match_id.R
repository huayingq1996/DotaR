#' A function to get a numeric vector of *n* match IDs
#'
#' @description A function to fetch the match IDs of the *n* most recent matchs
#' that involves the player with the specific player ID provided by the user.
#'
#'
#' @param player_id The numeric ID of your Dota 2 account, numeric
#' @param n The number of match IDs you want, numeric
#'
#' @return The function returns a numeric vector containing the match IDs of the *n*
#' most recent matchs that involves the player with the specific player ID provided by the user.
#'
#' @examples
#' \dontrun{
#' get_match_id(123456, 20)
#' }


get_match_id <- function(player_id, n){
  # Check inputs
  if (!is.numeric(player_id)){
    stop('player_id must be numeric')
  }
  if (!is.numeric(n)){
    stop('n must be numeric')
  }

  # Call API
  api_call <- httr::GET(paste("https://api.opendota.com/api/players/", player_id, "/matches?limit=", n, sep = ""))
  # Report error if any and stop
  if (httr::status_code(api_call) != 200){
    stop(paste("Error:", httr::status_code(api_call)))
  }

  # Parse JSON
  match_id_json <- httr::content(api_call, as = "parsed", type = "application/json")
  num_match <- length(match_id_json)
  # Print out how many games are returned
  cat(paste("\nRequested:", n, ""),
      paste("\nReturned:", num_match))

  # Extract match_id
  match_id_vec <- sapply(match_id_json, function(x) return(x$match_id))

  # Return match_id
  return(match_id_vec)
}
