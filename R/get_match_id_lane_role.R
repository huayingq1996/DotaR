#' A function to get a vector of *n* match IDs
#'
#' @description A function to fetch the match IDs of the *n* most recent matchs
#' that involves the player with the specific player ID and the specific lane role.
#'
#' @param player_id The numeric ID of your Dota 2 account, numeric
#' @param n The number of match IDs you want, numeric
#' @param lane_role Off lane = 1, mid lane = 2, safe lane = 3, numeric
#'
#' @return The function returns a numeric vector containing the match IDs of the *n*
#' most recent matchs that involves the player with with the specific player ID
#' and the specific lane role.
#'
#' @examples
#' \dontrun{
#' get_match_id_lane_role(123456, 20, 2)
#' }




get_match_id_lane_role <- function(player_id, n, lane_role){
  # Check inputs
  if (!is.numeric(player_id)){
    stop('player_id must be numeric')
  }
  if (!is.numeric(n)){
    stop('n must be numeric')
  }
  if (!is.numeric(lane_role)){
    stop('lane_role must be numeric')
  }

  # Call API
  api_call <- httr::GET(paste("https://api.opendota.com/api/players/", player_id, "/matches", sep = ""),
                  query = list(limit = n,
                               lane_role = lane_role))
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
