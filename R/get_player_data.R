#' A function to fetch the match data of a specific player
#'
#' @description A function to fetch the match data of a specific player
#'
#' @param player_id The numeric ID of your Dota 2 account, numeric
#' @param match_vec A numeric vector of at least one match ID, numeric
#' @param wait_time Wait time between API calls, numeric, default to 1s
#' @param summary_info Set to TRUE if you want summary information about
#' how many of the requested matchs are parsed by OpenDota, how many error
#' occured when calling the API, etc.
#'
#' @return The function returns a nested list. Each element in the list
#' is a parsed JSON file of a match.
#'
#' @examples
#' \dontrun{
#' get_player_data(123456, 123456)
#' }
#'
#' \dontrun{
#' get_player_data(123456, c(123456, 78910))
#' }
#'


get_player_data <- function(player_id,
                            match_vec,
                            wait_time = 1.00,
                            summary_info = TRUE){
  # Check input
  if (!is.numeric(player_id)){
    stop('player_id input must be numeric.')
  }
  if (!is.numeric(match_vec)){
    stop('match_vec input must be a numeric vector.')
  }

  # Save the inputs for later reference
  player_id <- player_id
  # In case there is any duplicate
  match_vec <- unique(match_vec)

  # Initialize a list to store the match data, number of parsed match
  # unparsed match, and error count, number of matches need to be fetched
  player_data_list <- list()
  parsed_match <- 0
  unparsed_match <- 0
  error_count <- 0
  num_match <- length(match_vec)

  # Start iteration
  for (i in 1:num_match){
    # Display informative message
    cat(paste("\nFetching", i, "of", num_match))

    # Match that need to be fetched
    match_id <- match_vec[i]

    # Initialize start time
    start_time <- proc.time()[3]

    # Call API
    api_call <- httr::GET(paste("https://api.opendota.com/api/matches/", match_id, sep = ""))
    # Check if we got an error during the call, if so, record the error and move on
    if (httr::status_code(api_call) != 200){
      warning(paste("Error:", httr::status_code(api_call)))
      error_count <- error_count + 1
      api_delay(start_time, wait_time)
      next
    }

    # Get JSON
    match_data <- httr::content(api_call, as = "parsed", type = "application/json")
    # If teamfight data is not parsed, we assume the match is not parsed by opendota and move on
    if (is.null(match_data$teamfights)){
      message(paste("Match", match_id, "is not parsed by OpenDota."))
      unparsed_match <- unparsed_match + 1
      api_delay(start_time, wait_time)
      next
    }

    # Define a function to extract all player id in the match
    extract_player_id <- function(x){
      if (is.null(x$account_id)){
        x$account_id <- 0
      }
      return(x$account_id)
    }

    # Get the data specific to the player
    players_id <- sapply(match_data$players, extract_player_id)
    player_index <- which(player_id == players_id)
    player_data <- match_data$players[[player_index]]

    # Store the JSON in match_data_list
    player_data_list[[i]] <- player_data
    parsed_match <- parsed_match + 1
    api_delay(start_time, wait_time)
  }

  # If summary_info == TRUE, print a summary information on how many games
  # were parsed.
  if (summary_info == TRUE){
    cat(paste("\nTotal matches:", num_match),
        paste("\nTotal parsed:", parsed_match),
        paste("\nTotal unparsed:", unparsed_match),
        paste("\nTotal error", error_count))
  }

  # If the user only requested 1 match, return the match JSON directly
  if (num_match == 1){
    return(player_data)
  } else {
    return(player_data_list)
  }
}
