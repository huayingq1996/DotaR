#' API Delay Function
#'
#' @description A function to control the wait time between API calls to OpenDota,
#' user needs to specify the start time of a call to the API. The wait time is 1s
#' by default since OpenDota ask you to restrict your call to 1 call per second
#' with a free API.
#'
#' @param start_time Time of last API call
#' @param wait_time Wait time between API calls
#'
#' @return No return. The function put the system to sleep if the time between
#' your last API call to the current one is less than 1s.
#'
#' @examples
#' \dontrun{
#' api_key(start_time, wait_time)
#' }


api_delay <- function(start_time, wait_time = 1.00){
  # End time and total time
  end_time <- proc.time()[3]
  total_time <- end_time - start_time

  # if total time is smaller than wait time, wait enough time
  # to make sure time between each API call is 1s
  if (total_time <= wait_time){
    wait_time <- wait_time - total_time
    Sys.sleep(wait_time)
  }
}
