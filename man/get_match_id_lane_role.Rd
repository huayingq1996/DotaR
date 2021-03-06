% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_match_id_lane_role.R
\name{get_match_id_lane_role}
\alias{get_match_id_lane_role}
\title{A function to get a vector of \emph{n} match IDs}
\usage{
get_match_id_lane_role(player_id, n, lane_role)
}
\arguments{
\item{player_id}{The numeric ID of your Dota 2 account, numeric}

\item{n}{The number of match IDs you want, numeric}

\item{lane_role}{Off lane = 1, mid lane = 2, safe lane = 3, numeric}
}
\value{
The function returns a numeric vector containing the match IDs of the \emph{n}
most recent matchs that involves the player with with the specific player ID
and the specific lane role.
}
\description{
A function to fetch the match IDs of the \emph{n} most recent matchs
that involves the player with the specific player ID and the specific lane role.
}
\examples{
\dontrun{
get_match_id_lane_role(123456, 20, 2)
}
}
