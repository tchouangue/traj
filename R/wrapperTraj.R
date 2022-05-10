
#' @title Wrapper Function to Perform Trajectory Analysis
#' @description Run three steps of trajectory analysis with default parameters.
#' @param Data Data frame containing trajectory data.
#'Each line should contain sequential observations. See details.
#' @param Time Data frame containing the time of each observation in the
#'\code{Data} table.See details.
#' @param ID Logical. Set to \code{FALSE} if the first column of \code{Data} corresponds to an \code{ID} variable. Defaults to \code{FALSE}.
#' @return The result is a \code{traj} object. Details can be found in the 'Value' section of \code{\link[traj]{step3clusters}}.
#' @details The function runs the full three step trajectory analysis and returns a \code{traj} object.
#'It will execute \code{\link[traj]{step1measures}}, \code{\link[traj]{step2factors}} and \code{\link[traj]{step3clusters}}
#'sequentially with their default parameters. The result of \code{step3clusters} will be returned. Details regarding 
#'the \code{data} and the \code{time} arguments are found in the 'Details' section of \code{\link[traj]{step1measures}}.
#' @examples 
#' \dontrun{
#' # Setup data and time
#' data = example.data$data
#' time = example.data$time
#'
#' # Run clustering wrapper function
#' wt = wrapperTraj(data, time, ID = TRUE)
#'
#' # Display and plot "traj" object
#' wt
#' summary(wt)
#'
#' plot(wt)
#'}
#'
#' @rdname wrapperTraj
#' @export 
wrapperTraj <- function(Data, Time, ID = FALSE)
{
  s1 = step1measures(Data, Time, ID = ID)
  s2 = step2factors(s1)
  s3 = step3clusters(s2)

  return(s3)
}
