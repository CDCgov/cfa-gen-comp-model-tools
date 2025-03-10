#' Check time within intervention period
#'
#' Helper function to determine if the time supplied falls inside or on the
#' supplied bounds (inclusive)
#'
#' @param time vector (type: double) containing time steps to run the model over
#' @param start_time time step (type: double) at which intervention will start
#' @param end_time time step (type: double) at which intervention will end
#' @return logical response if the time supplied falls in the bounds of an
#' intervention time
#' @examples
#' \dontrun{
#' yn_intervention <- is_intervention_period(
#'   time, intervention_start_time,
#'   intervention_end_time
#' )
#' }
##############################################################################
# NOTE: This function is PLANNED TO BE MODIFIED / INCORPORATED DIFFERENTLY in
#       this repo as part of the merge of compModels with SIRmodelbuilder
#       functionality. It may be determined that part of the functionality is
#       useful and, if so, it will remain or be incorporated into other
#       functionality.
##############################################################################
is_intervention_period <- function(time, start_time, end_time) {
  !is.na(start_time) &&
    !is.na(end_time) &&
    time >= start_time &&
    time <= end_time
}
