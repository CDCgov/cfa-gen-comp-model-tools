#' Helper function to expand transition matrix based on number of subgroup
#' combinations
#'
#' Takes in the user supplied transition matrix of rates of flow between model
#' states (compartments) and expands the transition matrix to dimensions of all
#' combinations of states and subgroups
#'
#' @param trans_matrix_base the initial matrix providing transition rates
#' between states (compartments) supplied by the user before stratification
#' @param num_combinations the total number of all subgroup combinations
#' @return an expanded matrix of transition rates between for each state
#' (compartment) and subgroup combination.

##############################################################################
# NOTE: This function is PLANNED TO BE REMOVED from this repo as part of the
#       merge of compModels with SIRmodelbuilder functionality. It may be
#       determined that part of the functionality is useful and, if so, it will
#       be incorporated into other functionality.
##############################################################################
expand_trans_mtx <- function(trans_matrix_base, num_combinations) {
  # Create block-diagonal matrix with trans_matrix_base replicated for each
  # subgroup combination
  if (num_combinations == 0) {
    return(trans_matrix_base)
  }

  expanded_trans_matrix <- Matrix::bdiag(replicate(num_combinations,
    as.matrix(trans_matrix_base),
    simplify = FALSE
  ))
  # Convert back to regular matrix if it's sparse
  if ("dgCMatrix" %in% class(expanded_trans_matrix)) {
    expanded_trans_matrix <- as.matrix(expanded_trans_matrix)
  }
  expanded_trans_matrix
}
