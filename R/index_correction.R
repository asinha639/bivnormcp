#' Correct Index Jumps Within a Threshold
#'
#' Cleans up a sequence of indices by merging nearby ones based on a user-defined distance threshold.
#'
#' @param index A numeric vector of change point indices.
#' @param tolerance Integer. The maximum allowed difference between consecutive indices to be considered the same (default is 2).
#'
#' @return A cleaned vector of unique indices.
#' @export
index_correction <- function(index, tolerance = 2) {
  # Check inputs
  if (!is.numeric(index)) {
    stop("Input 'index' must be a numeric vector.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    stop("Argument 'tolerance' must be a single non-negative numeric value.")
  }

  # Remove NAs up front
  index <- index[!is.na(index)]

  # If all removed or empty input
  if (length(index) == 0) {
    warning("No valid index values provided. Returning empty vector.")
    return(integer(0))
  }

  # Sort to ensure consistency
  index <- sort(index)

  # Apply merging logic
  if (length(index) > 1) {
    for (i in 2:length(index)) {
      if (abs(index[i] - index[i - 1]) <= tolerance) {
        index[i] <- index[i - 1]
      }
    }
  }

  # Return unique, filtered result
  cleaned_index <- unique(index)
  cleaned_index <- cleaned_index[cleaned_index != 0]

  if (length(cleaned_index) == 0) {
    warning("All indices removed after filtering (e.g., all were 0). Returning empty vector.")
  }

  return(cleaned_index)
}
