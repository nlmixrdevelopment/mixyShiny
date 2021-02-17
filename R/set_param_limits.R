#' Set limits for parameter sliders
#' 
#' This function is mainly for internal use. (It is not commonly needed by users
#' of the package.)
#' 
#' @details The multiplier is used when the limit is infinite as follows:
#' 
#' \itemize{
#' \item{Negative or positive estimate}{The \code{estimate} is multiplied or
#' divided by the \code{multiplier} to give the limit.}
#' \item{Zero estimate}{The positive or negative value of \code{multiplier} is
#' used as the limit.}
#' }
#' 
#' @param estimate Estimated value
#' @param limit The limit for estimation of the value
#' @param trans (Not yet used) how to translate the value from the
#'   estimate/limit scale to the linear scale
#' @param direction Is the limit the \code{"lower"} or \code{"upper"} limit?
#'   (Optional, may be automatically detected if all limits are in one direction
#'   from or the same as the estimate.)
#' @param multiplier How are estimates modified when the limit is infinite (see details)
#' @return New limits for use in the shiny inputs
#' @keywords Internal
#' @export
set_param_limits <- function(estimate, limit, trans, direction, multiplier=10) {
  # Check inputs
  stopifnot(is.numeric(estimate))
  stopifnot(is.numeric(limit))
  stopifnot((length(estimate) == length(limit)) | (length(limit) == 1))
  stopifnot(is.character(trans) | all(is.na(trans)))
  stopifnot(length(direction) == 1)
  if (missing(direction)) {
    direction <-
      if (all(limit == estimate)) {
        stop("Cannot detect limit direction")
      } else if (all(limit <= estimate)) {
        "lower"
      } else if (all(limit >= estimate)) {
        "upper"
      } else {
        stop("Error in limit direction detection") # nocov
      }
  }
  stopifnot(direction %in% c("lower", "upper"))
  stopifnot(length(multiplier) == 1)
  stopifnot(is.numeric(multiplier))
  # Setup what to do for negative, zero, and positive estimates with an infinite
  # bound.
  update_value <-
    list(
      lower=
        list(
          negative=multiplier,
          zero=-multiplier,
          positive=1/multiplier
        ),
      upper=
        list(
          negative=1/multiplier,
          zero=multiplier,
          positive=multiplier
        )
    )[[direction]]
  ret <- limit
  # Modify infinite limits according to the direction
  mask_inf <- is.infinite(ret)
  mask_inf_negative <- mask_inf & (estimate < 0)
  mask_inf_zero <- mask_inf & (estimate == 0)
  mask_inf_positive <- mask_inf & (estimate > 0)
  ret[mask_inf_negative] <- estimate[mask_inf_negative]*update_value$negative
  ret[mask_inf_zero] <- rep(update_value$zero, sum(mask_inf_zero))
  ret[mask_inf_positive] <- estimate[mask_inf_positive]*update_value$positive
  # Return the result
  ret
}
