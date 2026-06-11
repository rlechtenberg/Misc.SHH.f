#'Assert the absence of rows in a data.frame
#'
#'Asserts the absence of rows in a data.frame. Particularly useful for asserting
#'the absence of rows **meeting some specified criteria** by passing **an
#'expression that subsets a data.frame** (see examples).
#'
#'@param df A data.frame (or, probably more usefully, an expression subsetting a data.frame).
#'@param msg An optional message to display if `df` has any rows.
#'@param sound A sound to play if the assertion fails. See [beepr::beep()] for a
#'  list of valid options.
#'
#'@returns If the assertion fails, generates an error message. If run
#'  interactively, it will also play the specified sound to call the user's
#'  attention to the fact that the calling R session has encountered an error
#'  and halted code execution, display the error in a dialog, and open a new
#'  data viewer tab displaying the offending rows.
#'@export
#'
#' @examples
#'x <- data.frame(x = c(rep(1, 5), 6:10), y = c(rep(11, 3), 14:20))
#'
#'assert_no_rows(x |> dplyr::filter(y == 14))
#'
#'# throw an error with a custom message
#'assert_no_rows(
#'   x |> dplyr::filter(y == 14),
#'   msg = "y is 14 in some rows of x"
#')

assert_no_rows <- function(df, msg = NULL, sound = 9) {
  if (nrow(df) > 0) {
    if (is.null(msg)) {
      msg <- paste0(
        "Not expecting rows where ",
        deparse1(substitute(df)) # return the expression supplied via function argument `df` as text
      )
    }

    if (interactive()) {
      # audibly call user's attention to the fact that an error has halted code execution
      beepr::beep(sound = sound)

      rstudioapi::showDialog(
        title = "Stopping execution...",
        message = paste0(
          msg,
          "... Opening a new data viewer tab displaying the offending rows..."
        )
      )

      View(x = df, title = "Unexpected obs")
    }

    stop(msg)
  }
}

#' Assert the uniqueness of some variable(s) in a data frame
#'
#' @param df A data.frame or expression evaluating to one.
#' @param by A character vector specifying the name of a column whose values are expected to be unique within `df` (or names of 2+ columns, combinations of whose values are expected to be unique).
#'
#' @returns If the assertion fails, generates an error message. If run
#'   interactively, will also play the specified sound to call the user's
#'   attention to the fact that the calling R session has encountered an error
#'   and halted code execution, display the error in a dialog, and open a new
#'   data viewer tab displaying the offending rows.
#' @export
#'
#' @examples
#' x <- data.frame(x = c(rep(1, 5), 6:10), y = c(rep(11, 3), 14:20))
#' assert_unique(x, by = "x")
#' assert_unique(x, by = c("x", "y"))
assert_unique <- function(df, by = NULL, sound = 9) {
  stopifnot(!is.null(by))

  assert_no_rows(
    df = dplyr::filter(df, .by = {{ by }}, dplyr::n() > 1),
    msg = paste0(
      "Rows of ",
      deparse1(substitute(df)),
      " not unique by ",
      deparse1(substitute(by))
    ),
    sound = sound
  )
}
