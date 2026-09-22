#' Attach `notes` attribute
#'
#' @param x Any arbitrary object (e.g., a vector/variable, a data frame)
#' @param note A note to associate with that object
#'
#' @returns `x` (+ an additional/modified `notes` attribute)
#' @export
#'
#' @examples
#' x <- data.frame(
#'   x = rep(1:3,2) |>
#'     labelled::labelled(labels = c("One" = 1, "Two" = 2, "Three" = 3)) |>
#'     labelled::set_variable_labels("An integer") |>
#'     set_notes_attr("These are just integers between 1 and 3"),
#'   y = rep(letters[1:3],2) |>
#'     labelled::labelled(labels = c("A" = "a", "B" = "b", "C" = "c")) |>
#'     labelled::set_variable_labels("A letter") |>
#'     set_notes_attr("These are just letters 'a' through 'c'")
#'   ) |>
#'   set_notes_attr("An example dataset")
set_notes_attr <- function(x, note) {
  attr(x, "notes") <- note
  return(x)
}

#' Generate a data dictionary for a data frame
#'
#' @param df A data frame
#'
#' @returns A data frame with columns Variable, Label, Type, Classes, Values, and
#'   Notes describing each column in `df`
#' @export
#'
#' @examples
#' x <- data.frame(
#'   x = rep(1:3,2) |>
#'     labelled::labelled(labels = c("One" = 1, "Two" = 2, "Three" = 3)) |>
#'     labelled::set_variable_labels("An integer") |>
#'     set_notes_attr("These are just integers between 1 and 3"),
#'   y = rep(letters[1:3],2) |>
#'     labelled::labelled(labels = c("A" = "a", "B" = "b", "C" = "c")) |>
#'     labelled::set_variable_labels("A letter") |>
#'     set_notes_attr("These are just letters 'a' through 'c'")
#'   ) |>
#'   set_notes_attr("An example dataset")
#'
#'gen_data_dict(x) |> write.csv("temp.csv")
gen_data_dict <- function(df) {
  purrr::map(names(df), .f = function(col) {
    data.frame(
      Variable = col,
      Type = typeof(df[[col]]),
      Label = labelled::get_label_attribute(df[[col]]) %||% NA_character_,
      Values = paste(
        labelled::get_value_labels(df[[col]]),
        " = ",
        names(labelled::get_value_labels(df[[col]])),
        collapse = "\n"
      ) %||%
        NA_character_,
      # Classes = paste(class(df[["x"]]), collapse = ", "),
      Notes = attr(df[[col]], "notes") %||% NA_character_
    )
  }) |>
    purrr::reduce(.f = rbind)
}
