#' Convert all labelled columns to factors (e.g., in preparation for generating
#' tables and plots)
#'
#' @param df A data frame
#' @param ordered Whether or not the factors created should be ordered factors.
#'   Defaults to TRUE. If so, the order of factor levels matches the order in
#'   which the associated labels were defined.
#' @param droplevels Whether to drop unused factor levels, such that so that
#'   categories unrepresented in the data (i.e., with 0 counts) are omitted from
#'   summary tables. Defaults to TRUE.
#'
#' @returns Input data frame `df` with labelled columns converted to factors.
#'   When tabulating or plotting data, results in categories appearing in the
#'   order in which the associated labels were defined.
#' @export
#'
#' @examples
#' x <- data.frame(sex = c(1,1,2,3) |> labelled::labelled(labels = c(Male = 1, Female = 2, Transwoman = 3), label = 'Sex Assigned at Birth'))
#' str(x)
#' str(labelled_to_factor(x))
labelled_to_factor <- function(df, droplevels = TRUE) {
  df |>
    labelled::to_factor(
      levels = "labels",
      ordered = TRUE, # make ordered factors
      sort_levels = 'none', # sort factor levels according to the order in which the labels were defined
      drop_unused_labels = droplevels, # in gtsummary tables, causes categories/rows with 0 counts to be omitted
      labelled_only = TRUE # only convert labelled variables to factors; ignore all others
    )
}
