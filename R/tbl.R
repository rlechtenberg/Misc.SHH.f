#' Add bars depicting the percents within a summary table
#'
#' @param x gtsummary object
#'
#' @returns an gt table data object
#' @export
#'
#' @examples
#'x <- data.frame(
#' sex = c(rep(1, 75), rep(2, 25)) |>
#'   labelled::labelled(
#'     labels = c(Male = 1, Female = 2, Transwoman = 3),
#'     label = 'Sex Assigned at Birth'
#'   )
#' )
#' x |>
#'   Misc.SHH.f::labelled_to_factor() |>
#'   gtsummary::tbl_summary() |>
#'   add_bars_for_pcts()

add_bars_for_pcts <- function(x) {
  stopifnot("gtsummary" %in% class(x))

  x |>
    gtsummary::as_gt() |>
    gt::text_transform(
      # Targets all statistical output columns (stat_1, stat_2, etc.)
      locations = gt::cells_body(columns = starts_with("stat_")),
      fn = function(x) {
        # Extract the numeric value immediately preceding the '%' sign
        pct <- as.numeric(stringr::str_extract(x, "[0-9.]+(?=%)"))

        # Create an HTML progress bar div container behind the text
        ifelse(
          is.na(pct),
          x, # Leaves rows without a percentage (e.g., continuous means) completely untouched
          paste0(
            "<div style='position: relative; width: 100%; display: flex; align-items: center; min-height: 24px;'>",
            "<div style='position: absolute; left: 0; top: 2px; bottom: 2px; width: ",
            pct,
            "%; background-color: rgba(0, 123, 255, 0.2); border-radius: 3px;'></div>",
            "<span style='position: relative; z-index: 1; padding-left: 4px; font-weight: 500;'>",
            # z-index: 1: Guarantees that the typography is layered cleanly over
            # the top of the bar container, ensuring maximum readability.
            x,
            "</span>",
            "</div>"
          )
        )
      }
    )
}

#'Group breakdowns by non-mutually exclusive race/ethnicity variables (e.g.,
#'natam_multi, black_multi, latino)
#'
#'@param x gtsummary object of class 'tbl_summary' containing breakdowns by the
#'  variables specified in `vars` (possibly among other variables)
#'@param variables A character vector of names of variables to group that appear
#'  in x$table_body. Selected variables should be appear consecutively in table
#'@param header A string specifying the header/label to place above the variable
#'  group (e.g., "Other factors")
#'@param pattern_label_to_omit String specifying a regular expression matching
#'  labels (converted to uppercase for purposes of comparison) for categories to
#'  omit from the variable group. Defaults to "^((NOT)|(NO))\\b.+", matching
#'  labels beginning with "No" or "Not" (e.g., "Not Black", "No documented meth
#'  use")
#'@param pattern_label_to_show String specifying a regular expression matching
#'  labels (converted to uppercase for purposes of comparison) for categories to
#'  show in the variable group. Defaults to NULL. If specified,
#'  `pattern_label_to_omit` is ignored--set it to NULL to suppress a warning
#'  about this.
#'
#'@returns A gtsummary table object. If any of the tabulated observations (e.g.,
#'  PWH) have an unknown/missing value for a variable included in the variable
#'  group, a footnote is added to the label for that variable specifying the
#'  number missing
#'@export
#'
#' @examples
#' df <- kcHARS::read_eoq_append(
#' date = "2026-07-01",
#' col_select = c(ethnicity1, matches("race[0-9]"), birth_country_cd)
#' ) |>
#'   kcHARS::calc_race_eth_vars() |>
#'   kcHARS::calc_nativity() |>
#'   dplyr::select(-ethnicity1, -matches("race[0-9]"), -birth_country_cd)
#'
#' x <- df |> Misc.SHH.f::labelled_to_factor() |> gtsummary::tbl_summary()
#'
#' x # what the table looks like BEFORE applying group_race_eth_multi_vars()
#'
#' x |>
#'   group_dichot_vars(
#'     variables = c(
#'       "black_multi",
#'       "natam_multi",
#'       "asian_multi",
#'       "black_multi",
#'       "latino",
#'       "pacisl_multi",
#'       "white_multi",
#'       "nativity"
#'     ),
#'     header = "Misc",
#'     pattern_label_to_omit = "^(((NOT)|(NO))\\b.)|(U\\.S\\.\\-BORN)+"
#'   )
group_dichot_vars <- function(
  x,
  variables,
  header,
  pattern_label_to_omit = "^((NOT)|(NO))\\b.+",
  pattern_label_to_show = NULL
) {
  stopifnot(all(class(x) == c("tbl_summary", "gtsummary")))

  # if specified both pattern_label_to_omit and pattern_label_to_show, warn that
  # the former will be ingored
  if (!is.null(pattern_label_to_omit) & !is.null(pattern_label_to_show)) {
    warning(
      "Values specified for both `pattern_label_to_omit` and `pattern_label_to_show`. Note that, when the latter is specified, the former is ignored. (You can set it to NULL to suppress this warning in the future.)"
    )
  }

  # confirm the`x` stratifies by the expected non-mutually exclusive
  # race/ethnicity variables
  variables |>
    purrr::walk(.f = function(var) {
      if (!var %in% unique(x$table_body$variable)) {
        stop(paste0("tbl `x` does not stratify by variable `", var, "`."))
      }
    })

  stopifnot(is.character(header))
  stopifnot(length(header) == 1)

  y <- x |>
    gtsummary::add_variable_group_header(
      variables = variables,
      header = header
    ) |>
    gtsummary::modify_table_body(
      ~ .x |>
        # change the row type for the variable group header to 'label' so it
        # gets bolded the same as variable labels if/when
        # gtsummary::bold_labels() is subsequently applied
        dplyr::mutate(
          row_type = row_type |>
            dplyr::replace_when(
              label == header & row_type == 'variable_group' ~ 'label'
            )
        )
    )

  if (!is.null(pattern_label_to_show)) {
    y <- y |>
      gtsummary::modify_table_body(
        ~ .x |>
          # remove extraneous rows
          dplyr::filter_out(
            variable %in%
              variables &
              (# categories to omit
              !stringr::str_detect(
                label |> stringr::str_to_upper() |> stringr::str_trim(),
                pattern = pattern_label_to_show
              ) |
                # rows for Unknowns
                row_type == 'missing' |
                # rows showing variable labels
                row_type == "label")
          )
      )
  } else {
    y <- y |>
      gtsummary::modify_table_body(
        ~ .x |>
          # remove extraneous rows
          dplyr::filter_out(
            variable %in%
              variables &
              (# categories to omit
              stringr::str_detect(
                label |> stringr::str_to_upper() |> stringr::str_trim(),
                pattern = pattern_label_to_omit
              ) |
                # rows for Unknowns
                row_type == 'missing' |
                # rows showing variable labels
                row_type == "label")
          )
      )
  }

  # add a footnote for any Unknowns excluded ---
  names_stat_columns <- names(x$table_body) |>
    stringr::str_subset(pattern = "stat(_[0-9]+)+")

  if (length(names_stat_columns) == 1) {
    footnotes <- x$table_body |>
      dplyr::filter(row_type == 'missing') |>
      dplyr::mutate(
        footnote = paste0(
          var_label,
          ' unknown for n = ',
          .data[[names_stat_columns]]
        )
      ) |>
      dplyr::select(variable, footnote)
  } else {
    footnotes <- x$table_body |>
      dplyr::filter(row_type == 'missing') |>
      dplyr::rowwise() |>
      dplyr::mutate(
        footnote = paste0(
          var_label,
          ' unknown for (from left-most to right-most columns) n = ',
          paste(
            dplyr::c_across(cols = dplyr::all_of(c(names_stat_columns))),
            collapse = ", "
          )
        )
      ) |>
      dplyr::select(variable, footnote)
  }

  for (i in 1:nrow(footnotes)) {
    y <- y |>
      gtsummary::modify_table_styling(
        columns = label, # Target the row header column
        rows = variable == footnotes[i, "variable"] |> as.character(), # Select the specific row header text
        footnote = footnotes["footnote"][i] |> as.character()
      )
  }

  return(y)
}
