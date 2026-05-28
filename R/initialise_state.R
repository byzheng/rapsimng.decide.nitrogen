.initialise_state <- function(data, context, criteria, options, extras = list()) {
  vars <- context$vars
  # frost_column <- .resolve_optional_column(data, vars$frost_col)
  # heat_column <- .resolve_optional_column(data, vars$heat_col)

  notes <- character()
  # if (is.null(frost_column)) {
  #   notes <- .add_note(
  #     notes,
  #     sprintf(
  #       "Optional frost column '%s' not found; frost metrics and tables are returned as NA.",
  #       vars$frost_col
  #     )
  #   )
  # }
  # if (is.null(heat_column)) {
  #   notes <- .add_note(
  #     notes,
  #     sprintf(
  #       "Optional heat column '%s' not found; heat metrics and tables are returned as NA.",
  #       vars$heat_col
  #     )
  #   )
  # }
  # if (is.null(criteria$failure$yield_threshold)) {
  #   notes <- .add_note(
  #     notes,
  #     "No failure yield threshold provided; failure probabilities are returned as NA."
  #   )
  # }

  list(
    data = data,
    context = context,
    criteria = criteria,
    options = options,
    extras = extras,
    vars = vars,
    columns = list(
      sowing_window = vars$sowing_col,
      year = vars$year_col,
      sowing = vars$sowing_col,
      flower = vars$flower_col,
      yield = vars$yield_col,
      frost_reduction = vars$frost_reduction_col,
      heat_reduction = vars$heat_reduction_col,
      frost_events = vars$frost_event_col,
      heat_events = vars$heat_event_col
    ),
    notes = notes,
    cache = new.env(parent = emptyenv())
  )
}
