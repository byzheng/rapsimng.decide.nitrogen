.validate_inputs <- function(data, context, criteria, options) {
    if (!is.data.frame(data)) {
        stop("`data` must be a data.frame or tibble.", call. = FALSE)
    }

    if (nrow(data) == 0) {
        stop("`data` must contain at least one row.", call. = FALSE)
    }

    vars <- context$vars
    required_columns <- c(
        vars$cultivar_col,
        vars$year_col,
        vars$sowing_col,
        vars$flower_col,
        vars$yield_col,
        vars$frost_reduction_col,
        vars$heat_reduction_col,
        vars$frost_event_col,
        vars$heat_event_col
    )
    missing_columns <- setdiff(required_columns, names(data))

    if (length(missing_columns) > 0) {
        stop(
        sprintf(
            "Missing required columns: %s.",
            paste(missing_columns, collapse = ", ")
        ),
        call. = FALSE
        )
    }

    # yield_values <- data[[vars$yield_col]]
    # if (!is.numeric(yield_values)) {
    #     stop("The yield column must be numeric.", call. = FALSE)
    # }

    # optional_columns <- c(vars$frost_col, vars$heat_col)
    # for (column_name in optional_columns) {
    #     if (!column_name %in% names(data)) {
    #     next
    #     }

    #     if (!is.numeric(data[[column_name]]) && !is.logical(data[[column_name]])) {
    #     stop(
    #         sprintf("Optional risk column '%s' must be numeric or logical.", column_name),
    #         call. = FALSE
    #     )
    #     }
    # }

    failure_threshold <- criteria$failure$yield_threshold
    if (!is.null(failure_threshold)) {
        if (!is.numeric(failure_threshold) || length(failure_threshold) != 1L || is.na(failure_threshold)) {
        stop("`criteria$failure$yield_threshold` must be a single numeric value.", call. = FALSE)
        }
    }

    # if (!is.null(criteria$filter) && !is.list(criteria$filter)) {
    #     stop("`criteria$filter` must be NULL or a named list.", call. = FALSE)
    # }

    # logical_options <- c("include_metrics", "include_tables", "include_figures")
    # for (option_name in logical_options) {
    #     option_value <- options[[option_name]]
    #     if (!is.logical(option_value) || length(option_value) != 1L || is.na(option_value)) {
    #     stop(sprintf("`options$%s` must be TRUE or FALSE.", option_name), call. = FALSE)
    #     }
    # }
}
