.section_stress_spec <- function() {
	list(
		name = "stress",
		title = "Frost and Heat Stresses",
		description = "Frost and heat stress summary of the sowing window suitability evaluation.",
		evaluate = .evaluate_section_stress,
		document = .document_section_stress
	)
}

.evaluate_section_stress <- function(state, spec = .section_stress_spec()) {
	list(
		name = spec$name,
		title = spec$title,
		description = spec$description,
		metrics = list(
			frost_summary = .build_section_frost_metrics(state),
			heat_summary = .build_section_heat_metrics(state)
		)
	)
}

.compute_frost_summary <- function(state) {
	state$data |>
		dplyr::mutate(
			frost_reduction = .data[[state$columns$frost_reduction]],
			frost_events = .data[[state$columns$frost_events]]
		) |>
		dplyr::group_by(.data[[state$columns$sowing_window]]) |>
		dplyr::summarise(
			frost_reduction_mean = mean(.data$frost_reduction, na.rm = TRUE),
			frost_reduction_sd = stats::sd(.data$frost_reduction, na.rm = TRUE),
			frost_reduction_cv = ifelse(.data$frost_reduction_mean != 0, .data$frost_reduction_sd / .data$frost_reduction_mean, NA_real_),
			frost_event_mean = mean(.data$frost_events, na.rm = TRUE),
			frost_event_sd = stats::sd(.data$frost_events, na.rm = TRUE),
			frost_event_cv = ifelse(.data$frost_event_mean != 0, .data$frost_event_sd / .data$frost_event_mean, NA_real_),
			frost_reduction_q5 = stats::quantile(.data$frost_reduction, 0.05, na.rm = TRUE),
			frost_reduction_q10 = stats::quantile(.data$frost_reduction, 0.10, na.rm = TRUE),
			frost_reduction_q25 = stats::quantile(.data$frost_reduction, 0.25, na.rm = TRUE),
			frost_reduction_median = stats::median(.data$frost_reduction, na.rm = TRUE),
			frost_reduction_q75 = stats::quantile(.data$frost_reduction, 0.75, na.rm = TRUE),
			frost_reduction_q90 = stats::quantile(.data$frost_reduction, 0.90, na.rm = TRUE),
			frost_reduction_q95 = stats::quantile(.data$frost_reduction, 0.95, na.rm = TRUE),
			.groups = "drop"
		)
}

.build_section_frost_metrics <- function(state) {
	values <- .compute_frost_summary(state)

	metric_def <- tibble::tibble(
		name = c(
			"frost_reduction_mean", "frost_reduction_sd", "frost_reduction_cv",
			"frost_event_mean", "frost_event_sd", "frost_event_cv",
			"frost_reduction_q5", "frost_reduction_q10", "frost_reduction_q25",
			"frost_reduction_median", "frost_reduction_q75", "frost_reduction_q90", "frost_reduction_q95"
		),
		title = c(
			"Average Frost Reduction Ratio", "Frost Reduction Ratio Standard Deviation", "Frost Reduction Ratio Coefficient of Variation",
			"Average Frost Event Number", "Frost Event Number Standard Deviation", "Frost Event Number Coefficient of Variation",
			"5th Percentile Frost Reduction Ratio", "10th Percentile Frost Reduction Ratio", "25th Percentile Frost Reduction Ratio",
			"Median Frost Reduction Ratio", "75th Percentile Frost Reduction Ratio", "90th Percentile Frost Reduction Ratio", "95th Percentile Frost Reduction Ratio"
		),
		   description = c(
			   "The average cumulative frost reduction ratio across all years for each sowing window.",
			   "The standard deviation of cumulative frost reduction ratio across all years for each sowing window.",
			   "The coefficient of variation of cumulative frost reduction ratio across all years for each sowing window.",
			   "The average number of frost events across all years for each sowing window.",
			   "The standard deviation of frost event number across all years for each sowing window.",
			   "The coefficient of variation of frost event number across all years for each sowing window.",
			   "The 5th percentile of cumulative frost reduction ratio across all years for each sowing window.",
			   "The 10th percentile of cumulative frost reduction ratio across all years for each sowing window.",
			   "The 25th percentile of cumulative frost reduction ratio across all years for each sowing window.",
			   "The median cumulative frost reduction ratio across all years for each sowing window.",
			   "The 75th percentile of cumulative frost reduction ratio across all years for each sowing window.",
			   "The 90th percentile of cumulative frost reduction ratio across all years for each sowing window.",
			   "The 95th percentile of cumulative frost reduction ratio across all years for each sowing window."
		),
		unit = c("", "", "", "events", "events", "", "", "", "", "", "", "", "")
	)

	list(
		name = "frost_summary",
		value = values,
		metric_def = metric_def,
		description = "The summary statistics of frost stress across all sowing windows and years."
	)
}

.compute_heat_summary <- function(state) {
	state$data |>
		dplyr::mutate(
			heat_reduction = .data[[state$columns$heat_reduction]],
			heat_events = .data[[state$columns$heat_events]]
		) |>
		dplyr::group_by(.data[[state$columns$sowing_window]]) |>
		dplyr::summarise(
			heat_reduction_mean = mean(.data$heat_reduction, na.rm = TRUE),
			heat_reduction_sd = stats::sd(.data$heat_reduction, na.rm = TRUE),
			heat_reduction_cv = ifelse(.data$heat_reduction_mean != 0, .data$heat_reduction_sd / .data$heat_reduction_mean, NA_real_),
			heat_event_mean = mean(.data$heat_events, na.rm = TRUE),
			heat_event_sd = stats::sd(.data$heat_events, na.rm = TRUE),
			heat_event_cv = ifelse(.data$heat_event_mean != 0, .data$heat_event_sd / .data$heat_event_mean, NA_real_),
			heat_reduction_q5 = stats::quantile(.data$heat_reduction, 0.05, na.rm = TRUE),
			heat_reduction_q10 = stats::quantile(.data$heat_reduction, 0.10, na.rm = TRUE),
			heat_reduction_q25 = stats::quantile(.data$heat_reduction, 0.25, na.rm = TRUE),
			heat_reduction_median = stats::median(.data$heat_reduction, na.rm = TRUE),
			heat_reduction_q75 = stats::quantile(.data$heat_reduction, 0.75, na.rm = TRUE),
			heat_reduction_q90 = stats::quantile(.data$heat_reduction, 0.90, na.rm = TRUE),
			heat_reduction_q95 = stats::quantile(.data$heat_reduction, 0.95, na.rm = TRUE),
			.groups = "drop"
		)
}

.build_section_heat_metrics <- function(state) {
	values <- .compute_heat_summary(state)

	metric_def <- tibble::tibble(
		name = c(
			"heat_reduction_mean", "heat_reduction_sd", "heat_reduction_cv",
			"heat_event_mean", "heat_event_sd", "heat_event_cv",
			"heat_reduction_q5", "heat_reduction_q10", "heat_reduction_q25",
			"heat_reduction_median", "heat_reduction_q75", "heat_reduction_q90", "heat_reduction_q95"
		),
		title = c(
			"Average Heat Reduction Ratio", "Heat Reduction Ratio Standard Deviation", "Heat Reduction Ratio Coefficient of Variation",
			"Average Heat Event Number", "Heat Event Number Standard Deviation", "Heat Event Number Coefficient of Variation",
			"5th Percentile Heat Reduction Ratio", "10th Percentile Heat Reduction Ratio", "25th Percentile Heat Reduction Ratio",
			"Median Heat Reduction Ratio", "75th Percentile Heat Reduction Ratio", "90th Percentile Heat Reduction Ratio", "95th Percentile Heat Reduction Ratio"
		),
		   description = c(
			   "The average cumulative heat reduction ratio across all years for each sowing window.",
			   "The standard deviation of cumulative heat reduction ratio across all years for each sowing window.",
			   "The coefficient of variation of cumulative heat reduction ratio across all years for each sowing window.",
			   "The average number of heat events across all years for each sowing window.",
			   "The standard deviation of heat event number across all years for each sowing window.",
			   "The coefficient of variation of heat event number across all years for each sowing window.",
			   "The 5th percentile of cumulative heat reduction ratio across all years for each sowing window.",
			   "The 10th percentile of cumulative heat reduction ratio across all years for each sowing window.",
			   "The 25th percentile of cumulative heat reduction ratio across all years for each sowing window.",
			   "The median cumulative heat reduction ratio across all years for each sowing window.",
			   "The 75th percentile of cumulative heat reduction ratio across all years for each sowing window.",
			   "The 90th percentile of cumulative heat reduction ratio across all years for each sowing window.",
			   "The 95th percentile of cumulative heat reduction ratio across all years for each sowing window."
		),
		unit = c("", "", "", "events", "events", "", "", "", "", "", "", "", "")
	)

	list(
		name = "heat_summary",
		value = values,
		metric_def = metric_def,
		description = "The summary statistics of heat stress across all sowing windows and years."
	)
}

.stress_summary_table_columns <- function(prefix) {
	c(
		paste0(prefix, "_reduction_mean"),
		paste0(prefix, "_reduction_sd"),
		paste0(prefix, "_event_mean"),
		paste0(prefix, "_event_sd")
	)
}

.stress_summary_group_column <- function(metrics) {
	names(metrics$value)[[1]]
}

.stress_summary_column_labels <- function(metrics, columns) {
	defs <- metrics$metric_def |>
		dplyr::filter(.data$name %in% columns)

	labels <- vapply(columns, function(column_name) {
		row <- defs[defs$name == column_name, , drop = FALSE]
		if (nrow(row) == 0) {
			return(column_name)
		}

		unit <- row$unit[[1]]
		title <- row$title[[1]]
		if (is.na(unit) || !nzchar(unit)) {
			return(title)
		}

		paste0(title, " (", unit, ")")
	}, character(1))

	stats::setNames(labels, columns)
}

.stress_summary_table_data <- function(metrics, prefix, digits = 2) {
	columns <- .stress_summary_table_columns(prefix)
	group_column <- .stress_summary_group_column(metrics)
	labels <- .stress_summary_column_labels(metrics, columns)
	mean_column <- paste0(prefix, "_reduction_mean")

	table_data <- metrics$value |>
		dplyr::arrange(dplyr::desc(.data[[mean_column]])) |>
		dplyr::select(dplyr::all_of(c(group_column, columns))) |>
		dplyr::mutate(
			dplyr::across(dplyr::all_of(columns), ~ round(.x, digits))
		)

	colnames(table_data) <- c("SowingWindow", unname(labels[columns]))
	table_data
}

.render_stress_summary_table_markdown <- function(metrics, prefix) {
	table_data <- .stress_summary_table_data(metrics, prefix)
	as.character(knitr::kable(table_data, format = "pipe"))
}

.render_stress_summary_metric_notes <- function(metrics, prefix) {
	columns <- .stress_summary_table_columns(prefix)
	defs <- metrics$metric_def |>
		dplyr::filter(.data$name %in% columns)

	vapply(columns, function(column_name) {
		row <- defs[defs$name == column_name, , drop = FALSE]
		if (nrow(row) == 0) {
			return(paste0("- ", column_name))
		}

		unit <- row$unit[[1]]
		title <- row$title[[1]]
		label <- if (is.na(unit) || !nzchar(unit)) {
			title
		} else {
			paste0(title, " (", unit, ")")
		}

		paste0("- ", label, ": ", row$description[[1]])
	}, character(1))
}

.document_section_stress <- function(section, meta = NULL) {
	frost_summary <- .document_frost_summary(section, meta)
	heat_summary <- .document_heat_summary(section, meta)

	list(
		name = section$name,
		title = section$title,
		body = c(
			paste0("## ", section$title),
			"",
			frost_summary$body,
			"",
			heat_summary$body
		)
	)
}

.document_frost_summary <- function(section, meta = NULL) {
	metrics <- section$metrics$frost_summary
	plot_data_lines <- if (.document_uses_replay(meta)) {
		c(
			"stress_section <- get_section(\"stress\")",
			"frost_summary_metrics <- stress_section$metrics$frost_summary",
			"frost_summary_data <- frost_summary_metrics$value"
		)
	} else {
		c(
			"frost_summary_metrics <-",
			utils::capture.output(dput(metrics)),
			"frost_summary_data <- frost_summary_metrics$value"
		)
	}

	list(
		name = "frost_summary",
		title = "Frost Stress Summary",
		body = c(
			"### Frost Stress",
			"",
			"<!--",
			"Narrative:",
			   "- Goal: describe frost stress patterns across sowing windows",
			   "- Context: cumulative frost reduction ratios and frost event counts across sowing windows",
			"- Focus: frequency and severity patterns without claiming causality beyond reported metrics",
			"- Key metrics: mean reduction ratio, mean event number, and variability",
			"- Style: concise, descriptive, and neutral for decision support",
			"-->",
			"",
			.render_stress_summary_table_markdown(metrics, "frost"),
			"",
			   ": Summary statistics of frost stress across sowing windows. {#tbl-frost-summary}",
			"",
			.render_stress_summary_metric_notes(metrics, "frost"),
			"",
			   "Frost reduction ratio distribution across sowing windows shown using quantile-based boxplots.",
			"",
			"```{r}",
			"#| label: fig-frost-summary-plot",
			   "#| fig-cap: 'Frost stress summary across sowing windows'",
			plot_data_lines,
			   "sowing_window_column <- names(frost_summary_data)[[1]]",
			   "frost_summary_plot_data <- frost_summary_data |>",
			   "    dplyr::rename(sowing_window = dplyr::all_of(sowing_window_column)) |>",
			   "    dplyr::arrange(dplyr::desc(frost_reduction_mean)) |>",
			   "    dplyr::mutate(sowing_window = forcats::fct_reorder(sowing_window, frost_reduction_mean, .desc = TRUE))",
			   "ggplot2::ggplot(",
			   "    frost_summary_plot_data,",
			   "    ggplot2::aes(",
			   "        x = sowing_window,",
			"        ymin = frost_reduction_q5,",
			"        lower = frost_reduction_q25,",
			"        middle = frost_reduction_median,",
			"        upper = frost_reduction_q75,",
			"        ymax = frost_reduction_q95",
			"    )",
			") +",
			"    ggplot2::geom_boxplot(stat = \"identity\") +",
			"    ggplot2::coord_flip() +",
			   "    ggplot2::labs(y = \"Frost reduction ratio\", x = \"Sowing Window\")",
			"```"
		)
	)
}

.document_heat_summary <- function(section, meta = NULL) {
	metrics <- section$metrics$heat_summary
	plot_data_lines <- if (.document_uses_replay(meta)) {
		c(
			"stress_section <- get_section(\"stress\")",
			"heat_summary_metrics <- stress_section$metrics$heat_summary",
			"heat_summary_data <- heat_summary_metrics$value"
		)
	} else {
		c(
			"heat_summary_metrics <-",
			utils::capture.output(dput(metrics)),
			"heat_summary_data <- heat_summary_metrics$value"
		)
	}

	list(
		name = "heat_summary",
		title = "Heat Stress Summary",
		body = c(
			"### Heat Stress",
			"",
			"<!--",
			"Narrative:",
			   "- Goal: describe heat stress patterns across sowing windows",
			   "- Context: cumulative heat reduction ratios and heat event counts across sowing windows",
			"- Focus: frequency and severity patterns without claiming causality beyond reported metrics",
			"- Key metrics: mean reduction ratio, mean event number, and variability",
			"- Style: concise, descriptive, and neutral for decision support",
			"-->",
			"",
			.render_stress_summary_table_markdown(metrics, "heat"),
			"",
			   ": Summary statistics of heat stress across sowing windows. {#tbl-heat-summary}",
			"",
			.render_stress_summary_metric_notes(metrics, "heat"),
			"",
			   "Heat reduction ratio distribution across sowing windows shown using quantile-based boxplots.",
			"",
			"```{r}",
			"#| label: fig-heat-summary-plot",
			   "#| fig-cap: 'Heat stress summary across sowing windows'",
			plot_data_lines,
			   "sowing_window_column <- names(heat_summary_data)[[1]]",
			   "heat_summary_plot_data <- heat_summary_data |>",
			   "    dplyr::rename(sowing_window = dplyr::all_of(sowing_window_column)) |>",
			   "    dplyr::arrange(dplyr::desc(heat_reduction_mean)) |>",
			   "    dplyr::mutate(sowing_window = forcats::fct_reorder(sowing_window, heat_reduction_mean, .desc = TRUE))",
			   "ggplot2::ggplot(",
			   "    heat_summary_plot_data,",
			   "    ggplot2::aes(",
			   "        x = sowing_window,",
			"        ymin = heat_reduction_q5,",
			"        lower = heat_reduction_q25,",
			"        middle = heat_reduction_median,",
			"        upper = heat_reduction_q75,",
			"        ymax = heat_reduction_q95",
			"    )",
			") +",
			"    ggplot2::geom_boxplot(stat = \"identity\") +",
			"    ggplot2::coord_flip() +",
			   "    ggplot2::labs(y = \"Heat reduction ratio\", x = \"Sowing Window\")",
			"```"
		)
	)
}