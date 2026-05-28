.section_flower_spec <- function() {
	list(
		name = "flower",
		title = "Flowering",
		description = "Flowering time summary of the sowing window suitability evaluation.",
		evaluate = .evaluate_section_flower,
		document = .document_section_flower
	)
}

.evaluate_section_flower <- function(state, spec = .section_flower_spec()) {
	list(
		name = spec$name,
		title = spec$title,
		description = spec$description,
		metrics = list(
			flower_summary = .build_section_flower_metrics(state)
		)
	)
}

.compute_flower_summary <- function(state) {
	state$data |>
		dplyr::mutate(
			flower = .data[[state$columns$flower]]
		) |>
		dplyr::group_by(.data[[state$columns$sowing_window]]) |>
		dplyr::summarise(
			flower_mean = mean(.data$flower, na.rm = TRUE),
			flower_sd = stats::sd(.data$flower, na.rm = TRUE),
			flower_cv = ifelse(.data$flower_mean != 0, .data$flower_sd / .data$flower_mean, NA_real_),
			flower_q5 = stats::quantile(.data$flower, 0.05, na.rm = TRUE),
			flower_q10 = stats::quantile(.data$flower, 0.10, na.rm = TRUE),
			flower_q25 = stats::quantile(.data$flower, 0.25, na.rm = TRUE),
			flower_median = stats::median(.data$flower, na.rm = TRUE),
			flower_q75 = stats::quantile(.data$flower, 0.75, na.rm = TRUE),
			flower_q90 = stats::quantile(.data$flower, 0.90, na.rm = TRUE),
			flower_q95 = stats::quantile(.data$flower, 0.95, na.rm = TRUE),
			.groups = "drop"
		)
}

.build_section_flower_metrics <- function(state) {
	values <- .compute_flower_summary(state)

	metric_def <- tibble::tibble(
		name = c("flower_mean", "flower_sd", "flower_cv", "flower_q5", "flower_q10", "flower_q25", "flower_median", "flower_q75", "flower_q90", "flower_q95"),
		title = c("Average Flowering Time", "Flowering Time Standard Deviation", "Flowering Time Coefficient of Variation", "5th Percentile Flowering Time", "10th Percentile Flowering Time", "25th Percentile Flowering Time", "Median Flowering Time", "75th Percentile Flowering Time", "90th Percentile Flowering Time", "95th Percentile Flowering Time"),
		   description = c(
			   "The average flowering time across all years for each sowing window.",
			   "The standard deviation of flowering time across all years for each sowing window.",
			   "The coefficient of variation of flowering time across all years for each sowing window, calculated as the standard deviation divided by the mean.",
			   "The 5th percentile of flowering time across all years for each sowing window, representing an early flowering scenario.",
			   "The 10th percentile of flowering time across all years for each sowing window, representing a very early flowering scenario.",
			   "The 25th percentile of flowering time across all years for each sowing window, representing a below-average flowering time scenario.",
			   "The median flowering time across all years for each sowing window, representing a typical flowering time scenario.",
			   "The 75th percentile of flowering time across all years for each sowing window, representing an above-average flowering time scenario.",
			   "The 90th percentile of flowering time across all years for each sowing window, representing a late flowering scenario.",
			   "The 95th percentile of flowering time across all years for each sowing window, representing a very late flowering scenario."
		),
		unit = c("days", "days", "", "days", "days", "days", "days", "days", "days", "days")
	)

	list(
		name = "flower_summary",
		value = values,
		metric_def = metric_def,
		description = "The summary statistics of flowering time across all sowing windows and years."
	)
}

.flower_summary_table_columns <- function() {
	c("flower_mean", "flower_sd", "flower_cv")
}

.flower_summary_group_column <- function(metrics) {
	names(metrics$value)[[1]]
}

.flower_summary_column_labels <- function(metrics, columns) {
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

.flower_summary_table_data <- function(metrics, digits = 2) {
	columns <- .flower_summary_table_columns()
	group_column <- .flower_summary_group_column(metrics)
	labels <- .flower_summary_column_labels(metrics, columns)

	table_data <- metrics$value |>
		dplyr::arrange(.data$flower_mean) |>
		dplyr::select(dplyr::all_of(c(group_column, columns))) |>
		dplyr::mutate(
			dplyr::across(dplyr::all_of(columns), ~ round(.x, digits))
		)

	colnames(table_data) <- c("SowingWindow", unname(labels[columns]))
	table_data
}

.render_flower_summary_table_markdown <- function(metrics) {
	table_data <- .flower_summary_table_data(metrics)
	as.character(knitr::kable(table_data, format = "pipe"))
}

.render_flower_summary_table_caption <- function() {
	": Summary statistics of flowering time across sowing windows. {#tbl-flower-summary}"
}

.render_flower_summary_metric_notes <- function(metrics) {
	columns <- .flower_summary_table_columns()
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

.document_section_flower <- function(section, meta = NULL) {
	flower_summary <- .document_flower_summary(section, meta)

	list(
		name = section$name,
		title = section$title,
		body = c(
			paste0("## ", section$title),
			"",
			"",
			"",
			flower_summary$body
		)
	)
}

.document_flower_summary <- function(section, meta = NULL) {
	metrics <- section$metrics$flower_summary
	plot_data_lines <- if (.document_uses_replay(meta)) {
		c(
			"flower_section <- get_section(\"flower\")",
			"flower_summary_metrics <- flower_section$metrics$flower_summary",
			"flower_summary_data <- flower_summary_metrics$value"
		)
	} else {
		c(
			"flower_summary_metrics <-",
			utils::capture.output(dput(metrics)),
			"flower_summary_data <- flower_summary_metrics$value"
		)
	}

	list(
		name = "flower_summary",
		title = "Flowering Summary",
		body = c(
            "<!--",
			"Narrative:",
			"- Goal: describe flowering time patterns across sowing windows",
			"- Context: flowering timing across sowing windows (use table below)",
			"- Focus: differences in timing and variability without implying optimality",
			"- Key metrics: mean flowering time and spread (e.g. CV or quantiles)",
			"- Constraint: do not interpret earlier or later flowering as better or worse",
			"- Limit: no assessment of alignment with optimal flowering period (OFP) in this version",
			"- Style: concise, descriptive, and neutral for decision support",
            "-->",
			"",
			.render_flower_summary_table_markdown(metrics),
			"",
			.render_flower_summary_table_caption(),
			"",
			.render_flower_summary_metric_notes(metrics),
			"",
			"Flowering time distribution across sowing windows shown using quantile-based boxplots.",
			"",
			"```{r}",
			"#| label: fig-flower-summary-plot",
			"#| fig-cap: 'Flowering summary across sowing windows'",
			plot_data_lines,
			"sowing_window_column <- names(flower_summary_data)[[1]]",
			"flower_summary_plot_data <- flower_summary_data |>",
			"    dplyr::rename(sowing_window = dplyr::all_of(sowing_window_column)) |>",
			"    dplyr::arrange(flower_mean) |>",
			"    dplyr::mutate(sowing_window = forcats::fct_reorder(sowing_window, flower_mean))",
			"ggplot2::ggplot(",
			"    flower_summary_plot_data,",
			"    ggplot2::aes(",
			"        x = sowing_window,",
			"        ymin = flower_q5,",
			"        lower = flower_q25,",
			"        middle = flower_median,",
			"        upper = flower_q75,",
			"        ymax = flower_q95",
			"    )",
			") +",
			"    ggplot2::geom_boxplot(stat = \"identity\") +",
			"    ggplot2::coord_flip() +",
			"    ggplot2::labs(y = \"Flowering time (Days After Sowing)\", x = \"Sowing Window\")",
			"```"
		)
	)
}