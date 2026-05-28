
.section_yield_spec <- function() {
	list(
		name = "yield",
		title = "Yield",
		description = "Yield summary of the sowing window suitability evaluation.",
		evaluate = .evaluate_section_yield,
		document = .document_section_yield
	)
}

.evaluate_section_yield <- function(state, spec = .section_yield_spec()) {
	list(
		name = spec$name,
		title = spec$title,
		description = spec$description,
		metrics = list(
			yield_summary = .build_section_yield_metrics(state)
		)
	)
}

.compute_yield_summary <- function(state) {
    state$data |>
        dplyr::mutate(
            yield = .data[[state$columns$yield]] / 100
        ) |>
        dplyr::group_by(.data[[state$columns$sowing]]) |>
        dplyr::summarise(
            yield_mean = mean(.data$yield, na.rm = TRUE),
            yield_sd = stats::sd(.data$yield, na.rm = TRUE),
			yield_cv = ifelse(.data$yield_mean != 0, .data$yield_sd / .data$yield_mean, NA_real_),
			yield_risk = sum(.data$yield < state$criteria$failure$yield_threshold, na.rm = TRUE) / sum(!is.na(.data$yield)),
            yield_q5 = stats::quantile(.data$yield, 0.05, na.rm = TRUE),
            yield_q10 = stats::quantile(.data$yield, 0.10, na.rm = TRUE),
			yield_q25 = stats::quantile(.data$yield, 0.25, na.rm = TRUE),
			yield_median = stats::median(.data$yield, na.rm = TRUE),
			yield_q75 = stats::quantile(.data$yield, 0.75, na.rm = TRUE),
			yield_q90 = stats::quantile(.data$yield, 0.90, na.rm = TRUE),
			yield_q95 = stats::quantile(.data$yield, 0.95, na.rm = TRUE),
            .groups = "drop"
        )
}

.build_section_yield_metrics <- function(state) {

    values <- .compute_yield_summary(state)

    metric_def <- tibble::tibble(
        name = c("yield_mean", "yield_sd", "yield_cv", "yield_risk", "yield_q5", "yield_q10", "yield_q25", "yield_median", "yield_q75", "yield_q90", "yield_q95"),
        title = c("Average Yield", "Yield Standard Deviation", "Yield Coefficient of Variation", "Yield Risk", "5th Percentile Yield", "10th Percentile Yield", "25th Percentile Yield", "Median Yield", "75th Percentile Yield", "90th Percentile Yield", "95th Percentile Yield"),
		description = c(
			"The average yield across all years for each sowing window.",
			"The standard deviation of yield across all years for each sowing window.",
			"The coefficient of variation of yield across all years for each sowing window, calculated as the standard deviation divided by the mean.",
			paste0("The proportion of years where the yield was below the failure threshold (", state$criteria$failure$yield_threshold, " t/ha), indicating the risk of poor performance."),
			"The 5th percentile of yield across all years for each sowing window, representing a low yield scenario.",
			"The 10th percentile of yield across all years for each sowing window, representing a very low yield scenario.",
			"The 25th percentile of yield across all years for each sowing window, representing a below-average yield scenario.",
			"The median yield across all years for each sowing window, representing a typical yield scenario.",
			"The 75th percentile of yield across all years for each sowing window, representing an above-average yield scenario.",
			"The 90th percentile of yield across all years for each sowing window, representing a high yield scenario.",
			"The 95th percentile of yield across all years for each sowing window, representing a very high yield scenario."
		),
        unit = c("t/ha", "t/ha", "t/ha",  "", "t/ha", "t/ha", "t/ha", "t/ha", "t/ha", "t/ha", "t/ha")
    )

    list(
		name = "yield_summary",
		value = values,
		metric_def = metric_def,
		description = "The summary statistics of yield across all sowing windows and years impacted by frost and heat stresses."
	)
}

.yield_summary_table_columns <- function() {
	c("yield_mean", "yield_sd", "yield_cv", "yield_risk")
}

.yield_summary_group_column <- function(metrics) {
	names(metrics$value)[[1]]
}

.yield_summary_column_labels <- function(metrics, columns) {
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

.yield_summary_table_data <- function(metrics, digits = 2) {
	columns <- .yield_summary_table_columns()
	group_column <- .yield_summary_group_column(metrics)
	labels <- .yield_summary_column_labels(metrics, columns)

	table_data <- metrics$value |>
		dplyr::arrange(dplyr::desc(.data$yield_mean)) |>
		dplyr::select(dplyr::all_of(c(group_column, columns))) |>
		dplyr::mutate(
			dplyr::across(dplyr::all_of(columns), ~ round(.x, digits))
		)

	colnames(table_data) <- c("Sowing Window", unname(labels[columns]))
	table_data
}

.render_yield_summary_table_markdown <- function(metrics) {
	table_data <- .yield_summary_table_data(metrics)
	as.character(knitr::kable(table_data, format = "pipe"))
}

.render_yield_summary_table_caption <- function() {
	": Summary statistics of yield performance across sowing windows	. {#tbl-yield-summary}"
}

.render_yield_summary_metric_notes <- function(metrics) {
	columns <- .yield_summary_table_columns()
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

.document_section_yield <- function(section, meta = NULL) {
	yield_summary <- .document_yield_summary(section, meta)

	list(
		name = section$name,
		title = section$title,
		body = c(
			paste0("## ", section$title),
			"",
			# section$description,
			"",
			# paste0("### ", yield_summary$title),
			"",
			yield_summary$body
		)
	)
}


.document_yield_summary <- function(section, meta = NULL) {
	metrics <- section$metrics$yield_summary
	plot_data_lines <- if (.document_uses_replay(meta)) {
		c(
			"yield_section <- get_section(\"yield\")",
			"yield_summary_metrics <- yield_section$metrics$yield_summary",
			"yield_summary_data <- yield_summary_metrics$value"
		)
	} else {
		c(
			"yield_summary_metrics <-",
			utils::capture.output(dput(metrics)),
			"yield_summary_data <- yield_summary_metrics$value"
		)
	}

	list(
		name = "yield_summary",
		title = "Yield Summary",
		body = c(
			"<!--",
			"Narrative:",
			"- Goal: summarise",
			"- Context: yield risk performance across sowing windows (use table below)",
			"- Focus: high yield AND low risk sowing windows",
			"- Key metrics: mean yield, CV, downside risk (proportion of years below threshold)",
			"- Avoid: over-emphasising extreme outliers",
			"- Style: concise, farming decision oriented, decision-focused",
			"-->",
			"",
			.render_yield_summary_table_markdown(metrics),
			"",
			.render_yield_summary_table_caption(),
			"",
			.render_yield_summary_metric_notes(metrics),
			"",
			"Yield distribution across sowing windows shown using quantile-based boxplots.",
			"",
			"```{r}",
			"#| label: fig-yield-summary-plot",
			"#| fig-cap: 'Yield summary across sowing windows'",
			plot_data_lines,
			"sowing_window_column <- names(yield_summary_data)[[1]]",
			"yield_summary_plot_data <- yield_summary_data |>",
			"    dplyr::rename(sowing_window = dplyr::all_of(sowing_window_column)) |>",
			"    dplyr::arrange(dplyr::desc(yield_mean)) |>",
			"    dplyr::mutate(sowing_window = forcats::fct_reorder(sowing_window, yield_mean, .desc = TRUE))",
			"ggplot2::ggplot(",
			"    yield_summary_plot_data,",
			"    ggplot2::aes(",
			"        x = sowing_window,",
			"        ymin = yield_q5,",
			"        lower = yield_q25,",
			"        middle = yield_median,",
			"        upper = yield_q75,",
			"        ymax = yield_q95",
			"    )",
			") +",
			"    ggplot2::geom_boxplot(stat = \"identity\") +",
			"    ggplot2::coord_flip() +",
			"    ggplot2::labs(y = \"Yield (t/ha)\", x = \"Sowing Window\")",
			"```"
		)
	)
}