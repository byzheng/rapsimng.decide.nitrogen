
.section_yield_spec <- function() {
	list(
		name = "yield",
		title = "Yield",
		description = "Yield summary of the cultivar suitability evaluation.",
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
	data <- state$data |>
		dplyr::mutate(
			yield = .data[[state$columns$yield]] / 100,
			fertilisation = as.numeric(.data[[state$columns$fertilisation]])
		)

	# Calculate quantile breaks across ALL data (not per N level)
	quantiles <- stats::quantile(data$yield, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
	data <- data |>
		dplyr::mutate(
			yield_group = dplyr::case_when(
				.data$yield <= quantiles[2] ~ "Low",
				.data$yield <= quantiles[3] ~ "Mid",
				.data$yield > quantiles[3] ~ "High",
				TRUE ~ NA_character_
			)
		) |>
		dplyr::mutate(
			yield_group = factor(.data$yield_group, levels = c("Low", "Mid", "High"))
		)

	# Group by yield_group and fertilisation
	data |>
		dplyr::group_by(.data$yield_group, .data$fertilisation) |>
		dplyr::summarise(
			yield_mean = mean(.data$yield, na.rm = TRUE),
			yield_sd = stats::sd(.data$yield, na.rm = TRUE),
			yield_cv = ifelse(yield_mean != 0, yield_sd / yield_mean, NA_real_),
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
			"The average yield across all years for each fertilisation level.",
			"The standard deviation of yield across all years for each fertilisation level.",
			"The coefficient of variation of yield across all years for each fertilisation level, calculated as the standard deviation divided by the mean.",
			paste0("The proportion of years where the yield was below the failure threshold (", state$criteria$failure$yield_threshold, " t/ha), indicating the risk of poor performance for each fertilisation level."),
			"The 5th percentile of yield across all years for each fertilisation level, representing a low yield scenario.",
			"The 10th percentile of yield across all years for each fertilisation level, representing a very low yield scenario.",
			"The 25th percentile of yield across all years for each fertilisation level, representing a below-average yield scenario.",
			"The median yield across all years for each fertilisation level, representing a typical yield scenario.",
			"The 75th percentile of yield across all years for each fertilisation level, representing an above-average yield scenario.",
			"The 90th percentile of yield across all years for each fertilisation level, representing a high yield scenario.",
			"The 95th percentile of yield across all years for each fertilisation level, representing a very high yield scenario."
		),
        unit = c("t/ha", "t/ha", "t/ha",  "", "t/ha", "t/ha", "t/ha", "t/ha", "t/ha", "t/ha", "t/ha")
    )

    list(
		name = "yield_summary",
		value = values,
		metric_def = metric_def,
		description = "The summary statistics of yield across all fertilisation levels and years."
	)
}

 .yield_summary_table_columns <- function() {
	 c("yield_mean", "yield_sd", "yield_cv", "yield_risk")
 }

 .yield_summary_group_column <- function(metrics) {
	 c("yield_group", "fertilisation")
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
	group_columns <- .yield_summary_group_column(metrics)
	labels <- .yield_summary_column_labels(metrics, columns)

	table_data <- metrics$value |>
		dplyr::arrange(yield_group, dplyr::desc(fertilisation)) |>
		dplyr::select(dplyr::all_of(c(group_columns, columns))) |>
		dplyr::mutate(
			dplyr::across(dplyr::all_of(columns), ~ round(.x, digits))
		)

	colnames(table_data) <- c("Yield Group", "Fertilisation", unname(labels[columns]))
	table_data
}

.render_yield_summary_table_markdown <- function(metrics) {
	table_data <- .yield_summary_table_data(metrics)
	as.character(knitr::kable(table_data, format = "pipe"))
}

.render_yield_summary_table_caption <- function() {
	": Summary statistics of yield performance across fertilisation levels. {#tbl-yield-summary}"
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
			"",
			"Goal:",
			"- Summarise nitrogen response across yield groups and fertilisation levels using aggregated (multi-year) simulation results.",
			"",
			"Context:",
			"- Yield performance is evaluated across nitrogen levels under climate variability.",
			"- Results are grouped into three yield outcome groups (Low, Mid, High) based on quantiles.",
			"- The table below provides summary statistics by group and nitrogen level.",
			"",
			"Focus:",
			"- Identify fertilisation levels that balance:",
			"- stable yield performance (consistency)",
			"- acceptable downside risk",
			"- Emphasise trade-offs rather than a single “best” nitrogen level.",
			"",
			"Key metrics:",
			"- Mean yield (central performance)",
			"- Variability (CV or SD)",
			"- Downside risk (proportion of years below threshold or low quantile)",
			"",
			"Interpretation guidance:",
			"- Compare nitrogen responses within each yield group (Low, Mid, High)",
			"- Highlight how nitrogen response differs across seasonal conditions",
			"- Describe patterns such as:",
			"- diminishing returns at higher N",
			"- increased variability at higher N",
			"- stability at moderate N levels",
			"",
			"Avoid:",
			"- Claiming optimal or recommended nitrogen rate",
			"- Over-emphasising extreme or rare outcomes",
			"- Interpreting beyond what simulation outputs support",
			"",
			"Style:",
			"- Concise and clear",
			"- Farming decision-oriented (practical interpretation)",
			"- Focus on risk vs reward rather than maximisation",
			"- Neutral and evidence-based (no prescriptive advice)",
			"",
			"-->",
			"",
			.render_yield_summary_table_markdown(metrics),
			"",
			.render_yield_summary_table_caption(),
			"",
			.render_yield_summary_metric_notes(metrics),
			"",
			"Yield distribution across fertilisation levels and yield groups shown using quantile-based boxplots.",
			"",
			"```{r}",
			"#| label: fig-yield-summary-plot",
			"#| fig-cap: 'Yield summary across fertilisation levels and yield groups'",
			plot_data_lines,
			"yield_summary_plot_data <- yield_summary_data |>",
			"    dplyr::mutate(fertilisation = as.numeric(.data$fertilisation)) |>",
			"    dplyr::arrange(fertilisation, dplyr::desc(fertilisation)) |>",
			"    dplyr::mutate(fertilisation = factor(fertilisation, levels = sort(unique(fertilisation), decreasing = TRUE)))",
			"ggplot2::ggplot(",
			"    yield_summary_plot_data,",
			"    ggplot2::aes(",
			"        x = fertilisation,",
			"        ymin = yield_q5,",
			"        lower = yield_q25,",
			"        middle = yield_median,",
			"        upper = yield_q75,",
			"        ymax = yield_q95",
			"    )",
			") +",
			"    ggplot2::geom_boxplot(stat = \"identity\") +",
			"    ggplot2::coord_flip() +",
			"    ggplot2::labs(y = \"Yield (t/ha)\", x = \"Fertilisation\") +",
			"    ggplot2::facet_wrap(~yield_group, nrow = 1)",
			"```"
		)
	)
}