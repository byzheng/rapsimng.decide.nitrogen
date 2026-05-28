#' Document sowing window suitability from APSIM NG outputs
#'
#' @description
#' Analyse APSIM NG outputs already loaded into R to assess sowing window suitability
#' under a defined environment and sowing window; returns a structured decision
#' report.
#'
#' @param data data.frame/tibble of APSIM outputs.
#' @param context list controlling variable mapping and reporting context.
#' @param criteria list controlling decision criteria, including optional risk and
#'   filtering thresholds.
#' @param options list controlling output toggles and figure behaviour.
#' @param ... additional values stored in report metadata for downstream use.
#' @export
document <- function(
	data,
	context = list(),
	criteria = list(),
	options = list(),
	...
) {
	report <- if (inherits(data, "rapsimng_decide_report")) {
		data
	} else {
		evaluate(
			data = data,
			context = context,
			criteria = criteria,
			options = options,
			...
		)
	}

	doc <- .assemble_document(report)
	doc
}

.document_title <- function(meta) {
	title <- meta$extras$title

	if (is.null(title) || !nzchar(title)) {
		title <- meta$context$meta$title
	}

	if (is.null(title) || !nzchar(title)) {
		title <- "Sowing Window Suitability Report"
	}

	title
}

.document_author <- function(meta) {
	author <- meta$extras$author

	if (is.null(author) || !nzchar(author)) {
		author <- meta$context$meta$author
	}

	author
}

.document_date <- function(meta) {
	date <- meta$extras$date

	if (is.null(date)) {
		date <- meta$context$meta$date
	}

	if (inherits(date, "Date")) {
		date <- as.character(date)
	}

	date
}

.document_yaml_string <- function(value) {
	paste0('"', gsub('"', '\\\\"', as.character(value), fixed = TRUE), '"')
}

.document_source <- function(meta) {
	source <- meta$source

	if (is.null(source)) {
		source <- meta$extras$source
	}

	if (is.null(source)) {
		context_meta <- meta$context$meta
		if (!is.null(context_meta$source) && !is.null(context_meta$report)) {
			source <- list(
				file = context_meta$source,
				report = context_meta$report
			)
		}
	}

	source
}

.document_uses_replay <- function(meta) {
	source <- .document_source(meta)
	!is.null(source) && !is.null(source$file) && !is.null(source$report)
}

.document_object_lines <- function(name, value) {
	c(
		paste0(name, " <-"),
		utils::capture.output(dput(value))
	)
}

.document_yaml_lines <- function(meta) {
	lines <- c(
		"---",
		paste0("title: ", .document_yaml_string(.document_title(meta)))
	)

	author <- .document_author(meta)
	if (!is.null(author) && nzchar(author)) {
		lines <- c(lines, paste0("author: ", .document_yaml_string(author)))
	}

	date <- .document_date(meta)
	if (!is.null(date) && nzchar(date)) {
		lines <- c(lines, paste0("date: ", .document_yaml_string(date)))
	}

	c(
		lines,
		"format: html",
		"execute:",
		"  echo: false",
		"  eval: true",
		"  message: false",
		"  warning: false",
		"---",
		""
	)
}

.document_setup_chunk_snapshot <- function(meta) {
	data_lines <- .document_object_lines("data", meta$data)
	context_lines <- .document_object_lines("context", meta$context)
	criteria_lines <- .document_object_lines("criteria", meta$criteria)
	options_lines <- .document_object_lines("options", meta$options)
	extras_lines <- .document_object_lines("extras", meta$extras)

	c(
		"```{r}",
		"#| label: setup-data",
		"#| include: false",
		data_lines,
		context_lines,
		criteria_lines,
		options_lines,
		extras_lines,
		"report <- do.call(",
		"    rapsimng.decide::evaluate,",
		"    c(",
		"        list(data = data, decision='sowing', context = context, criteria = criteria, options = options),",
		"        extras",
		"    )",
		")",
		"get_section <- function(name) {",
		"    report$sections[[name]]",
		"}",
		"```"
	)
}

.document_setup_chunk_replay <- function(meta) {
	source <- .document_source(meta)
	context_lines <- .document_object_lines("context", meta$context)
	criteria_lines <- .document_object_lines("criteria", meta$criteria)
	options_lines <- .document_object_lines("options", meta$options)
	extras_lines <- .document_object_lines("extras", meta$extras)
	reader <- source$reader

	if (is.null(reader) || !nzchar(reader)) {
		reader <- "rapsimng.decide::read_output"
	}

	c(
		"```{r}",
		"#| label: setup-data",
		"#| include: false",
		context_lines,
		criteria_lines,
		options_lines,
		extras_lines,
		paste0("source <- list(file = ", .document_yaml_string(source$file), ", report = ", .document_yaml_string(source$report), ")"),
		paste0("data <- ", reader, "(source$file, source$report)"),
		"report <- do.call(",
		"    rapsimng.decide.sowing::evaluate,",
		"    c(",
		"        list(data = data, context = context, criteria = criteria, options = options),",
		"        extras",
		"    )",
		")",
		"get_section <- function(name) {",
		"    report$sections[[name]]",
		"}",
		"```"
	)
}

.document_prefix <- function(meta) {
	notes <- meta$notes
	setup_chunk <- if (.document_uses_replay(meta)) {
		.document_setup_chunk_replay(meta)
	} else {
		.document_setup_chunk_snapshot(meta)
	}

	if (length(notes) == 0) {
		notes <- "No evaluation notes were recorded."
	}

	list(
		name = "prefix",
		title = .document_title(meta),
		body = c(
			.document_yaml_lines(meta),
			setup_chunk,
			"",
			# "## Evaluation Notes",
			"",
			# paste0("- ", notes),
			""
		)
	)
}

.document_lines <- function(document) {
	c(
		document$prefix$body,
		unlist(lapply(document$sections, function(section) {
			if (is.null(section)) {
				return(NULL)
			}

			c(section$body, "")
		}), use.names = FALSE)
	)
}

.assemble_document <- function(report) {
	registry <- .registry_sections()
	sections <- report$sections

	if (is.null(sections)) {
		sections <- report[setdiff(names(report), "meta")]
	}

	section_order <- report$meta$section_order
	if (is.null(section_order)) {
		section_order <- names(sections)
	}

	prefix <- .document_prefix(report$meta)

	documents <- lapply(section_order, function(section_name) {
		section_spec <- registry[[section_name]]
		section <- sections[[section_name]]

		if (is.null(section_spec) || is.null(section)) {
			return(NULL)
		}

		section_spec$document(section, report$meta)
	})

	document <- list(
		meta = report$meta,
		prefix = prefix,
		sections = documents,
		section_order = section_order
	)

	structure(document, class = "rapsimng_decide_document")
}
