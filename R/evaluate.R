#' Evaluate sowing window suitability from APSIM NG outputs
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
evaluate <- function(
	data,
	context = list(),
	criteria = list(),
	options = list(),
	...
) {

	# context <- .standardise_context(context)
	# criteria <- .standardise_criteria(criteria)
	# options <- .standardise_options(options)

	.validate_inputs(
		data = data,
		context = context,
		criteria = criteria,
		options = options
	)

	state <- .initialise_state(
		data = data,
		context = context,
		criteria = criteria,
		options = options,
		extras = list(...)
	)
	.assemble_report(state)
}
