
.registry_sections <- function() {
    list(
		summary = .section_summary_spec(),
        yield = .section_yield_spec(),
        flower = .section_flower_spec(),
        stress = .section_stress_spec()
    )
}

.resolve_report_source <- function(state) {
    source <- state$extras$source

    if (!is.null(source)) {
        return(source)
    }

    context_meta <- state$context$meta
    if (is.null(context_meta$source) || is.null(context_meta$report)) {
        return(NULL)
    }

    list(
        file = context_meta$source,
        report = context_meta$report
    )
}

.build_report_meta <- function(state, registry) {
    list(
        data = state$data,
        source = .resolve_report_source(state),
        context = state$context,
        criteria = state$criteria,
        options = state$options,
        extras = state$extras,
        notes = state$notes,
        section_order = names(registry)
    )
}

.assemble_report <- function(state) {
    registry <- .registry_sections()
    sections <- lapply(
        registry,
        function(section_spec) section_spec$evaluate(state, section_spec)
    )

    report <- list(
        meta = .build_report_meta(state, registry),
        sections = sections
    )
    report[names(sections)] <- sections

    class(report) <- unique(c("rapsimng_decide_report", class(report)))
    report
}