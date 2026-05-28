.section_summary_spec <- function() {
	list(
		name = "summary",
		title = "Summary",
		description = "Summary section providing an overview of the evaluation context and key findings.",
		evaluate = .evaluate_section_summary,
		document = .document_section_summary
	)
}

.evaluate_section_summary <- function(state, spec = .section_summary_spec()) {
	list(
		name = spec$name,
		title = spec$title,
		description = spec$description
	)
}

.document_section_summary <- function(section, meta = NULL) {
	list(
		name = section$name,
		title = section$title,
		body = c(
			paste0("## ", section$title),
			"",
            "<!--",
            " Narrative (Executive Summary):",
            "",
            " - Goal: Summarise key decision-relevant findings across the report.",
            "",
            " - Context: Use only structured outputs (tables and figures) from all sections.",
            "",
            " - Focus:",
            "   - differences in average yield across cultivars",
            "   - variability and downside risk",
            "   - major stress-related patterns (if explicitly shown)",
            "   - relative ordering where clearly visible in tables",
            "",
            " - Evidence rule:",
            "   - Only use values and rankings explicitly presented in the report",
            "   - Do NOT derive new metrics or thresholds",
            "",
            " - Avoid:",
            "   - using narrative text from other sections",
            "   - introducing new assumptions or agronomic rules",
            "   - making recommendations or choosing a cultivar",
            "",
            " - Output:",
            "   - 3-5 sentences",
            "   - concise, decision-relevant summary highlighting trade-offs",
            "",
            " - Style:",
            "   - clear, neutral, evidence-based language",
            "   - farming decision oriented but non-prescriptive",
            "-->"

		)
	)
}