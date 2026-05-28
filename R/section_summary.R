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
            "Narrative (Executive Summary):",
            "",
            "Goal:",
            "- Summarise key decision-relevant findings across all sections using structured outputs.",
            "",
            "Context:",
            "- Integrate results from yield, flowering, frost, and heat analyses.",
            "- Focus on aggregated (multi-year) simulation outputs.",
            "- Nitrogen performance is interpreted across yield outcome groups (Low, Mid, High).",
            "",
            "Focus:",
            "- Differences in yield performance across nitrogen levels",
            "- Variability and downside risk",
            "- Behaviour under different seasonal conditions (Low, Mid, High groups)",
            "- Stress-related patterns (frost and heat) only if explicitly shown in outputs",
            "- Relative ordering of nitrogen levels where clearly supported by tables",
            "",
            "Evidence rule:",
            "- Use ONLY values, rankings, or patterns explicitly presented in tables and figures",
            "- Do NOT derive new metrics, thresholds, or rules",
            "- Do NOT infer beyond the presented data",
            "",
            "Interpretation guidance:",
            "- Emphasise trade-offs between yield, stability, and risk",
            "- Highlight consistent patterns across yield groups (e.g. stable vs variable performance)",
            "- Prefer robust patterns over isolated extremes",
            "",
            "Avoid:",
            "- Using narrative text from other sections",
            "- Introducing new assumptions or agronomic rules",
            "- Making recommendations or selecting a “best” nitrogen level",
            "- Over-emphasising rare or extreme observations",
            "",
            "Output:",
            "- 3–5 sentences",
            "- Concise summary of key patterns and trade-offs",
            "- Structured and logically ordered (from general to specific)",
            "",
            "Style:",
            "- Clear and neutral",
            "- Evidence-based and non-prescriptive",
            "- Farming decision-oriented (risk vs reward framing)",
            "-->"

		)
	)
}