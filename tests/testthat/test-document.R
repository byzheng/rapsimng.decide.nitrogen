test_that("document builds and renders from example APSIM output", {
    testthat::skip_if_not_installed("here")
    testthat::skip_if_not_installed("quarto")
    testthat::skip_if_not_installed("rapsimng.decide")

    quarto_bin <- tryCatch(quarto::quarto_path(), error = function(...) "")
    testthat::skip_if(!nzchar(quarto_bin), "Quarto CLI is not available")

    data <- rapsimng.decide::read_output(
        system.file("example/sowing.apsimx", package = "rapsimng.decide.sowing"),
        "HarvestReport"
    ) |>
        dplyr::filter(Year > 1995)

    context <- list(
        meta = list(
            title = "Sowing Window Suitability Report",
            author = "Author Names",
            date = Sys.Date(),
            notes = c("This is a test report.")
        ),
        vars = list(
            cultivar_col = "Cultivar",
            year_col = "Year",
            sowing_col = "SowingDate",
            flower_col = "Wheat.Phenology.FloweringDAS",
            yield_col = "Wheat.FrostHeatDamageFunctions.FrostHeatYield",
            frost_reduction_col = "Wheat.FrostHeatDamageFunctions.CumulativeFrostReductionRatio",
            heat_reduction_col = "Wheat.FrostHeatDamageFunctions.CumulativeHeatReductionRatio",
            frost_event_col = "Wheat.FrostHeatDamageFunctions.FrostEventNumber",
            heat_event_col = "Wheat.FrostHeatDamageFunctions.HeatEventNumber"
        )
    )

    criteria <- list(failure = list(yield_threshold = 1.0))
    options <- list()

    report <- evaluate(data, context, criteria, options)
    doc <- document(report)
    lines <- rapsimng.decide.sowing:::.document_lines(doc)

    testthat::expect_true(any(grepl('title: "Sowing Window Suitability Report"', lines, fixed = TRUE)))
    testthat::expect_false(any(grepl('rapsimng.decide::read_output(source$file, source$report)', lines, fixed = TRUE)))
    testthat::expect_true(any(grepl('rapsimng.decide::evaluate,', lines, fixed = TRUE)))
    testthat::expect_true(any(
        grepl('Sowing Window', lines, fixed = TRUE) &
            grepl('Average Yield', lines, fixed = TRUE)
    ))
    
    testthat::expect_false(any(grepl('knitr::kable(yield_summary_table)', lines, fixed = TRUE)))

    render_dir <- file.path(tempdir(), "sowing-report-test")
    dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(render_dir, recursive = TRUE, force = TRUE), add = TRUE)

    qmd_path <- file.path(render_dir, "sowing-report.qmd")
    html_path <- file.path(render_dir, "sowing-report.html")
    writeLines(lines, qmd_path, useBytes = TRUE)

    quarto::quarto_render(
        input = qmd_path,
        output_file = basename(html_path),
        quiet = TRUE
    )

    testthat::expect_true(file.exists(qmd_path))
    testthat::expect_true(file.exists(html_path))
})
