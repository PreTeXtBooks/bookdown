appendix_source <- function(file) {
  candidates <- c(
    file.path("pretext", "source", file),
    file.path("..", "..", "pretext", "source", file),
    file.path("..", "..", "..", "pretext", "source", file),
    file.path("..", "..", "..", "bookdown", "pretext", "source", file)
  )
  matches <- candidates[file.exists(candidates)]
  testthat::expect_true(length(matches) > 0, info = paste("Missing", file))
  normalizePath(matches[[1]], mustWork = TRUE)
}

test_that("PreTeXt tools appendix preserves key Rmd installation details", {
  tools <- paste(xfun::read_utf8(appendix_source("ch_tools.ptx")), collapse = "\n")

  expect_match(tools, "Comprehensive R\\s+Archive Network")
  expect_match(tools, "there will be a few new releases of R every year", fixed = TRUE)
  expect_match(tools, "you need to install <c>devtools</c> first:", fixed = TRUE)
})

test_that("PreTeXt usage appendix preserves key Rmd explanatory details", {
  usage <- paste(xfun::read_utf8(appendix_source("ch_usage.ptx")), collapse = "\n")

  expect_match(usage, "<q>Literate Programming</q>", fixed = TRUE)
  expect_match(usage, "<c>*.Rnw</c> documents", fixed = TRUE)
  expect_match(usage, "<c>*.Rhtml</c>", fixed = TRUE)
  expect_match(usage, "R\\s+Markdown can also be used as notebooks")
  expect_match(
    usage,
    "pandoc.org/MANUAL.html#block-content-in-list-items",
    fixed = TRUE
  )
  expect_match(
    usage,
    "Besides characters, another common type of values are logical values",
    fixed = TRUE
  )
  expect_match(usage, "There are many more possible output formats", fixed = TRUE)
})
