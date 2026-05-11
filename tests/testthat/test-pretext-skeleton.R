test_that("PreTeXt skeleton mirrors the Rmd book layout", {
  candidates <- c(
    test_path("..", "..", "pretext", "source"),
    test_path("..", "..", "..", "pretext", "source"),
    test_path("..", "..", "..", "bookdown", "pretext", "source")
  )
  source_dir <- candidates[dir.exists(candidates)][1]
  expect_false(is.na(source_dir))
  expect_length(source_dir, 1)

  expected_files <- c(
    "meta_docinfo.ptx",
    "meta_frontmatter.ptx",
    "meta_backmatter.ptx",
    "ch_about_author.ptx",
    "ch_intro.ptx",
    "ch_components.ptx",
    "ch_formats.ptx",
    "ch_customization.ptx",
    "ch_editing.ptx",
    "ch_publishing.ptx",
    "ch_tools.ptx",
    "ch_usage.ptx",
    "ch_faq.ptx"
  )
  chapter_files <- grep("^ch_.*[.]ptx$", expected_files, value = TRUE)

  expect_true(all(file.exists(file.path(source_dir, expected_files))))

  main_ptx <- paste(xfun::read_utf8(file.path(source_dir, "main.ptx")), collapse = "\n")
  for (file in c("meta_frontmatter.ptx", chapter_files, "meta_backmatter.ptx")) {
    expect_match(main_ptx, sprintf('href="./%s"', file), fixed = TRUE)
  }

  frontmatter <- paste(
    xfun::read_utf8(file.path(source_dir, "meta_frontmatter.ptx")),
    collapse = "\n"
  )
  expect_match(frontmatter, "<titlepage>", fixed = TRUE)
  expect_match(frontmatter, "<abstract>", fixed = TRUE)
  expect_match(frontmatter, "<preface", fixed = TRUE)

  backmatter <- paste(
    xfun::read_utf8(file.path(source_dir, "meta_backmatter.ptx")),
    collapse = "\n"
  )
  expect_match(backmatter, "<references>", fixed = TRUE)
  expect_match(backmatter, "<index>", fixed = TRUE)
})

test_that("PreTeXt introduction chapter mirrors the Rmd structure", {
  candidates <- c(
    test_path("..", "..", "pretext", "source"),
    test_path("..", "..", "..", "pretext", "source"),
    test_path("..", "..", "..", "bookdown", "pretext", "source")
  )
  source_dir <- candidates[dir.exists(candidates)][1]
  expect_false(is.na(source_dir))
  expect_length(source_dir, 1)

  intro <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_intro.ptx")),
    collapse = "\n"
  )

  expect_match(intro, "<section xml:id=\"motivation\">", fixed = TRUE)
  expect_match(intro, "<section xml:id=\"get-started\">", fixed = TRUE)
  expect_match(intro, "<section xml:id=\"usage\">", fixed = TRUE)
  expect_match(intro, "<section xml:id=\"new-session\">", fixed = TRUE)
  expect_match(intro, "<section xml:id=\"some-tips\">", fixed = TRUE)
  expect_match(intro, "<aside>", fixed = TRUE)
  expect_match(intro, "<program language=\"r\">", fixed = TRUE)
  expect_match(intro, "bookdown::render_book('foo.Rmd', 'bookdown::gitbook')", fixed = TRUE)
  expect_no_match(intro, "Replace this placeholder text", fixed = TRUE)
})
