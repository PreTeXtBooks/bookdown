pretext_source_dir <- function() {
  candidates <- c(
    test_path("..", "..", "pretext", "source"),
    test_path("..", "..", "..", "pretext", "source"),
    test_path("..", "..", "..", "bookdown", "pretext", "source")
  )
  exists <- dir.exists(candidates)
  expect_true(
    any(exists),
    info = paste(
      "No PreTeXt source directory found. Checked:",
      paste(candidates, collapse = ", ")
    )
  )
  candidates[exists][1]
}

test_that("PreTeXt skeleton mirrors the Rmd book layout", {
  source_dir <- pretext_source_dir()

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
  source_dir <- pretext_source_dir()

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

test_that("PreTeXt components chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  components <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_components.ptx")),
    collapse = "\n"
  )

  expect_match(components, "<section xml:id=\"markdown-syntax\">", fixed = TRUE)
  expect_match(components, "<subsection xml:id=\"equations\">", fixed = TRUE)
  expect_match(components, "<subsection xml:id=\"theorems\">", fixed = TRUE)
  expect_match(components, "<section xml:id=\"figures\">", fixed = TRUE)
  expect_match(components, "<section xml:id=\"tables\">", fixed = TRUE)
  expect_match(components, "<section xml:id=\"citations\">", fixed = TRUE)
  expect_match(components, "images/knit-logo.png", fixed = TRUE)
  expect_match(components, "DT::datatable(iris)", fixed = TRUE)
  expect_match(components, "knitr::include_app('https://yihui.shinyapps.io/miniUI/'", fixed = TRUE)
  expect_no_match(components, "This is an empty sample chapter file", fixed = TRUE)
})

test_that("PreTeXt output formats chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  formats <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_formats.ptx")),
    collapse = "\n"
  )

  expect_match(formats, "<section xml:id=\"html\">", fixed = TRUE)
  expect_match(formats, "<subsection xml:id=\"gitbook-style\">", fixed = TRUE)
  expect_match(formats, "<subsection xml:id=\"bs4-book\">", fixed = TRUE)
  expect_match(formats, "<subsection xml:id=\"bootstrap-style\">", fixed = TRUE)
  expect_match(formats, "<section xml:id=\"latex-pdf\">", fixed = TRUE)
  expect_match(formats, "<section xml:id=\"e-books\">", fixed = TRUE)
  expect_match(formats, "<section xml:id=\"a-single-document\">", fixed = TRUE)
  expect_match(formats, "images/gitbook.png", fixed = TRUE)
  expect_match(formats, "images/bs4-book.png", fixed = TRUE)
  expect_match(formats, "bookdown::bs4_book", fixed = TRUE)
  expect_match(formats, "bookdown::word_document2", fixed = TRUE)
  expect_no_match(formats, "This is an empty sample chapter file", fixed = TRUE)
})

test_that("PreTeXt customization chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  customization <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_customization.ptx")),
    collapse = "\n"
  )

  expect_match(customization, "<section xml:id=\"yaml-options\">", fixed = TRUE)
  expect_match(customization, "<section xml:id=\"theming\">", fixed = TRUE)
  expect_match(customization, "<section xml:id=\"templates\">", fixed = TRUE)
  expect_match(customization, "<section xml:id=\"configuration\">", fixed = TRUE)
  expect_match(customization, "<section xml:id=\"internationalization\">", fixed = TRUE)
  expect_match(customization, "bookdown::gitbook", fixed = TRUE)
  expect_match(customization, "bookdown::pdf_book", fixed = TRUE)
  expect_match(customization, "before_chapter_script", fixed = TRUE)
  expect_match(customization, "bookdown:::label_names", fixed = TRUE)
  expect_no_match(customization, "This is an empty sample chapter file", fixed = TRUE)
})

test_that("PreTeXt editing chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  editing <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_editing.ptx")),
    collapse = "\n"
  )

  expect_match(editing, "<section xml:id=\"build-the-book\">", fixed = TRUE)
  expect_match(editing, "<section xml:id=\"preview-a-chapter\">", fixed = TRUE)
  expect_match(editing, "<section xml:id=\"serve-the-book\">", fixed = TRUE)
  expect_match(editing, "<section xml:id=\"rstudio-ide\">", fixed = TRUE)
  expect_match(editing, "<section xml:id=\"collaboration\">", fixed = TRUE)
  expect_match(editing, "bookdown::render_book(\"index.Rmd\", \"bookdown::gitbook\")", fixed = TRUE)
  expect_match(editing, "bookdown::serve_book(daemon = TRUE)", fixed = TRUE)
  expect_match(editing, "bookdown::bookdown_site", fixed = TRUE)
  expect_match(editing, "images/mathquill.png", fixed = TRUE)
  expect_match(editing, "images/disqus.png", fixed = TRUE)
  expect_no_match(editing, "This is an empty sample chapter file", fixed = TRUE)
})

test_that("PreTeXt publishing chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  publishing <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_publishing.ptx")),
    collapse = "\n"
  )

  expect_match(publishing, "<section xml:id=\"rstudio-connect\">", fixed = TRUE)
  expect_match(publishing, "<section xml:id=\"netlify-drop\">", fixed = TRUE)
  expect_match(publishing, "<section xml:id=\"github\">", fixed = TRUE)
  expect_match(publishing, "<section xml:id=\"features-for-html-publishing\">", fixed = TRUE)
  expect_match(publishing, "<section xml:id=\"publishers\">", fixed = TRUE)
  expect_match(publishing, "bookdown::publish_book(render = 'local')", fixed = TRUE)
  expect_match(publishing, "output_dir: \"docs\"", fixed = TRUE)
  expect_match(publishing, "bookdown::pdf_book:", fixed = TRUE)
  expect_match(publishing, "images/netlify-drag-drop-update.png", fixed = TRUE)
  expect_match(publishing, "images/404.png", fixed = TRUE)
  expect_match(publishing, "images/social-twitter.png", fixed = TRUE)
  expect_no_match(publishing, "This is an empty sample chapter file", fixed = TRUE)
})

test_that("PreTeXt tools chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  tools <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_tools.ptx")),
    collapse = "\n"
  )

  expect_match(tools, "<section xml:id=\"r-and-r-packages\">", fixed = TRUE)
  expect_match(tools, "<section xml:id=\"pandoc\">", fixed = TRUE)
  expect_match(tools, "<section xml:id=\"latex\">", fixed = TRUE)
  expect_match(tools, "install.packages(\"bookdown\")", fixed = TRUE)
  expect_match(tools, "devtools::install_github('rstudio/bookdown')", fixed = TRUE)
  expect_match(tools, "rmarkdown::pandoc_version()", fixed = TRUE)
  expect_match(tools, "tinytex::install_tinytex()", fixed = TRUE)
  expect_match(tools, "tinytex::tlmgr_update()", fixed = TRUE)
  expect_match(tools, "tinytex::reinstall_tinytex()", fixed = TRUE)
  expect_no_match(tools, "This is an empty sample chapter file", fixed = TRUE)
})
