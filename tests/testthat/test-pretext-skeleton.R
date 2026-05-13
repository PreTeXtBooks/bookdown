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
  chapter_ids <- c(
    ch_about_author = "ch-about-author",
    ch_intro = "ch-intro",
    ch_components = "ch-components",
    ch_formats = "ch-formats",
    ch_customization = "ch-customization",
    ch_editing = "ch-editing",
    ch_publishing = "ch-publishing",
    ch_tools = "ch-tools",
    ch_usage = "ch-usage",
    ch_faq = "ch-faq"
  )

  expect_true(all(file.exists(file.path(source_dir, expected_files))))

  for (stem in names(chapter_ids)) {
    chapter <- paste(
      xfun::read_utf8(file.path(source_dir, paste0(stem, ".ptx"))),
      collapse = "\n"
    )
    expect_match(
      chapter,
      sprintf('<chapter xml:id="%s"', chapter_ids[[stem]]),
      fixed = TRUE
    )
  }

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
  expect_match(frontmatter, "<title>Acknowledgments</title>", fixed = TRUE)
  expect_no_match(frontmatter, "Replace this abstract", fixed = TRUE)
  expect_no_match(frontmatter, "Use the front matter for prefaces", fixed = TRUE)
  expect_no_match(frontmatter, "Add acknowledgments, contributor notes", fixed = TRUE)

  backmatter <- paste(
    xfun::read_utf8(file.path(source_dir, "meta_backmatter.ptx")),
    collapse = "\n"
  )
  expect_match(backmatter, "<references>", fixed = TRUE)
  expect_match(backmatter, "<title>References</title>", fixed = TRUE)
  expect_match(backmatter, "Dynamic Documents with R and knitr", fixed = TRUE)
  expect_match(backmatter, "Literate Programming", fixed = TRUE)
  expect_match(backmatter, "<c>book.bib</c>", fixed = TRUE)
  expect_match(backmatter, "<c>packages.bib</c>", fixed = TRUE)
  expect_no_match(backmatter, "Add bibliography entries or generated references here.", fixed = TRUE)
  expect_match(backmatter, "<index>", fixed = TRUE)
})

test_that("PreTeXt about-author chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  about_author <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_about_author.ptx")),
    collapse = "\n"
  )

  expect_match(about_author, "<title>About the Author</title>", fixed = TRUE)
  expect_match(about_author, "Yihui Xie", fixed = TRUE)
  expect_match(about_author, "href=\"http://yihui.org\" visual=\"yihui.org\"", fixed = TRUE)
  expect_match(about_author, "<em>bookdown</em>", fixed = TRUE)
  expect_match(about_author, "Capital of Statistics", fixed = TRUE)
  expect_match(about_author, "https://github.com/yihui", fixed = TRUE)
  expect_match(about_author, "classical Chinese literature", fixed = TRUE)
  expect_no_match(about_author, "Use this sample file for author biographies", fixed = TRUE)
})

test_that("PreTeXt frontmatter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  frontmatter <- paste(
    xfun::read_utf8(file.path(source_dir, "meta_frontmatter.ptx")),
    collapse = "\n"
  )

  expect_match(frontmatter, "<personname>Yihui Xie</personname>", fixed = TRUE)
  expect_match(frontmatter, "bookdown.org/yihui/bookdown", fixed = TRUE)
  expect_match(frontmatter, "../assets/images/cover.jpg", fixed = TRUE)
  expect_match(frontmatter, "../assets/images/logo.png", fixed = TRUE)
  expect_match(frontmatter, "../assets/images/by-nc-sa.png", fixed = TRUE)
  for (text in c(
    "<section xml:id=\"why-read-this-book\">",
    "<section xml:id=\"structure-of-the-book\">",
    "<section xml:id=\"software-information-and-conventions\">",
    "<xref ref=\"markdown-syntax\"/>",
    "<xref ref=\"a-single-document\"/>",
    "<xref ref=\"ch-intro\" text=\"title\"/>",
    "<xref ref=\"rstudio-ide\" text=\"title\"/>",
    "<xref ref=\"ch-usage\" text=\"title\"/>",
    "<xref ref=\"ch-tools\" text=\"title\"/>"
  )) {
    expect_match(frontmatter, text, fixed = TRUE)
  }
  expect_match(frontmatter, "sessionInfo()", fixed = TRUE)
  expect_match(frontmatter, "github.com/rstudio/bookdown/graphs/contributors", fixed = TRUE)
  expect_match(frontmatter, "Lastly I want to thank my family", fixed = TRUE)
  expect_match(frontmatter, "Elkhorn, Nebraska", fixed = TRUE)
  expect_no_match(frontmatter, "Replace this abstract", fixed = TRUE)
})

test_that("PreTeXt introduction chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  intro <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_intro.ptx")),
    collapse = "\n"
  )

  for (text in c(
    "<section xml:id=\"motivation\">",
    "<section xml:id=\"get-started\">",
    "<section xml:id=\"usage\">",
    "<section xml:id=\"new-session\">",
    "<section xml:id=\"some-tips\">",
    "<xref ref=\"ch-tools\" text=\"title\"/>",
    "<xref ref=\"ch-usage\" text=\"title\"/>",
    "<xref ref=\"configuration\"/>"
  )) {
    expect_match(intro, text, fixed = TRUE)
  }
  expect_match(intro, "<aside>", fixed = TRUE)
  expect_match(intro, "<program language=\"r\">", fixed = TRUE)
  expect_match(intro, "bookdown::render_book('foo.Rmd', 'bookdown::gitbook')", fixed = TRUE)
  expect_match(intro, "Dynamic Documents with R and knitr", fixed = TRUE)
  expect_match(intro, "daringfireball.net/projects/markdown", fixed = TRUE)
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
  expect_match(customization, "<xref ref=\"collaboration\"/>", fixed = TRUE)
  expect_match(customization, "<xref ref=\"bootstrap-style\"/>", fixed = TRUE)
  expect_match(customization, "https://disqus.com", fixed = TRUE)
  expect_match(customization, "https://hypothes.is", fixed = TRUE)
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
  expect_match(editing, "<xref ref=\"configuration\"/>", fixed = TRUE)
  expect_match(editing, "<xref ref=\"new-session\"/>", fixed = TRUE)
  expect_match(editing, "<xref ref=\"usage\"/>", fixed = TRUE)
  expect_match(editing, "<xref ref=\"gitbook-style\"/>", fixed = TRUE)
  expect_match(editing, "<xref ref=\"yaml-options\"/>", fixed = TRUE)
  expect_match(editing, "rstudio.github.io/rstudioaddins", fixed = TRUE)
  expect_match(editing, "help.github.com/articles/about-pull-requests", fixed = TRUE)
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
  expect_match(publishing, "<xref ref=\"build-the-book\"/>", fixed = TRUE)
  expect_match(publishing, "posit.co/products/enterprise/connect", fixed = TRUE)
  expect_match(publishing, "bookdown.org/connect", fixed = TRUE)
  expect_match(publishing, "pages.github.com", fixed = TRUE)
  expect_match(publishing, "http://jekyllrb.com", fixed = TRUE)
  expect_match(publishing, "https://travis-ci.com", fixed = TRUE)
  expect_match(publishing, "http://bit.ly/2cvloKV", fixed = TRUE)
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
  expect_match(tools, "https://www.posit.co", fixed = TRUE)
  expect_match(tools, "rmarkdown::pandoc_version()", fixed = TRUE)
  expect_match(tools, "http://pandoc.org", fixed = TRUE)
  expect_match(tools, "tinytex::install_tinytex()", fixed = TRUE)
  expect_match(tools, "https://www.latex-project.org/get/", fixed = TRUE)
  expect_match(tools, "<c>titling</c> package", fixed = TRUE)
  expect_match(tools, "tinytex::tlmgr_update()", fixed = TRUE)
  expect_match(tools, "tinytex::reinstall_tinytex()", fixed = TRUE)
  expect_no_match(tools, "This is an empty sample chapter file", fixed = TRUE)
})

test_that("PreTeXt usage chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  usage <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_usage.ptx")),
    collapse = "\n"
  )

  expect_match(usage, "<section xml:id=\"knitr\">", fixed = TRUE)
  expect_match(usage, "<section xml:id=\"r-markdown\">", fixed = TRUE)
  expect_match(usage, "<xref ref=\"ch-intro\"/>", fixed = TRUE)
  expect_match(usage, "<xref ref=\"r-code\"/>", fixed = TRUE)
  expect_match(usage, "<xref ref=\"ch-components\"/>", fixed = TRUE)
  expect_match(usage, "https://stackoverflow.com", fixed = TRUE)
  expect_match(usage, "Literate Programming", fixed = TRUE)
  expect_match(usage, "authoring_knitr_engines.html", fixed = TRUE)
  expect_match(usage, "https://jupyter.org", fixed = TRUE)
  expect_match(usage, "blog.rstudio.org/2016/10/05/r-notebooks", fixed = TRUE)
  expect_match(usage, "knitr::knit()", fixed = TRUE)
  expect_match(usage, "# a literal code chunk", fixed = TRUE)
  expect_match(usage, "output: [\"html_document\", \"word_document\"]", fixed = TRUE)
  expect_match(usage, "bookdown::gitbook", fixed = TRUE)
  expect_match(usage, "?rmarkdown::html_document", fixed = TRUE)
  expect_match(usage, "knitr::combine_words(", fixed = TRUE)
  expect_no_match(usage, "This is an empty sample chapter file", fixed = TRUE)
})

test_that("PreTeXt FAQ chapter mirrors the Rmd structure", {
  source_dir <- pretext_source_dir()

  faq <- paste(
    xfun::read_utf8(file.path(source_dir, "ch_faq.ptx")),
    collapse = "\n"
  )

  expect_match(faq, "<title>FAQ</title>", fixed = TRUE)
  expect_match(faq, "Will <c>bookdown</c> have the features X, Y, and Z?", fixed = TRUE)
  expect_match(faq, "https://github.com/rstudio/bookdown/issues", fixed = TRUE)
  expect_match(faq, "Pandoc's Markdown supports raw LaTeX code", fixed = TRUE)
  expect_match(faq, "Markdown should be kept as simple as possible", fixed = TRUE)
  expect_match(faq, "control your own wild heart", fixed = TRUE)
  expect_no_match(faq, "This is an empty sample chapter file", fixed = TRUE)
})
