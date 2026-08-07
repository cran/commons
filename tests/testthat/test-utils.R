test_that("tokenize() drops punctuation and one-character terms", {
  expect_equal(tokenize("Revenue, by region!"), c("revenue", "by", "region"))
  expect_equal(tokenize("a bc"), "bc")
  expect_equal(tokenize(""), character())
})

test_that("lexical_rank() ranks by number of shared terms", {
  docs <- c("revenue by region", "headcount by region", "unrelated text")

  expect_equal(lexical_rank("revenue region", docs), c(1L, 2L))
  expect_equal(lexical_rank("headcount", docs), 2L)
})

test_that("lexical_rank() returns nothing when there's no overlap", {
  expect_equal(lexical_rank("revenue", "headcount"), integer(0))
  expect_equal(lexical_rank("revenue", character()), integer(0))
  expect_equal(lexical_rank("", "revenue"), integer(0))
})

test_that("lexical_rank() honors `n`", {
  docs <- c("revenue one", "revenue two", "revenue three")
  expect_length(lexical_rank("revenue", docs, n = 2), 2)
})

test_that("df_to_markdown() renders a pipe table and caps rows", {
  out <- df_to_markdown(data.frame(x = 1:2, y = c("a", "b")))
  expect_match(out, "\\|\\s*x\\|y\\s*\\|")

  capped <- df_to_markdown(data.frame(x = 1:60))
  expect_match(capped, "*10 more rows not shown.*", fixed = TRUE)
  expect_no_match(capped, "51", fixed = TRUE)
})

test_that("flatten_inline() collapses wrapped prose", {
  expect_equal(flatten_inline("one\n  two\nthree"), "one two three")
})

test_that("word_pattern() matches whole words and escapes metacharacters", {
  expect_true(grepl(word_pattern("sales"), "from sales where"))
  expect_false(grepl(word_pattern("sales"), "wholesales"))
  expect_true(grepl(word_pattern("a.b"), "select a.b"))
  expect_false(grepl(word_pattern("a.b"), "select axb"))
})

test_that("maybe_icon() returns nothing when bsicons isn't installed", {
  local_mocked_bindings(is_installed = function(...) FALSE, .package = "rlang")

  expect_null(maybe_icon("search"))
})

test_that("html_escape() escapes the characters that break attributes", {
  expect_equal(html_escape("<a href='x'>&"), "&lt;a href=&#39;x&#39;&gt;&amp;")
})
