test_that("the system prompt lists the agent's tables", {
  agent <- test_agent()

  expect_match(agent$get_system_prompt(), "# Available tables", fixed = TRUE)
  expect_match(agent$get_system_prompt(), "- sales", fixed = TRUE)
})

test_that("several sources are grouped with a dialect hint", {
  sources <- list(a = test_source(tables = "sales"), b = test_source(tables = "reps"))

  expect_snapshot(cat(sources_tables_text(sources)))
})

test_that("dataset-level dictionary prose is ambient", {
  src <- test_source(dictionary = test_dictionary_path())

  expect_snapshot(cat(dictionary_prompt_text(list(src))))
})

test_that("per-table dictionary prose stays out of the prompt", {
  src <- test_source(dictionary = test_dictionary_path())
  prompt <- dictionary_prompt_text(list(src))

  expect_no_match(prompt, "Excludes orders still in flight", fixed = TRUE)
})

test_that("several dictionaries are labeled by source name", {
  sources <- list(
    warehouse = test_source(dictionary = test_dictionary_path()),
    finance = test_source(dictionary = test_dictionary_path())
  )

  prompt <- dictionary_prompt_text(sources)

  expect_match(prompt, "## warehouse", fixed = TRUE)
  expect_match(prompt, "## finance", fixed = TRUE)
})

test_that("sources without a dictionary contribute nothing", {
  expect_equal(dictionary_prompt_text(list(test_source())), "")
})

test_that("check_system_prompt() catches a path passed as prompt content", {
  path <- withr::local_tempfile(fileext = ".md")
  writeLines("You are an analyst.", path)

  expect_snapshot(error = TRUE, check_system_prompt(path), transform = scrub_path)
  expect_snapshot(error = TRUE, check_system_prompt(c("a", "b")))
})

test_that("check_system_prompt() accepts prompt content", {
  expect_silent(check_system_prompt("You are an analyst.\nBe brief."))
})
