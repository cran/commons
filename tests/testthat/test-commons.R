test_that("commons() returns a Chat carrying its own prompt and tools", {
  agent <- test_agent()

  expect_s3_class(agent, "Chat")
  expect_length(agent$get_tools(), 5)
  expect_length(agent$get_turns(), 0)
  expect_match(agent$get_system_prompt(), "self-service data analyst")
})

test_that("commons() leaves the client it was given alone", {
  client <- test_client()
  agent <- commons(client, data_sources = test_source())

  expect_null(client$get_system_prompt())
  expect_length(client$get_tools(), 0)
  expect_false(identical(client, agent))
})

test_that("commons() accepts a bare data source or a named list", {
  src <- test_source()

  expect_length(commons(test_client(), src)$get_tools(), 5)
  expect_length(commons(test_client(), list(a = src, b = src))$get_tools(), 5)
})

test_that("commons() takes a custom system prompt and appends the tables", {
  agent <- test_agent(system_prompt = "You are a terse analyst.\n")

  expect_match(agent$get_system_prompt(), "^You are a terse analyst\\.")
  expect_match(agent$get_system_prompt(), "# Available tables", fixed = TRUE)
})

test_that("commons() warns when the client already carries a prompt or tools", {
  client <- test_client()
  client$set_system_prompt("You are a poet.")
  client$register_tool(ellmer::tool(function() 1, "One.", name = "one"))

  expect_snapshot(agent <- commons(client, data_sources = test_source()))
  expect_no_match(agent$get_system_prompt(), "poet")
  expect_length(agent$get_tools(), 5)
})

test_that("commons() starts from a clean conversation", {
  client <- test_client()
  client$set_turns(list(ellmer::Turn("user", "Hi")))

  expect_length(commons(client, data_sources = test_source())$get_turns(), 0)
})

test_that("commons() validates its arguments", {
  src <- test_source()

  expect_snapshot(error = TRUE, commons("not a chat", src))
  expect_snapshot(error = TRUE, commons(test_client(), "not a source"))
  expect_snapshot(
    error = TRUE,
    commons(test_client(), src, semantic_layer = list(test_measure()))
  )
})

test_that("commons() requires arguments after `data_sources` to be named", {
  expect_snapshot(
    error = TRUE,
    commons(test_client(), test_source(), semantic_layer(test_measure()))
  )
})

test_that("commons() surfaces a measure whose hidden argument names no source", {
  layer <- semantic_layer(measure("revenue", "d", function(warehouse) NULL))

  expect_snapshot(
    error = TRUE,
    commons(test_client(), test_source(), semantic_layer = layer)
  )
})

test_that("measure_injectables() only offers named sources", {
  src <- test_source()

  expect_equal(measure_injectables(list(src)), list())
  expect_named(measure_injectables(list(warehouse = src)), "warehouse")
})

test_that("agent state collects the searchable context from every source", {
  state <- new_agent_state(
    list(
      a = test_source(dictionary = test_dictionary_path()),
      b = test_source()
    ),
    semantic_layer()
  )

  expect_length(state$context, 5)
  expect_true(any(grepl("net revenue", state$context)))
})

test_that("agent state has no context without a dictionary", {
  state <- new_agent_state(list(test_source()), semantic_layer())

  expect_equal(state$context, character())
})
