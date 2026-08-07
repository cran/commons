# commons() warns when the client already carries a prompt or tools

    Code
      agent <- commons(client, data_sources = test_source())
    Condition
      Warning in `commons()`:
      The system prompt set on `client` is ignored; commons builds its own.
      i Pass it to the `system_prompt` argument instead.
      Warning in `commons()`:
      The tools registered on `client` are ignored; commons registers its own.

# commons() validates its arguments

    Code
      commons("not a chat", src)
    Condition
      Error in `commons()`:
      ! `client` must be an <ellmer::Chat>, e.g. from `ellmer::chat_anthropic()`.

---

    Code
      commons(test_client(), "not a source")
    Condition
      Error in `commons()`:
      ! `data_sources` must be a `data_source()` or a named list of them.

---

    Code
      commons(test_client(), src, semantic_layer = list(test_measure()))
    Condition
      Error in `commons()`:
      ! `semantic_layer` must be a `semantic_layer()`.

# commons() requires arguments after `data_sources` to be named

    Code
      commons(test_client(), test_source(), semantic_layer(test_measure()))
    Condition
      Error in `commons()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = semantic_layer(test_measure())
      i Did you forget to name an argument?

# commons() surfaces a measure whose hidden argument names no source

    Code
      commons(test_client(), test_source(), semantic_layer = layer)
    Condition
      Error in `commons()`:
      ! Measure "revenue" has undocumented argument `warehouse` matching no data source.
      i `data_sources` has no named sources.

