# several sources are grouped with a dialect hint

    Code
      cat(sources_tables_text(sources))
    Output
      Tables are grouped by data source. Pass the source's name as `source` to `describe_table` and `run_sql`.
      
      ## a (duckdb)
      
      - sales
      
      ## b (duckdb)
      
      - reps

# dataset-level dictionary prose is ambient

    Code
      cat(dictionary_prompt_text(list(src)))
    Output
      
      # About the data
      
      ## Sales
      
      One row per closed order.
      
      Revenue is always net of refunds.
      
      Definitions of domain terms:
      
      - net revenue: Revenue after refunds and credits.
      - book of business: The set of accounts a rep owns.

# check_system_prompt() catches a path passed as prompt content

    Code
      check_system_prompt(path)
    Condition
      Error:
      ! `system_prompt` must be prompt content, not a file path.
      i Did you mean `ellmer::interpolate_file("<tempfile>")`?

---

    Code
      check_system_prompt(c("a", "b"))
    Condition
      Error:
      ! `system_prompt` must be a single string, not a character vector.

