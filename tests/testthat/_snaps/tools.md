# call_measure errors on an unknown measure

    Code
      call_measure_tool(registry, "nope", "{}")
    Condition
      Error in `call_measure_tool()`:
      ! No measure named "nope".
      i Registered measures: "order_count".

---

    Code
      call_measure_tool(list(), "nope", "{}")
    Condition
      Error in `call_measure_tool()`:
      ! No measure named "nope".
      i No measures are registered.

# search_context reports misses and missing documentation

    Code
      cat(search_context_tool(state$context, "headcount")@value)
    Output
      No context found for "headcount".

---

    Code
      cat(search_context_tool(character(), "net revenue"))
    Output
      No data documentation is available for this agent.

# describe_table merges the dictionary with the live schema

    Code
      cat(describe_table_tool(src, "reps")@value)
    Output
      One row per sales representative.
      
      Columns of `reps`:
      
      - rep (character)
      - region (character)
      
      Relationships:
      
      - sales.rep = reps.rep (many-to-one): Each order is credited to one rep.
      
      Sample rows:
      
      |rep |region   |
      |:---|:--------|
      |Ada |EMEA     |
      |Bo  |Americas |
      |Cy  |APAC     |

# run_sql refuses to write

    Code
      run_sql_tool(test_source(), "DELETE FROM sales")
    Condition
      Error in `run_sql_tool()`:
      ! The query contains a disallowed operation: `DELETE`.
      i Only read-only SELECT queries are allowed.

