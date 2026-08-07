# data_source() errors on tables the connection doesn't have

    Code
      data_source(con, tables = c("sales", "nope"))
    Condition
      Error in `data_source()`:
      ! `tables` names table not on the connection: "nope".

# data_source() rejects anything but a single connection

    Code
      data_source()
    Condition
      Error in `data_source()`:
      ! `data_source()` needs a database connection.

---

    Code
      data_source(sales = test_sales())
    Condition
      Error in `data_source()`:
      ! `data_source()` needs a <DBIConnection>, not a data frame.

---

    Code
      data_source(con, con)
    Condition
      Error in `data_source()`:
      ! `data_source()` takes a single database connection.

# data_source() rejects a dictionary that isn't a path

    Code
      data_source(con, dictionary = list())
    Condition
      Error in `data_source()`:
      ! `dictionary` must be a path to a data-dict.yaml file.

# list_tables() requires a data source

    Code
      list_tables("sales")
    Condition
      Error in `list_tables()`:
      ! `data_source` must be a `data_source()`.

# normalize_table_registry() rejects malformed entries

    Code
      normalize_table_registry(c("sales", "sales"))
    Condition
      Error:
      ! `tables` must not contain duplicate labels: "sales".

---

    Code
      normalize_table_registry(".sales")
    Condition
      Error:
      ! Schema-qualified entries in `tables` must not contain empty name components.

---

    Code
      normalize_table_registry(1)
    Condition
      Error:
      ! `tables` must be a character vector, a list, or a <DBI::Id>.

---

    Code
      normalize_table_registry(list(NA_character_))
    Condition
      Error:
      ! Each entry in `tables` must be a table name or a <DBI::Id>.

---

    Code
      normalize_table_registry(DBI::Id(schema = "public"))
    Condition
      Error:
      ! <DBI::Id> entries in `tables` must include a `table` component.

# source_describe() errors on an unregistered table

    Code
      source_describe(src, "reps")
    Condition
      Error:
      ! No table named "reps".
      i Available tables: "sales".

# check_query() rejects statements that would write

    Code
      check_query("DROP TABLE sales")
    Condition
      Error:
      ! The query contains a disallowed operation: `DROP`.
      i Only read-only SELECT queries are allowed.

---

    Code
      check_query("  insert into sales values (1)")
    Condition
      Error:
      ! The query contains a disallowed operation: `INSERT`.
      i Only read-only SELECT queries are allowed.

---

    Code
      check_query("update\n sales set revenue = 0")
    Condition
      Error:
      ! The query contains a disallowed operation: `UPDATE`.
      i Only read-only SELECT queries are allowed.

# resolve_sql_source() requires a valid name with several sources

    Code
      resolve_sql_source(sources, NULL)
    Condition
      Error:
      ! `source` is required when an agent has multiple data sources.
      i Available sources: "a" and "b".

---

    Code
      resolve_sql_source(sources, "c")
    Condition
      Error:
      ! No data source named "c".
      i Available sources: "a" and "b".

# as_data_sources() wraps a bare source and validates lists

    Code
      as_data_sources("sales")
    Condition
      Error:
      ! `data_sources` must be a `data_source()` or a named list of them.

---

    Code
      as_data_sources(list())
    Condition
      Error:
      ! `data_sources` must be a `data_source()` or a named list of them.

---

    Code
      as_data_sources(list(src, src))
    Condition
      Error:
      ! Each entry in `data_sources` must be named.

---

    Code
      as_data_sources(list(a = src, a = src))
    Condition
      Error:
      ! `data_sources` names must be unique; duplicated name: "a".

