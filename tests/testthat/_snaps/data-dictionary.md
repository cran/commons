# data_dictionary() errors informatively on a bad path

    Code
      data_dictionary("no-such-file.yaml")
    Condition
      Error:
      ! `dictionary` does not exist: 'no-such-file.yaml'.

# as_data_dictionary() passes through and rejects other input

    Code
      as_data_dictionary(list(name = "x"))
    Condition
      Error:
      ! `dictionary` must be a path to a data-dict.yaml file.

# key_by_name() requires a name on each entry

    Code
      key_by_name(list(list(description = "A")), "table")
    Condition
      Error:
      ! Each table in a data dictionary needs a name.

# dictionary_entry_text() renders prose, columns, and joins

    Code
      cat(dictionary_entry_text(d, "sales"))
    Output
      Dictionary entry for `sales`:
      
      Closed orders, one row each.
      
      Excludes orders still in flight.
      
      Documented columns:
      
      - revenue (number, USD): Net revenue for the order.
      - region: Sales region. Values: EMEA, Americas, APAC.
      
      Relationships:
      
      - sales.rep = reps.rep (many-to-one): Each order is credited to one rep.

