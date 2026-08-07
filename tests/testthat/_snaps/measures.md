# measure() validates its scalar arguments

    Code
      measure(1, "d", fn)
    Condition
      Error in `measure()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      measure("m", 1, fn)
    Condition
      Error in `measure()`:
      ! `description` must be a single string, not the number 1.

---

    Code
      measure("m", "d", fn, title = 1)
    Condition
      Error in `measure()`:
      ! `title` must be a single string or `NULL`, not the number 1.

# semantic_layer() rejects duplicates and non-measures

    Code
      semantic_layer(test_measure(), test_measure())
    Condition
      Error in `semantic_layer()`:
      ! Measure names must be unique; duplicated name: "order_count".

---

    Code
      semantic_layer(function() 1)
    Condition
      Error in `semantic_layer()`:
      ! Every item in `semantic_layer` must be an <ellmer::ToolDef>.

# resolve_injections() errors on an unmatched argument with no default

    Code
      resolve_injections(registry, list())
    Condition
      Error:
      ! Measure "revenue" has undocumented argument `warehouse` matching no data source.
      i `data_sources` has no named sources.

---

    Code
      resolve_injections(registry, list(finance = 1))
    Condition
      Error:
      ! Measure "revenue" has undocumented argument `warehouse` matching no data source.
      i Available sources: "finance".

# validate_measure_args() enforces enums and arrays

    Code
      validate_measure_args(m, list(region = "LATAM", regions = "EMEA"))
    Condition
      Error:
      ! Invalid value for `region` of measure "m": "LATAM".
      i Allowed: "EMEA" and "APAC".

# validate_measure_args() reports missing and unknown arguments

    Code
      validate_measure_args(m, list())
    Condition
      Error:
      ! Measure "m" requires argument `region`.

---

    Code
      validate_measure_args(m, list(region = "EMEA", rep = "Ada"))
    Condition
      Error:
      ! Unknown argument for measure "m": "rep".
      i Valid arguments: "region".

# search_measures_text() renders matching schemas

    Code
      cat(search_measures_text(registry, "revenue by region"))
    Output
      ### revenue_by_region
      Total revenue for a sales region.
      
      arguments:
        - region (string, required) Sales region.

# search_measures_text() handles empty registries and misses

    Code
      cat(search_measures_text(semantic_layer(test_measure())$measures, "headcount"))
    Output
      No measure matches "headcount". Consider writing a SQL query.

