# Process All Crowds and Generate Output

Internal helper function that iterates through all crowd identifiers and
generates the appropriate output for each crowd.

## Usage

``` r
process_all_crowds(
  args,
  omitted_cols_list,
  kept_indep_cats_list,
  data,
  mesos_var,
  mesos_group,
  ...
)
```

## Arguments

- args:

  Validated list of makeme function arguments

- omitted_cols_list:

  Named list of omitted variables for each crowd

- kept_indep_cats_list:

  Named list of kept independent categories for each crowd

- data:

  Data frame being analyzed

- mesos_var:

  Mesos-level grouping variable

- mesos_group:

  Specific mesos group identifier

- ...:

  Additional arguments passed to process_crowd_data

## Value

Named list of crowd outputs:

- Each element corresponds to one crowd identifier

- Content depends on the specific makeme type requested

- May contain plots, tables, or other analysis objects
