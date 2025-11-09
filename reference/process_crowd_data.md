# Process Data for a Single Crowd

Internal helper function that handles the complete processing pipeline
for a single crowd, from data filtering to final output generation.

## Usage

``` r
process_crowd_data(
  crwd,
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

- crwd:

  Character string identifying the current crowd

- args:

  List of makeme function arguments

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

  Additional arguments passed to data summarization functions

## Value

Final output object for the crowd, or NULL if no data remains:

- Plot, table, or other analysis object depending on type

- NULL if crowd has no valid data after filtering

## Details

Complete processing pipeline:

- Calculates omitted variables for the crowd

- Filters data by crowd membership and variable exclusions

- Applies independent category filtering if enabled

- Detects variable types and generates data summary

- Performs validation and post-processing

- Generates final output via make_content()
