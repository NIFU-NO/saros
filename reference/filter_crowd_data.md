# Filter and Prepare Data for a Specific Crowd

Internal helper function that filters data for a specific crowd
identifier, applying variable exclusions and category filtering as
needed.

## Usage

``` r
filter_crowd_data(data, args, crwd, omitted_cols_list, kept_indep_cats_list)
```

## Arguments

- data:

  Data frame being analyzed

- args:

  List of makeme function arguments

- crwd:

  Character string identifying the current crowd

- omitted_cols_list:

  Named list of omitted variables for each crowd

- kept_indep_cats_list:

  Named list of kept independent categories for each crowd

## Value

List with subset data and variables for the crowd, or NULL if no data
remains:

- `subset_data`: Filtered data frame for the crowd

- `dep_crwd`: Character vector of dependent variables for this crowd

- `indep_crwd`: Character vector of independent variables for this crowd

## Details

Applies the following filtering steps:

- Removes omitted variables based on hiding criteria

- Filters rows to match crowd membership

- Applies independent category filtering if enabled

- Returns NULL and warns if no data remains after filtering
