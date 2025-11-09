# Initialize Crowd-Based Filtering Data Structures

Internal helper function that sets up the data structures needed for
crowd-based filtering and processing of variables and categories.

## Usage

``` r
initialize_crowd_filtering(crowd, args)
```

## Arguments

- crowd:

  Character vector of crowd identifiers

- args:

  List of makeme function arguments

## Value

List with three named elements:

- `kept_cols_list`: Named list of kept column information for each crowd

- `omitted_cols_list`: Named list of omitted variables for each crowd

- `kept_indep_cats_list`: Named list of kept independent categories for
  each crowd

## Details

For each crowd, this function calls keep_cols() and keep_indep_cats() to
determine which variables and categories should be retained based on the
various hiding criteria (NA values, sample sizes, etc.).
