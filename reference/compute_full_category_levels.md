# Compute Full Category Levels from Unfiltered Data

Internal helper function that computes the complete set of category
levels from the full unfiltered dataset. This ensures consistent color
assignments across all crowd groups when using mesos_var/mesos_group
filtering.

## Usage

``` r
compute_full_category_levels(data, dep, showNA = "ifany")
```

## Arguments

- data:

  Full unfiltered data frame

- dep:

  Character vector of dependent variable names

- showNA:

  Character indicating whether to include NA as a level

## Value

Character vector of all category levels across all dep variables, or
NULL if not applicable
