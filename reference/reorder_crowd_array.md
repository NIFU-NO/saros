# Reorder Crowd Array Based on Hide Settings

Internal helper function that reorders the crowd array to prioritize
crowds specified in hide_for_all_crowds_if_hidden_for_crowd, ensuring
they are processed first to determine variable exclusions early.

## Usage

``` r
reorder_crowd_array(crowd, hide_for_all_crowds_if_hidden_for_crowd)
```

## Arguments

- crowd:

  Character vector of crowd identifiers

- hide_for_all_crowds_if_hidden_for_crowd:

  Character vector of crowd identifiers that should be processed first
  to determine global exclusions

## Value

Character vector with reordered crowd identifiers:

- Priority crowds first (those in
  hide_for_all_crowds_if_hidden_for_crowd)

- Remaining crowds after
