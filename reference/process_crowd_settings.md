# Process Crowd Settings

Internal helper function that reorders the crowd array to ensure
priority crowds (specified in hide_for_all_crowds_if_hidden_for_crowd)
are processed first.

## Usage

``` r
process_crowd_settings(args)
```

## Arguments

- args:

  List of makeme function arguments

## Value

Modified args list with reordered crowd vector:

- Priority crowds (in hide_for_all_crowds_if_hidden_for_crowd) first

- Remaining crowds after

- This ensures global hiding logic is applied correctly
