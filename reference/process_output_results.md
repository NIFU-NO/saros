# Process Output Results

Internal helper function that performs final processing of makeme
output, including crowd renaming, NULL removal, and output
simplification.

## Usage

``` r
process_output_results(out, args)
```

## Arguments

- out:

  Named list of crowd outputs from process_all_crowds

- args:

  List of makeme function arguments (for translations and
  simplify_output)

## Value

Processed output in final form:

- Crowds renamed according to translations if provided

- NULL results removed

- Single element extracted if simplify_output=TRUE and length=1

- Empty data.frame returned if no valid results

- Otherwise returns the full named list
