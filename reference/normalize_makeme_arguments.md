# Normalize Multi-Choice Arguments to Single Values

Internal helper function that ensures makeme arguments that might be
vectors are normalized to single values by taking the first element.

## Usage

``` r
normalize_makeme_arguments(args)
```

## Arguments

- args:

  List of makeme function arguments

## Value

Modified args list with normalized single-value arguments:

- `showNA`: First element of showNA vector

- `data_label`: First element of data_label vector

- `data_label_position`: First element of data_label_position vector

- `type`: First element of evaluated type expression
