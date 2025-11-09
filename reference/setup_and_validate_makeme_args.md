# Setup and Validate Makeme Arguments

Internal helper function that performs final argument setup and
validation before processing. Consolidates variable resolution,
normalization, and validation.

## Usage

``` r
setup_and_validate_makeme_args(args, data, dep_pos, indep_pos, indep)
```

## Arguments

- args:

  List of makeme function arguments

- data:

  Data frame being analyzed

- dep_pos:

  Named integer vector of dependent variable positions

- indep_pos:

  Named integer vector of independent variable positions

- indep:

  Independent variable selection (for validation)

## Value

Modified and validated args list ready for processing:

- Variable names resolved from positions

- Overlaps between dep and indep resolved

- Multi-choice arguments normalized

- All validation checks passed

- Crowd array reordered for optimal processing
