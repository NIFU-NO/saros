# Validate multiple parameters at once

Validate multiple parameters at once

## Usage

``` r
validate_params(params, spec, call = rlang::caller_env())
```

## Arguments

- params:

  Named list of parameter values to validate

- spec:

  Named list of validation specifications. Each element should have:

  - type: one of "integerish", "double", "bool", "string"

  - min, max: optional for numeric types

  - null_allowed: optional boolean (default FALSE)

- call:

  Calling environment for error messages
