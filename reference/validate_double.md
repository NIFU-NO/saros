# Validate double/numeric parameter

Validate double/numeric parameter

## Usage

``` r
validate_double(
  x,
  min = -Inf,
  max = Inf,
  null_allowed = FALSE,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
)
```

## Arguments

- x:

  Value to validate

- min:

  Minimum allowed value (default -Inf)

- max:

  Maximum allowed value (default Inf)

- null_allowed:

  Whether NULL is an acceptable value (default FALSE)

- call:

  Calling environment for error messages

- arg:

  Argument name for error messages
