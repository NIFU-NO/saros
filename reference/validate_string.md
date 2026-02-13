# Validate string parameter

Validate string parameter

## Usage

``` r
validate_string(
  x,
  null_allowed = FALSE,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
)
```

## Arguments

- x:

  Value to validate

- null_allowed:

  Whether NULL is an acceptable value (default FALSE)

- call:

  Calling environment for error messages

- arg:

  Argument name for error messages
