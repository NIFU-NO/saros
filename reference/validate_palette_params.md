# Validate Palette Parameters

Validates palette-related parameters used by
[`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) and
[`ggsaver()`](https://nifu-no.github.io/saros/reference/ggsaver.md).
Ensures type safety and provides clear error messages for invalid
inputs.

## Usage

``` r
validate_palette_params(
  palette_codes = NULL,
  priority_palette_codes = NULL,
  label_wrap_width = NULL,
  ncol = NULL,
  byrow = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- palette_codes:

  Optional list of named character vectors

- priority_palette_codes:

  Optional character vector

- label_wrap_width:

  Integer for legend label wrapping

- ncol:

  Optional integer for legend columns

- byrow:

  Logical for legend key arrangement

- call:

  Calling environment for error messages

## Value

NULL (called for side effects - validation)
