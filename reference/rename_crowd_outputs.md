# Rename Crowd Outputs

Internal helper function that renames crowd identifiers in the output
based on provided translations.

## Usage

``` r
rename_crowd_outputs(out, translations)
```

## Arguments

- out:

  Named list of crowd outputs

- translations:

  Named list of translation mappings for crowd identifiers

## Value

Modified out list with crowd names translated:

- Names changed according to translations with crowd prefix pattern

- Only string translations are applied

- Untranslated crowds retain original names
