# Resolve Category Colors for Charts

Centralizes color resolution logic for both HTML and DOCX charts.
Handles checkbox detection, level reordering, and priority palette
setup.

## Usage

``` r
resolve_category_colors(cat_levels, girafe_settings)
```

## Arguments

- cat_levels:

  Character vector of category levels

- girafe_settings:

  List of girafe settings from global_settings_get("girafe")

## Value

List with:

- checkbox: Logical indicating if this is a checkbox plot

- cat_levels: Character vector (reordered if checkbox)

- priority_palette_codes: Named character vector for priority colors

- colour_palette: Named character vector of resolved colors
