# Identify Suitable Font Given Background Hex Colour

Uses the W3C relative luminance formula (WCAG 2.0) to determine
contrast.

## Usage

``` r
hex_bw(hex_code, na_colour = "#ffffff")
```

## Arguments

- hex_code:

  Colour in hex-format.

## Value

Colours in hex-format, either black or white.
