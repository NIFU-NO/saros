# Wrapper Function for [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) with Palette Support

Saves ggplot2 objects with automatic palette application from global
settings. Inherits palette settings from
[`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) global
options, ensuring saved plots match the appearance of interactive plots.

## Usage

``` r
ggsaver(
  plot,
  filename,
  palette_codes = NULL,
  priority_palette_codes = NULL,
  label_wrap_width = 80,
  ncol = NULL,
  byrow = TRUE,
  ...
)
```

## Arguments

- plot:

  A ggplot2 object to save.

- filename:

  File path where the plot should be saved.

- palette_codes:

  Optional list of named character vectors with names being categories
  and values being colours. Inherits from
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md)
  global settings if not specified. The final character vector of the
  list is taken as a final resort. Defaults to `NULL`.

- priority_palette_codes:

  Optional named character of categories (as names) with corresponding
  colours (as values) which are used first. Inherits from
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md)
  global settings if not specified. Defaults to `NULL`.

- label_wrap_width:

  Integer. Number of characters fit on the legend labels before
  wrapping. Inherits from
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md)
  global settings. Defaults to `80`.

- ncol:

  Optional integer or NULL for legend columns. Inherits from
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md)
  global settings. Defaults to `NULL`.

- byrow:

  Whether to display legend keys by row or by column. Inherits from
  [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md)
  global settings. Defaults to `TRUE`.

- ...:

  Arguments forwarded to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Value

No return value, called for side effects (saves plot to file)

## Details

This function extends
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
by applying colour palettes before saving, ensuring consistency between
interactive plots (via
[`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md)) and
saved static images. Palette settings are inherited from global settings
set via
[`global_settings_set()`](https://nifu-no.github.io/saros/reference/global_settings_set.md)
for the "girafe" function.

If `palette_codes` is provided (either directly or via global settings),
the function applies the same palette transformation that
[`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) uses
for interactive plots.

## See also

- [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) for
  interactive plots with palette support

- [`global_settings_set()`](https://nifu-no.github.io/saros/reference/global_settings_set.md)
  for setting default palettes

- [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  for the underlying save function

## Examples

``` r
library(ggplot2)
my_plot <- ggplot(data=mtcars, aes(x=hp, y=mpg, fill=factor(cyl))) + geom_point()

if (FALSE) { # \dontrun{
# Save with default settings
ggsaver(my_plot, tempfile(fileext = ".png"))

# Set global palette and save
global_settings_set(
  fn_name = "girafe",
  new = list(palette_codes = list(c("red", "blue", "green")))
)
ggsaver(my_plot, tempfile(fileext = ".png"))

# Override global palette for specific save
ggsaver(
  my_plot,
  tempfile(fileext = ".png"),
  palette_codes = list(c("purple", "orange", "yellow"))
)
} # }
```
