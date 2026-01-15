# Wrapper Function for [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

This only exists to make it easy to use it in
[`make_link()`](https://nifu-no.github.io/saros/reference/make_link.md)

## Usage

``` r
ggsaver(plot, filename, ...)
```

## Arguments

- plot:

  Plot

- filename:

  Note

- ...:

  Arguments forwarded to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Value

No return value, called for side effects

## Examples

``` r
library(ggplot2)
my_plot <- ggplot(data=mtcars, aes(x=hp, y=mpg)) + geom_point()
make_link(my_plot, folder=tempdir(), file_suffix = ".png",
          save_fn = ggsaver, width = 16, height = 16, units = "cm")
#> [1] "[download figure data](/tmp/Rtmp4hz4UY/a6989776ed770d3733c72010974ba5b2.png)"
```
