# Method for Creating Saros Contents

Takes the same arguments as `makeme`, except that dep and indep in
make_content are character vectors, for ease of user-customized function
programming.

## Usage

``` r
make_content(type, ...)
```

## Arguments

- type:

  *Method name*

  `scalar<character>` with a class named by itself.

  Optional string indicating the specific method. Occasionally useful
  for error messages, etc.

- ...:

  *Dots*

  Arguments provided by `makeme`

## Value

The returned object class depends on the type. `type="*_table_html"`
always returns a `tibble`. `type="*_plot_html"` always returns a
`ggplot`. `type="*_docx"` always returns a `rdocx` object if
`path=NULL`, or has side-effect of writing docx file to disk if `path`
is set.
