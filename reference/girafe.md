# Pull global plotting settings before displaying plot

This function extends
[ggiraph::girafe](https://davidgohel.github.io/ggiraph/reference/girafe.html)
by allowing colour palettes to be globally specified.

## Usage

``` r
girafe(
  ggobj,
  ...,
  char_limit = 200,
  label_wrap_width = 80,
  interactive = TRUE,
  palette_codes = NULL,
  priority_palette_codes = NULL,
  ncol = NULL,
  byrow = TRUE,
  colour_2nd_binary_cat = NULL,
  checked = NULL,
  not_checked = NULL,
  width_svg = NULL,
  height_svg = NULL,
  pointsize = 12
)
```

## Arguments

- ggobj:

  ggplot2-object.

- ...:

  Dots forwarded to
  [`ggiraph::girafe()`](https://davidgohel.github.io/ggiraph/reference/girafe.html)

- char_limit:

  Integer. Number of characters to fit on a line of plot (legend-space).
  Will be replaced in the future with a function that guesses this.

- label_wrap_width:

  Integer. Number of characters fit on the axis text space before
  wrapping.

- interactive:

  Boolean. Whether to produce a ggiraph-plot with interactivity
  (defaults to TRUE) or a static ggplot2-plot.

- palette_codes:

  Optional list of named character vectors with names being categories
  and values being colours. The final character vector of the list is
  taken as a final resort. Defaults to `NULL`.

- priority_palette_codes:

  Optional named character of categories (as names) with corresponding
  colours (as values) which are used first, whereupon the remaining
  unspecified categories are pulled from the last vector of
  `palette_codes`. Defaults to `NULL`.

- ncol:

  Optional integer or NULL.

- byrow:

  Whether to display legend keys by row or by column.

- colour_2nd_binary_cat:

  Optional string. Color for the second category in binary checkbox
  plots. When set together with `checked` and `not_checked`, reverses
  the category order so that `not_checked` appears second and receives
  this color. Ignored if checkbox criteria are not met.

- checked, not_checked:

  Optional string. If specified and the fill categories of the plot
  matches these, a special plot is returned where not_checked is hidden.
  Its usefulness comes in plots which are intended for checkbox
  responses where unchecked is not always a conscious choice.

- pointsize, height_svg, width_svg:

  See
  [`ggiraph::girafe()`](https://davidgohel.github.io/ggiraph/reference/girafe.html).

## Value

If interactive, only side-effect of generating ggiraph-plot. If
interactive=FALSE, returns modified ggobj.

## Examples

``` r
plot <- makeme(data = ex_survey, dep = b_1)
girafe(plot)

{"x":{"html":"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='ggiraph-svg' role='graphics-document' id='svg_fe174e4c81bd36aa' viewBox='0 0 432 360'>\n <defs id='svg_fe174e4c81bd36aa_defs'>\n  <clipPath id='svg_fe174e4c81bd36aa_c1'>\n   <rect x='0' y='0' width='432' height='360'/>\n  <\/clipPath>\n  <clipPath id='svg_fe174e4c81bd36aa_c2'>\n   <rect x='20.35' y='5.48' width='0' height='322.82'/>\n  <\/clipPath>\n <\/defs>\n <g id='svg_fe174e4c81bd36aa_rootg' class='ggiraph-svg-rootg'>\n  <g clip-path='url(#svg_fe174e4c81bd36aa_c1)'>\n   <rect x='0' y='0' width='432' height='360' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.75' stroke-linejoin='round' stroke-linecap='round' class='ggiraph-svg-bg'/>\n   <rect x='-3.11' y='0' width='438.21' height='360' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='round'/>\n  <\/g>\n  <g clip-path='url(#svg_fe174e4c81bd36aa_c2)'>\n   <rect x='20.35' y='5.48' width='0' height='322.82' fill='#EBEBEB' fill-opacity='1' stroke='none'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,166.89 20.35,166.89' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,328.29 20.35,5.48' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <rect id='svg_fe174e4c81bd36aa_e1' x='20.35' y='45.83' width='0' height='242.11' fill='#F8766D' fill-opacity='1' stroke='none' title='Category: &amp;lt;b&amp;gt;Not at all&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;Dependent: &amp;lt;b&amp;gt;&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;Percentage: &amp;lt;b&amp;gt;43&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;n (cell): &amp;lt;b&amp;gt;129&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;N (per independent var; valid): &amp;lt;b&amp;gt;300&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;N (total; valid): &amp;lt;b&amp;gt;300&amp;lt;/b&amp;gt;' data-id='1' onclick='alert(\"Category: Not at all\\n Dependent: \\n Percentage: 43\\n n (cell): 129\\n N (per independent var; valid): 300\\n N (total; valid): 300\\nDependent variable name: b_1\\nIndependent variable name: \");'/>\n   <rect id='svg_fe174e4c81bd36aa_e2' x='20.35' y='45.83' width='0' height='242.11' fill='#00BA38' fill-opacity='1' stroke='none' title='Category: &amp;lt;b&amp;gt;A bit&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;Dependent: &amp;lt;b&amp;gt;&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;Percentage: &amp;lt;b&amp;gt;48&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;n (cell): &amp;lt;b&amp;gt;143&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;N (per independent var; valid): &amp;lt;b&amp;gt;300&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;N (total; valid): &amp;lt;b&amp;gt;300&amp;lt;/b&amp;gt;' data-id='2' onclick='alert(\"Category: A bit\\n Dependent: \\n Percentage: 48\\n n (cell): 143\\n N (per independent var; valid): 300\\n N (total; valid): 300\\nDependent variable name: b_1\\nIndependent variable name: \");'/>\n   <rect id='svg_fe174e4c81bd36aa_e3' x='20.35' y='45.83' width='0' height='242.11' fill='#619CFF' fill-opacity='1' stroke='none' title='Category: &amp;lt;b&amp;gt;A lot&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;Dependent: &amp;lt;b&amp;gt;&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;Percentage: &amp;lt;b&amp;gt;9&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;n (cell): &amp;lt;b&amp;gt;28&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;N (per independent var; valid): &amp;lt;b&amp;gt;300&amp;lt;/b&amp;gt;&amp;lt;br/&amp;gt;N (total; valid): &amp;lt;b&amp;gt;300&amp;lt;/b&amp;gt;' data-id='3' onclick='alert(\"Category: A lot\\n Dependent: \\n Percentage: 9\\n n (cell): 28\\n N (per independent var; valid): 300\\n N (total; valid): 300\\nDependent variable name: b_1\\nIndependent variable name: \");'/>\n   <text id='svg_fe174e4c81bd36aa_e4' x='20.35' y='170.9' font-size='8.25pt' font-family='DejaVu Sans' data-id='1' onclick='alert(\"Category: Not at all\\n Dependent: \\n Percentage: 43\\n n (cell): 129\\n N (per independent var; valid): 300\\n N (total; valid): 300\\nDependent variable name: b_1\\nIndependent variable name: \");'>43<\/text>\n   <text id='svg_fe174e4c81bd36aa_e5' x='20.35' y='170.9' font-size='8.25pt' font-family='DejaVu Sans' data-id='2' onclick='alert(\"Category: A bit\\n Dependent: \\n Percentage: 48\\n n (cell): 143\\n N (per independent var; valid): 300\\n N (total; valid): 300\\nDependent variable name: b_1\\nIndependent variable name: \");'>48<\/text>\n   <text id='svg_fe174e4c81bd36aa_e6' x='20.35' y='170.9' font-size='8.25pt' font-family='DejaVu Sans' data-id='3' onclick='alert(\"Category: A lot\\n Dependent: \\n Percentage: 9\\n n (cell): 28\\n N (per independent var; valid): 300\\n N (total; valid): 300\\nDependent variable name: b_1\\nIndependent variable name: \");'>9<\/text>\n  <\/g>\n  <g clip-path='url(#svg_fe174e4c81bd36aa_c1)'>\n   <polyline points='17.61,166.89 20.35,166.89' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,331.03 20.35,328.29' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,331.03 20.35,328.29' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,331.03 20.35,328.29' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,331.03 20.35,328.29' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='20.35,331.03 20.35,328.29' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <text x='13.38' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>0%<\/text>\n   <text x='10.58' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>25%<\/text>\n   <text x='10.58' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>50%<\/text>\n   <text x='10.58' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>75%<\/text>\n   <text x='7.78' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>100%<\/text>\n   <text x='-10.05' y='352.23' font-size='8.25pt' font-family='DejaVu Sans'>.proportion<\/text>\n   <text transform='translate(10.39,206.88) rotate(-90.00)' font-size='8.25pt' font-family='DejaVu Sans'>.variable_label<\/text>\n   <rect x='31.31' y='144.87' width='398.32' height='44.03' fill='#FFFFFF' fill-opacity='1' stroke='none'/>\n   <rect x='36.79' y='166.14' width='17.28' height='17.28' fill='#EBEBEB' fill-opacity='1' stroke='none'/>\n   <rect x='37.5' y='166.85' width='15.86' height='15.86' fill='#F8766D' fill-opacity='1' stroke='none'/>\n   <rect x='105.16' y='166.14' width='17.28' height='17.28' fill='#EBEBEB' fill-opacity='1' stroke='none'/>\n   <rect x='105.87' y='166.85' width='15.86' height='15.86' fill='#00BA38' fill-opacity='1' stroke='none'/>\n   <rect x='153.7' y='166.14' width='17.28' height='17.28' fill='#EBEBEB' fill-opacity='1' stroke='none'/>\n   <rect x='154.41' y='166.85' width='15.86' height='15.86' fill='#619CFF' fill-opacity='1' stroke='none'/>\n   <text x='59.55' y='177.99' font-size='6.6pt' font-family='DejaVu Sans'>Not at all<\/text>\n   <text x='127.92' y='177.99' font-size='6.6pt' font-family='DejaVu Sans'>A bit<\/text>\n   <text x='176.46' y='177.99' font-size='6.6pt' font-family='DejaVu Sans'>A lot<\/text>\n  <\/g>\n <\/g>\n<\/svg>","js":null,"uid":"svg_fe174e4c81bd36aa","ratio":1.2,"settings":{"tooltip":{"css":".tooltip_SVGID_ { padding:5px;background:black;color:white;border-radius:2px;text-align:left; ; position:absolute;pointer-events:none;z-index:999;}","placement":"doc","opacity":0.9,"offx":10,"offy":10,"use_cursor_pos":true,"use_fill":false,"use_stroke":false,"delay_over":200,"delay_out":500},"hover":{"css":".hover_data_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_data_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_data_SVGID_ { fill:orange;stroke:black; }\nline.hover_data_SVGID_, polyline.hover_data_SVGID_ { fill:none;stroke:orange; }\nrect.hover_data_SVGID_, polygon.hover_data_SVGID_, path.hover_data_SVGID_ { fill:orange;stroke:none; }\nimage.hover_data_SVGID_ { stroke:orange; }","reactive":true,"nearest_distance":null},"hover_inv":{"css":""},"hover_key":{"css":".hover_key_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_key_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_key_SVGID_ { fill:orange;stroke:black; }\nline.hover_key_SVGID_, polyline.hover_key_SVGID_ { fill:none;stroke:orange; }\nrect.hover_key_SVGID_, polygon.hover_key_SVGID_, path.hover_key_SVGID_ { fill:orange;stroke:none; }\nimage.hover_key_SVGID_ { stroke:orange; }","reactive":true},"hover_theme":{"css":".hover_theme_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_theme_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_theme_SVGID_ { fill:orange;stroke:black; }\nline.hover_theme_SVGID_, polyline.hover_theme_SVGID_ { fill:none;stroke:orange; }\nrect.hover_theme_SVGID_, polygon.hover_theme_SVGID_, path.hover_theme_SVGID_ { fill:orange;stroke:none; }\nimage.hover_theme_SVGID_ { stroke:orange; }","reactive":true},"select":{"css":".select_data_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_data_SVGID_ { stroke:none;fill:red; }\ncircle.select_data_SVGID_ { fill:red;stroke:black; }\nline.select_data_SVGID_, polyline.select_data_SVGID_ { fill:none;stroke:red; }\nrect.select_data_SVGID_, polygon.select_data_SVGID_, path.select_data_SVGID_ { fill:red;stroke:none; }\nimage.select_data_SVGID_ { stroke:red; }","type":"multiple","only_shiny":true,"selected":[]},"select_inv":{"css":""},"select_key":{"css":".select_key_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_key_SVGID_ { stroke:none;fill:red; }\ncircle.select_key_SVGID_ { fill:red;stroke:black; }\nline.select_key_SVGID_, polyline.select_key_SVGID_ { fill:none;stroke:red; }\nrect.select_key_SVGID_, polygon.select_key_SVGID_, path.select_key_SVGID_ { fill:red;stroke:none; }\nimage.select_key_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"select_theme":{"css":".select_theme_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_theme_SVGID_ { stroke:none;fill:red; }\ncircle.select_theme_SVGID_ { fill:red;stroke:black; }\nline.select_theme_SVGID_, polyline.select_theme_SVGID_ { fill:none;stroke:red; }\nrect.select_theme_SVGID_, polygon.select_theme_SVGID_, path.select_theme_SVGID_ { fill:red;stroke:none; }\nimage.select_theme_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"zoom":{"min":1,"max":1,"duration":300,"default_on":false},"toolbar":{"position":"topright","pngname":"diagram","tooltips":null,"fixed":false,"hidden":[],"delay_over":200,"delay_out":500},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}
```
