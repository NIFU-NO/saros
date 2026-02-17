# Package index

## The main function you will use

You use this function with the `type`-argument to create most things
you’ll need.

- [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md) :
  Embed Interactive Plot of Various Kinds Using Tidyselect Syntax

## Get, set and reset global options

Global options allow you to easily set defaults for all your makeme
function calls, so you basically do not need to specify the same
arguments throughout. data, dep and indep will still need to be
specified.

- [`global_settings_get()`](https://nifu-no.github.io/saros/reference/global_settings_get.md)
  : Get Global Options for saros-functions
- [`global_settings_set()`](https://nifu-no.github.io/saros/reference/global_settings_set.md)
  : Get Global Options for saros-functions
- [`global_settings_reset()`](https://nifu-no.github.io/saros/reference/global_settings_reset.md)
  : Reset Global Options for saros-functions

## Gets the possible data_label and type options for the makeme()-arguments

- [`get_data_label_opts()`](https://nifu-no.github.io/saros/reference/get_data_label_opts.md)
  : Get Valid Data Labels for Figures and Tables

- [`get_makeme_types()`](https://nifu-no.github.io/saros/reference/get_makeme_types.md)
  :

  Get all registered options for the type-argument in the
  `makeme`-function

## Save output and create a link to it on rendering

- [`make_link()`](https://nifu-no.github.io/saros/reference/make_link.md)
  : Save data to a file and return a Markdown link

- [`make_link(`*`<default>`*`)`](https://nifu-no.github.io/saros/reference/make_link.default.md)
  : Save data to a file and return a Markdown link

- [`make_link(`*`<list>`*`)`](https://nifu-no.github.io/saros/reference/make_link.list.md)
  : Save data to a file and return a Markdown link

- [`make_file_links()`](https://nifu-no.github.io/saros/reference/make_file_links.md)
  : Create Markdown Links to Files with Document Titles

- [`ggsaver()`](https://nifu-no.github.io/saros/reference/ggsaver.md) :

  Wrapper Function for
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  with Palette Support

## Read and write tabular data

Functions for reading and writing data frames to various file formats

- [`tabular_read()`](https://nifu-no.github.io/saros/reference/tabular_read.md)
  : Read tabular data from various formats
- [`tabular_write()`](https://nifu-no.github.io/saros/reference/tabular_write.md)
  : Write tabular data to various formats

## Advanced Palette-Retrieving ggiraph::girafe wrapper

- [`girafe()`](https://nifu-no.github.io/saros/reference/girafe.md) :
  Pull global plotting settings before displaying plot

## Generate figure captions and Quarto tabsets

- [`get_fig_title_suffix_from_ggplot()`](https://nifu-no.github.io/saros/reference/get_fig_title_suffix_from_ggplot.md)
  : Generate Figure Title Suffix with N Range and Optional Download
  Links
- [`crowd_plots_as_tabset()`](https://nifu-no.github.io/saros/reference/crowd_plots_as_tabset.md)
  : Convert List of Plots to Quarto Tabset

## Compute optimal figure heights for the built-in cat_plot

- [`fig_height_h_barchart()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart.md)
  : Estimate figure height for a horizontal bar chart
- [`fig_height_h_barchart2()`](https://nifu-no.github.io/saros/reference/fig_height_h_barchart2.md)
  : Estimate figure height for a horizontal bar chart

## Obtain sample size range for a makeme-object (experimental)

- [`n_range()`](https://nifu-no.github.io/saros/reference/n_range.md) :
  Provides a range (or single value) for N in data, given dep and indep

- [`n_range2()`](https://nifu-no.github.io/saros/reference/n_range2.md)
  :

  Provides a range (or single value) for N in a `ggplot2`-object from
  [`makeme()`](https://nifu-no.github.io/saros/reference/makeme.md)

## Convert cat_plot-object to simple conditional text interpretations

- [`txt_from_cat_mesos_plots()`](https://nifu-no.github.io/saros/reference/txt_from_cat_mesos_plots.md)
  : Extract Text Summary from Categorical Mesos Plots

## Only relevant when expanding the S3 system with your own functions

- [`make_content()`](https://nifu-no.github.io/saros/reference/make_content.md)
  : Method for Creating Saros Contents

## Example dataset

- [`ex_survey`](https://nifu-no.github.io/saros/reference/ex_survey.md)
  : ex_survey: Mockup dataset of a survey.

## Deprecated functions replaced by makeme()

- [`embed_cat_prop_plot()`](https://nifu-no.github.io/saros/reference/embed_cat_prop_plot.md)
  : Embed Interactive Categorical Plot (DEPRECATED!)
- [`embed_cat_table()`](https://nifu-no.github.io/saros/reference/embed_cat_table.md)
  : Embed Reactable Table (DEPRECATED!)
- [`embed_chr_table_html()`](https://nifu-no.github.io/saros/reference/embed_chr_table_html.md)
  : Interactive table of text data (DEPRECATED)
