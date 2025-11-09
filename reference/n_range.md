# Provides a range (or single value) for N in data, given dep and indep

Provides a range (or single value) for N in data, given dep and indep

## Usage

``` r
n_range(
  data,
  dep,
  indep = NULL,
  mesos_var = NULL,
  mesos_group = NULL,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
)
```

## Arguments

- data:

  Dataset

- dep, indep:

  Tidyselect syntax

- mesos_var:

  Optional, NULL or string specifying name of variable used to split
  dataset.

- mesos_group:

  Optional, NULL or string specifying value in `mesos_var` indicating
  the target group.

- glue_template_1, glue_template_2:

  String, for the case of a single value (1) or a range with
  minimum-maximum of values (2).

## Value

String.

## Examples

``` r
n_range(data = ex_survey, dep = b_1:b_3, indep = x1_sex)
#> 300
```
