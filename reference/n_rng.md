# Obtain range of N for a given data set and other settings.

Obtain range of N for a given data set and other settings.

## Usage

``` r
n_rng(
  data,
  dep,
  indep = NULL,
  crowd = "all",
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

  Character vector, names of (in)dependent variables

- crowd:

  String, one of "all", "target" or "others".

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

Always a string.
