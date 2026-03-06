# Attach dep_label_prefix attribute to a make_content output object

Attaches the main question (i.e. the label prefix of the dep variables)
as an attribute `"dep_label_prefix"` on the returned object. Works for
ggplot, data.frame, and mschart objects. Should not be called when
returning an rdocx object.

## Usage

``` r
attach_dep_label_prefix(obj, main_question)
```

## Arguments

- obj:

  The object to annotate (ggplot, data.frame, mschart, etc.)

- main_question:

  Character scalar; the dep label prefix. Must satisfy
  `rlang::is_string(main_question)` and be non-empty. If not, `obj` is
  returned unchanged.

## Value

`obj`, unchanged when `main_question` is not a non-empty string,
otherwise with `"dep_label_prefix"` set in the appropriate location.

## Details

Storage location by class:

- **ggplot / gg**: stored on `obj$data` when `obj$data` is a data.frame,
  so the attribute survives further `+` operations. Falls back to
  `attr(obj, ...)` when `obj$data` is a `waiver()` (empty `ggplot()`).

- **ms_barchart**: stored on `obj$data` (consistent with ggplot).

- **everything else** (data.frame, tibble, …): stored on the object
  itself.
